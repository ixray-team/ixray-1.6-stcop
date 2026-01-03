#include "stdafx.h"

#include "xrLight_Implicit.h"
#include "xrLight_ImplicitDeflector.h"

#include "light_point.h"
#include "xrDeflector.h"
#include "xrLC_GlobalData.h"
#include "xrFace.h"
#include "xrLight_ImplicitCalcGlobs.h"

#include "../../xrCore/Collision/xrCDB.h"

using Implicit = xr_map<u32, ImplicitDeflector>;
using Implicit_it = Implicit::iterator;

#include "../xrForms/xrThread.h"
#include "../xrForms/CompilersUI.h"
#include "../xrDXT/xrDXT.h"

#ifdef LCCUDA_BUILD
#	include "CUDA/CUDARayCast.h"
#endif

class ImplicitThread : public CThread
{
public:

	ImplicitExecute	execute;
	ImplicitThread(u32 ID, ImplicitDeflector* _DATA) : CThread(ID), execute()
	{

	}
	virtual void Execute();
};

void ImplicitThread::Execute()
{
	// Priority
	SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_BELOW_NORMAL);
	Sleep(0);
	execute.Execute();
}

// 2 : Mainthread + UI thread
int ThreadTaskID_Implication = 0;
 
xrCriticalSection csLockImplicit;
ImplicitCalcGlobs cl_globs;

void RunImplicitMultithread(ImplicitDeflector& defl)
{
	// Start threads
	ThreadTaskID_Implication = 0;

  	CThreadManager tmanager;
	for (u32 thID = 0; thID < gCompilerMode.ThreadsPerWork; thID++)
 		tmanager.start(new ImplicitThread(thID, &defl));
 	tmanager.wait();
}

void ImplicitExecute::Execute()
{
	ImplicitDeflector& defl			= cl_globs.DATA();
	CDB::COLLIDER DB;

	// Setup variables
	Fvector2 dim, half;
	dim .set(float(defl.Width()), float(defl.Height()));
	half.set(.5f / dim.x, .5f / dim.y);

	// Jitter data
	Fvector2 JS;
	JS.set(.499f / dim.x, .499f / dim.y);
	u32 Jcount;
	Fvector2* Jitter;
	Jitter_Select(Jitter, Jcount);

	// Lighting itself
	DB.ray_options(0);

	while (true)
	{
		csLockImplicit.Enter();
		int V = ThreadTaskID_Implication;
		if (ThreadTaskID_Implication >= defl.Height())
		{
			csLockImplicit.Leave();
			break;
		}
		ThreadTaskID_Implication++;

		Progress(float(V) / float(defl.Height()));
		csLockImplicit.Leave();
 
		AditionalData("CurrentV: %u", V);
 
		for (u32 U = 0; U < defl.Width(); U++)
		{
			base_color_c C;
			u32 Fcount = 0;
 			try
			{
				for (u32 J = 0; J < Jcount; J++)
				{
					// LUMEL space
					Fvector2				P;
					P.x = float(U) / dim.x + half.x + Jitter[J].x * JS.x;
					P.y = float(V) / dim.y + half.y + Jitter[J].y * JS.y;
					xr_vector<Face*>& space = cl_globs.Hash().query(P.x, P.y);

					// World space
					Fvector wP, wN, B;
					for (vecFaceIt it = space.begin(); it != space.end(); it++)
					{
						Face* F = *it;
						_TCF& tc = F->tc[0];
						if (tc.isInside(P, B))
						{
							// We found triangle and have barycentric coords
							Vertex* V1 = F->v[0];
							Vertex* V2 = F->v[1];
							Vertex* V3 = F->v[2];
							wP.from_bary(V1->P, V2->P, V3->P, B);
							wN.from_bary(V1->N, V2->N, V3->N, B);
							wN.normalize();

							u32 flags = (gCompilerMode.LC_NoSun ? LP_dont_sun : 0);
							LightPoint(&DB, inlc_global_data()->RCAST_Model(), C, wP, wN, inlc_global_data()->L_static(), flags, F);
							Fcount++;
						}
					}
				}
			}
			catch (...)
			{
				clMsg("* THREAD #%d: Access violation. Possibly recovered.");//,thID
			}

			if (Fcount)
			{
				// Calculate lighting amount
				C.scale(Fcount);
				C.mul(.5f);
				defl.Lumel(U, V)._set(C);
				defl.Marker(U, V) = 255;
			}
			else
			{
				defl.Marker(U, V) = 0;
			}
		}
	}
}

#ifdef LCCUDA_BUILD

class CImplicitDeflector
{
	struct ColorsReady
	{
		base_color_c C;
		u8			 Samples = 0;
	};

	xr_concurrent_unordered_map <size_t, ColorsReady>		ColorsImplicitGPU;

public:
	ImplicitDeflector* defl = nullptr;
 
	CImplicitDeflector()
	{
 	}

	void ApplyColor(size_t IndexTask, base_color_c& C)
	{
		ColorsImplicitGPU[IndexTask].C.add(C);
		ColorsImplicitGPU[IndexTask].Samples++;
	}

	void ApplyColors()
	{
 		for (auto& T : ColorsImplicitGPU)
		{
			auto KEY = T.first;
 			u8	Samples = T.second.Samples;

			u32 U = GPUTaskinSystem.GetU(KEY);
			u32 V = GPUTaskinSystem.GetV(KEY);

 			if (Samples)
			{
				// Color
				auto& C = T.second.C;
 
				// Calculate lighting amount
				C.scale(Samples);
				C.mul(.5f);
				defl->Lumel(U, V)._set(C);
				defl->Marker(U, V) = 255;
			}
			else
			{
				defl->Marker(U, V) = 0;
			}
		}
		ColorsImplicitGPU.clear();

	}

	void RunTaskGPU()
	{
		clMsg("$ Run Tasks GPU");
		defl = & cl_globs.DATA();

		CTimer tStats;
		tStats.Start();
 
		// Setup variables
		Fvector2 dim, half;
		dim.set(float(defl->Width()), float(defl->Height()));
		half.set(.5f / dim.x, .5f / dim.y);

		// Jitter data
		Fvector2 JS;
		JS.set(.499f / dim.x, .499f / dim.y);
		u32 Jcount;
		Fvector2* Jitter;
		Jitter_Select(Jitter, Jcount);

		GPUTaskinSystem.RestartALL();

		u32 flags = (gCompilerMode.LC_NoSun ? LP_dont_sun : 0);
		GPUTaskinSystem.current_flags = flags;
		GPUTaskinSystem.ColorsMapType = eImplicit;

		// Однопоточный режим пока что 
		xr_atomic_u32 task_height  = 0;
		xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [&](size_t TaskID)
		{
  			while(true)
			{
				auto V = task_height.fetch_add(1);
				if (V >= defl->Height()) break;
 
				for (u32 U = 0; U < defl->Width(); U++)
				{
 					try
					{
						for (u32 SampleID = 0; SampleID < Jcount; SampleID++)
						{
							// LUMEL space
							Fvector2				P;
							P.x = float(U) / dim.x + half.x + Jitter[SampleID].x * JS.x;
							P.y = float(V) / dim.y + half.y + Jitter[SampleID].y * JS.y;
							xr_vector<Face*>& space = cl_globs.Hash().query(P.x, P.y);

							// World space
							Fvector wP, wN, B;
							for (vecFaceIt it = space.begin(); it != space.end(); it++)
							{
								Face* F = *it;
								_TCF& tc = F->tc[0];
								if (tc.isInside(P, B))
								{
									// We found triangle and have barycentric coords
									Vertex* V1 = F->v[0];
									Vertex* V2 = F->v[1];
									Vertex* V3 = F->v[2];
									wP.from_bary(V1->P, V2->P, V3->P, B);
									wN.from_bary(V1->N, V2->N, V3->N, B);
									wN.normalize();

									GPUTaskinSystem.LightPointPacked_add_task(GPUTaskinSystem.MakeKey(U, V), nullptr, wP, wN, F);
								}
							}
						}
					}
					catch (...)
					{
						clMsg("* THREAD #%d: Access violation. Possibly recovered.");//,thID
					}
				}

				AditionalData("Current: %u", V);
			};

			// Остаток доработать 
			GPUTaskinSystem.LightPointPacked_run_tasks();
		});
		
		ApplyColors();

		GPUTaskinSystem.RestartALL();
	}
};

CImplicitDeflector GPU_DeflectorIMPL;

void ApplyColorGPU(size_t IndexTask, base_color_c& C)
{
	GPU_DeflectorIMPL.ApplyColor(IndexTask, C);
}
#endif

static xr_vector<u32> not_clear;
void ImplicitLightingExec()
{
	Implicit		calculator;

	cl_globs.Allocate();
	not_clear.clear();
	// Sorting
	Status("Sorting faces...");
	for (vecFaceIt I = inlc_global_data()->g_faces().begin(); I != inlc_global_data()->g_faces().end(); I++)
	{
		Face* F = *I;
		if (F->pDeflector)				continue;
		if (!F->hasImplicitLighting())	continue;

		Progress(float(I - inlc_global_data()->g_faces().begin()) / float(inlc_global_data()->g_faces().size()));
		b_material& M = inlc_global_data()->materials()[F->dwMaterial];
		u32				Tid = M.surfidx;
		b_BuildTexture* T = &(inlc_global_data()->textures()[Tid]);

		Implicit_it		it = calculator.find(Tid);
		if (it == calculator.end())
		{
			ImplicitDeflector	ImpD;
			ImpD.texture = T;
			ImpD.faces.push_back(F);
			calculator.insert(std::make_pair(Tid, ImpD));
			not_clear.push_back(Tid);
		}
		else {
			ImplicitDeflector& ImpD = it->second;
			ImpD.faces.push_back(F);
		}
	}

	// Lighing
	for (Implicit_it imp = calculator.begin(); imp != calculator.end(); imp++)
	{
		ImplicitDeflector& defl = imp->second;
		Status("Lighting implicit map '%s'...", defl.texture->name);
		Progress(0);
		defl.Allocate();

		// Setup cache
		Progress(0);
		cl_globs.Initialize(defl);

#ifdef LCCUDA_BUILD
		if (gCompilerMode.CUDA)
		{
			GPU_DeflectorIMPL.RunTaskGPU();
		}
		else
#endif
			RunImplicitMultithread(defl);

		defl.faces.clear();

		// Expand
		Status("Processing lightmap...");
		for (u32 ref = 254; ref > 0; ref--)	if (!ApplyBorders(defl.lmap, ref)) break;

		Status("Mixing lighting with texture...");
		{
			b_BuildTexture& TEX = *defl.texture;
			VERIFY(!TEX.pSurface.Empty());
			u32* color = (u32*)*TEX.pSurface;
			for (u32 V = 0; V < defl.Height(); V++) {
				for (u32 U = 0; U < defl.Width(); U++) {
					// Retreive Texel
					float	h = defl.Lumel(U, V).h._r();
					u32& C = color[V * defl.Width() + U];
					C = subst_alpha(C, u8_clr(h));
				}
			}
		}

		xr_vector<u32> packed;
		defl.lmap.Pack(packed);
		defl.Deallocate();

		// base
		Status("Saving base...");
		{
			string128 name;
			string_path out_name;
			xr_strcpy(name, lc_global_data()->GetLavelName());

			R_ASSERT(name[0] && defl.texture);

			b_BuildTexture& TEX = *defl.texture;
			xr_strconcat(out_name, name, "\\", TEX.name, ".dds");
			FS.update_path(out_name, "$game_levels$", out_name);
			clMsg("Saving texture '%s'...", out_name);
			VerifyPath(out_name);
			BYTE* raw_data = LPBYTE(*TEX.pSurface);
			u32	w = TEX.dwWidth;
			u32	h = TEX.dwHeight;
			u32	pitch = w * 4;
			STextureParams fmt = TEX.THM;

			switch (gCompilerMode.LmapsFormat)
			{
				case LCLightmapFormat::FORMAT_RGBA: fmt.fmt = STextureParams::tfRGBA; break;
				case LCLightmapFormat::FORMAT_BC7:  fmt.fmt = STextureParams::tfBC7; break;
				case LCLightmapFormat::FORMAT_BC5:  fmt.fmt = STextureParams::tfDXT5; break;
			}

			fmt.flags.set(STextureParams::flDitherColor, FALSE);
			fmt.flags.set(STextureParams::flGenerateMipMaps, FALSE);
			fmt.flags.set(STextureParams::flBinaryAlpha, FALSE);
			DXTUtils::Compress(out_name, raw_data, 0, w, h, pitch, &fmt, 4);
		}

		// lmap
		Status("Saving lmap...");
		{
			string128 name;
			string_path out_name;
			xr_strcpy(name, lc_global_data()->GetLavelName());

			b_BuildTexture& TEX = *defl.texture;
			xr_strconcat(out_name, name, "\\", TEX.name, "_lm.dds");
			FS.update_path(out_name, "$game_levels$", out_name);
			clMsg("Saving texture '%s'...", out_name);
			VerifyPath(out_name);
			BYTE* raw_data = LPBYTE(&*packed.begin());
			u32	w = TEX.dwWidth;
			u32	h = TEX.dwHeight;
			u32	pitch = w * 4;
			STextureParams			fmt;

			switch (gCompilerMode.LmapsFormat)
			{
				case LCLightmapFormat::FORMAT_RGBA: fmt.fmt = STextureParams::tfRGBA; break;
				case LCLightmapFormat::FORMAT_BC7:  fmt.fmt = STextureParams::tfBC7; break;
				case LCLightmapFormat::FORMAT_BC5:  fmt.fmt = STextureParams::tfDXT5; break;
			}

			fmt.flags.set(STextureParams::flDitherColor, FALSE);
			fmt.flags.set(STextureParams::flGenerateMipMaps, FALSE);
			fmt.flags.set(STextureParams::flBinaryAlpha, FALSE);
			DXTUtils::Compress(out_name, raw_data, 0, w, h, pitch, &fmt, 4);
		}
		//defl.Deallocate				();
	}
	not_clear.clear();
	cl_globs.Deallocate();
	calculator.clear();
}