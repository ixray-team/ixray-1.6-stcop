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
CTimer tImplicit;

xrCriticalSection csLockImplicit;
ImplicitCalcGlobs cl_globs;

void RunImplicitMultithread(ImplicitDeflector& defl)
{
	// Start threads
	ThreadTaskID_Implication = 0;

	tImplicit.Start();

	CThreadManager tmanager;
	for (u32 thID = 0; thID < gCompilerMode.ThreadsPerWork; thID++)
	{
		tmanager.start(new ImplicitThread(thID, &defl));
	}

	tmanager.wait();
}

void ImplicitExecute::Execute()
{
	ImplicitDeflector& defl = cl_globs.DATA();
	CDB::COLLIDER DB;

	// Setup variables
	Fvector2 dim, half;
	dim.set(float(defl.Width()), float(defl.Height()));
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

							u32 flags = (inlc_global_data()->b_nosun() ? LP_dont_sun : 0);
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

		if (V % 8 == 0)
			AditionalData("CurrentV: %u | time: %.0f", V, tImplicit.GetElapsed_sec());
	}
}

#ifdef LCCUDA_BUILD
void RunTaskGPU()
{
	CTimer tStats;
	tStats.Start();
  
 	ImplicitDeflector& defl = cl_globs.DATA();
	// Setup variables
	Fvector2 dim, half;
	dim.set(float(defl.Width()), float(defl.Height()));
	half.set(.5f / dim.x, .5f / dim.y);

	// Jitter data
	Fvector2 JS;
	JS.set(.499f / dim.x, .499f / dim.y);
	u32 Jcount;
	Fvector2* Jitter;
	Jitter_Select(Jitter, Jcount);


	GPUTaskinSystem.RestartALL();


	u32 flags = (inlc_global_data()->b_nosun() ? LP_dont_sun : 0);
	GPUTaskinSystem.current_flags = flags;

	//
	xr_map<size_t, u32> FacesCount;  
 	for (u32 V = 0; V < defl.Height(); V++)
	{
		for (u32 U = 0; U < defl.Width(); U++)
		{
			base_color_c C;
			u32 Fcount = 0;
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

							GPUTaskinSystem.LightPointPacked(U, V, wP, wN, flags, F);
							Fcount++;
						}
					}
				}
			}
			catch (...)
			{
				clMsg("* THREAD #%d: Access violation. Possibly recovered.");//,thID
			}

			FacesCount[GPUTaskinSystem.MakeKey(U, V)] = Fcount;
		}
		AditionalData("Current: %u", V);
	};

	// Остаток доработать 
	GPUTaskinSystem.LightPointPackedRun();

	CTimer tColors; tColors.Start();
  	for (auto& T : GPUTaskinSystem.Colors)
	{
		auto KEY = T.first;
		u32 U = GPUTaskinSystem.GetU(KEY);
		u32 V = GPUTaskinSystem.GetV(KEY);

		u32 Fcount = FacesCount[KEY];
		if (Fcount)
		{
			auto& C = T.second;
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
 
	clMsg("@ CPU Code: %llu | CPU CopyToGPU : %u | GPU(%u) | CPU copy result(%u) | Clear(%u)",
		GPUTaskinSystem.StatsRaysAdd / 1000,
		GPUTaskinSystem.StatsCopyRaysGPU / 1000,
		GPUTaskinSystem.StatsTraverseGPU / 1000,
		GPUTaskinSystem.StatsCopyResultGPU / 1000,
		GPUTaskinSystem.StatsClearingListGPU / 1000
	);

	
	GPUTaskinSystem.RestartALL();
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
			RunTaskGPU();
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
			VERIFY(TEX.pSurface);
			u32* color = TEX.pSurface;
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
			BYTE* raw_data = LPBYTE(TEX.pSurface);
			u32	w = TEX.dwWidth;
			u32	h = TEX.dwHeight;
			u32	pitch = w * 4;
			STextureParams fmt = TEX.THM;
			fmt.fmt = lc_global_data()->GetLmapRGBA() ? STextureParams::tfRGBA : STextureParams::tfDXT5;
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
			fmt.fmt = lc_global_data()->GetLmapRGBA() ? STextureParams::tfRGBA : STextureParams::tfDXT5;
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