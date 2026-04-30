#include "stdafx.h"
#include "xrLight_ImplicitDeflector.h"

#include "light_point.h"
#include "xrDeflector.h"
#include "xrLC_GlobalData.h"
#include "xrFace.h"

#include "../../xrCore/Collision/xrCDB.h"
#include "../xrForms/CompilersUI.h"

// 2 : Mainthread + UI thread
ImplicitCalcGlobs cl_globs;
xr_atomic_u32 ThreadTaskID_Implication = 0;

void RunImplicitMultithread(ImplicitDeflector& defl)
{
	// Start threads
	ThreadTaskID_Implication = 0;

 	xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [](size_t taskID)
	{
		ImplicitDeflector& defl = cl_globs.DATA();
 		// Setup variables		
		Fvector2 dim;   dim.set(float(defl.Width()), float(defl.Height()));
		Fvector2 half;  half.set(.5f / dim.x, .5f / dim.y);

		// Jitter data
		Fvector2 JS; JS.set(.499f / dim.x, .499f / dim.y);
		
		// Thread Local Update
 		Fvector2* Jitter; u32 Jcount;
		CDB::COLLIDER DB;  DB.ray_options(0);
		Jitter_Select(Jitter, Jcount);

		while (true)
		{
			u32 tID = ThreadTaskID_Implication.fetch_add(1);
			if (ThreadTaskID_Implication >= defl.Height()) 	break;
			Progress(float(tID) / float(defl.Height()));

			AditionalData("Implict Deflector: %u/%u", tID, defl.Height());

			for (u32 U = 0; U < defl.Width(); U++)
			{
				base_color_c C;
				u32 Fcount = 0;
				for (u32 J = 0; J < Jcount; J++)
				{
					// LUMEL space
					Fvector2				P;
					P.x = float(U) / dim.x + half.x + Jitter[J].x * JS.x;
					P.y = float(tID) / dim.y + half.y + Jitter[J].y * JS.y;
 
					// World space
					Fvector wP, wN, B;
					for (auto F : cl_globs.query(P.x, P.y))
					{
 						_TCF& tc = F->tc[0];
						if (tc.isInside(P, B))
						{
							// We found triangle and have barycentric coords
							GetBarycentric(F, wP, wN, B);

							 
							LightPoint(&DB, inlc_global_data()->RCAST_Model(), C, wP, wN, inlc_global_data()->L_static(), LGetCurrentFlags(), F);
							Fcount++;
						}
					}
				}

				if (Fcount)
				{
					// Calculate lighting amount
					C.scale(Fcount);
					C.mul(.5f);
					defl.Lumel(U, tID)._set(C);
					defl.Marker(U, tID) = 255;
				}
				else
				{
					defl.Marker(U, tID) = 0;
				}
			}
		}
	});
}

extern void RunImplicitGPU(); 

void ImplicitLightingExec()
{
	UpdateCurrentPhase("Implicit");
 
	// Sorting
	Status("Sorting faces...");
	xr_map<u32, ImplicitDeflector>	calculator;
	for (vecFaceIt I = inlc_global_data()->g_faces().begin(); I != inlc_global_data()->g_faces().end(); I++)
	{
		Face* F = *I;
		if (F->pDeflector)				continue;
		if (!F->hasImplicitLighting())	continue;

		Progress(float(I - inlc_global_data()->g_faces().begin()) / float(inlc_global_data()->g_faces().size()));
		b_material& M = inlc_global_data()->materials()[F->dwMaterial];
		u32				Tid = M.surfidx;
		b_BuildTexture* T = &(inlc_global_data()->textures()[Tid]);

		auto		it = calculator.find(Tid);
		if (it == calculator.end())
		{
			ImplicitDeflector	ImpD;
			ImpD.texture = T;
			ImpD.faces.push_back(F);
			calculator.insert(std::make_pair(Tid, ImpD));
 		}
		else {
			ImplicitDeflector& ImpD = it->second;
			ImpD.faces.push_back(F);
		}
	}

	// Lighing
	for (auto& imp : calculator)
	{
		ImplicitDeflector& defl = imp.second;
		Status("Lighting implicit map '%s'...", defl.texture->name);

		// Setup cache
 		cl_globs.Initialize(defl);
#ifdef LCCUDA_BUILD
		if (gCompilerMode.CUDA)
			RunImplicitGPU();
		else
#endif
			RunImplicitMultithread(defl);

 		defl.faces.clear();
		defl.faces.shrink_to_fit();

		// Saving DDS
		defl.SaveTextures();
 	}
 	calculator.clear();
}