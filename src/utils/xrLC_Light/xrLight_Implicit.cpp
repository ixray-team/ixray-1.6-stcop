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

struct LummelData
{
	u32 Jcount;
	Fvector2* JD;

 	Fvector2 dim;
	Fvector2 half;
	Fvector2 JS;
};

void RunImplicitMultithread(ImplicitDeflector& defl)
{

	// Thread Local Update
	thread_local xr_vector<JiterPixel> EmbreePacket;
 
	// Start threads
	ThreadTaskID_Implication = 0;
	xr_parallel_for(size_t(0), size_t(gCompilerMode.ThreadsPerWork), [](size_t thread)
		{
			ImplicitDeflector& defl = cl_globs.DATA();
		
			Fvector2* Jitter2D; u32 Jcount;
			Jitter_Select(Jitter2D, Jcount);

			LummelData Pixel;
			Pixel.dim.set(float(defl.Width()), float(defl.Height()));
			Pixel.half.set(.5f / Pixel.dim.x, .5f / Pixel.dim.y);
			Pixel.JS.set(.499f / Pixel.dim.x, .499f / Pixel.dim.y);
			Pixel.Jcount = Jcount;
			Pixel.JD = Jitter2D;
			
			
			// Размер сетки !
			u32 TILE_SIZE = 8;
 			while (true)
			{
				u32 Y = ThreadTaskID_Implication.fetch_add(TILE_SIZE);
				if (Y >= defl.Height()) break;

				AditionalData("Implict Deflector: {%u/%u}", Y, defl.Height());
				for (u32 X = 0; X < defl.Width(); X += TILE_SIZE)
				{
					EmbreePacket.clear();
					 
					// Only Lightpoint 
					/*
 					thread_local base_color_c C; 
					thread_local Fvector wP, wN, B;
					for (auto J = 0; J < Pixel.Jcount; J++)
					{
						C.clear_color();

						// LUMEL space
						Fvector2 P;
						P.x = float(X) / Pixel.dim.x + Pixel.half.x + Pixel.JD[J].x * Pixel.JS.x;
						P.y = float(Y) / Pixel.dim.y + Pixel.half.y + Pixel.JD[J].y * Pixel.JS.y;

						// World space
						for (auto F : cl_globs.query(P.x, P.y))
						{
							_TCF& tc = F->tc[0];
							if (tc.isInside(P, B))
							{
								// We found triangle and have barycentric coords
								GetBarycentric(F, wP, wN, B);
 								LightPoint(0, 0, C, wP, wN, lc_global_data()->L_static(), LGetCurrentFlags(), F);
								break;
							}
						}

						defl.Lumel(X, Y)._add(C);
						defl.Samples(X, Y) += 1;
					}
					*/
					
					// Packed Way
 					for (u32 tX = X; tX < std::min(defl.Width(), X + TILE_SIZE); tX++)
					for (u32 tY = Y; tY < std::min(defl.Height(), Y + TILE_SIZE); tY++)
					{
					
 						thread_local Fvector wP, wN, B;
						for (auto J = 0; J < Pixel.Jcount; J++)
						{
							// LUMEL space
							Fvector2 P;
							P.x = float(tX) / Pixel.dim.x + Pixel.half.x + Pixel.JD[J].x * Pixel.JS.x;
							P.y = float(tY) / Pixel.dim.y + Pixel.half.y + Pixel.JD[J].y * Pixel.JS.y;

							// World space
							for (auto F : cl_globs.query(P.x, P.y))
							{
								_TCF& tc = F->tc[0];
								if (tc.isInside(P, B))
								{
									// We found triangle and have barycentric coords
									GetBarycentric(F, wP, wN, B);
									EmbreePacket.emplace_back().SetDataRays(tY, tX, wP, wN, F);
 									break;
								}
							}
						}
					
					}

					LightPoint_Jitters(EmbreePacket, lc_global_data()->L_static(), LGetCurrentFlags());
					
					for (auto& WP : EmbreePacket)
					{
						defl.Lumel(WP.U, WP.V)._add(WP.C);
						defl.Samples(WP.U, WP.V) += 1;
					}
				}
			}
		}
	);
   
	// Apply Colors !
	for (auto V = 0; V<defl.Height(); V++)
	for (auto U = 0; U<defl.Width(); U++)
	{
		u32 Samples = defl.Samples(U, V);
		if (Samples)
		{
			// Calculate lighting amount
			base_color_c C;
			defl.Lumel(U, V)._get(C);
			C.scale(Samples);
			C.mul(.5f);
			defl.Lumel(U, V)._set(C);
			defl.Marker(U, V) = 255;
		}
	}
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