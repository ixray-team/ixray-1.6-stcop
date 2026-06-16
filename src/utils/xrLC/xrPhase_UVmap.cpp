#include "StdAfx.h"
#include "Build.h"

#include "../xrLC_Light/xrDeflector.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrFace.h"


void Detach(vecFace* S)
{
  	map_v2v			verts;
	verts.clear();

	// Collect vertices
	for (vecFaceIt F = S->begin(); F != S->end(); ++F)
	{
		for (int i = 0; i < 3; ++i)
		{
			Vertex* V = (*F)->v[i];
			Vertex* VNewCreate;
 			map_v2v_it	W = verts.find(V);	// iterator
 
			if (W == verts.end())
			{	// where is no such-vertex
				VNewCreate = V->CreateCopy_NOADJ(lc_global_data()->g_vertices());	// make copy
				verts.insert(std::make_pair(V, VNewCreate));
			}
			else
			{
				// such vertex(key) already exists - update its adjacency
				VNewCreate = W->second;
			}
			
			VNewCreate->prep_add(*F);
			V->prep_remove(*F);
 			(*F)->v[i] = VNewCreate;
		}
	}

	// vertices are already registered in container
	// so we doesn't need "vers" for this time
	verts.clear(); 	
}

bool sort_faces(Face* face, Face* face2)
{
	if (face->CalcArea() > face2->CalcArea())
		return true;
	return false;
}
 
void CBuild::xrPhase_UVmap()
{
	CTimer PhaseTime;
	PhaseTime.Start();

 	// Main loop
	Status("Processing...");
	lc_global_data()->g_deflectors().reserve(64 * 1024);

 	xr_vector<Face*> faces_affected;
  	for (int SP = 0; SP < int(g_XSplit.size()); SP++)
	{
 		// Detect vertex-lighting and avoid this subdivision
		if (g_XSplit[SP]->empty())				continue;
		Face* Fvl = g_XSplit[SP]->front();
		if (Fvl->Shader().flags.bLIGHT_Vertex) 	continue;	// do-not touch (skip)
		if (!Fvl->Shader().flags.bRendering) 	continue;	// do-not touch (skip)
		if (Fvl->hasImplicitLighting())			continue;	// do-not touch (skip)

		while (true)
		{
			// Сортировка списка в перед с больщими зонами.
			std::sort(g_XSplit[SP]->begin(), g_XSplit[SP]->end(), sort_faces);
			if (g_XSplit[SP] == nullptr) break;
			// Select maximal sized poly
			Face* msF = NULL;

			for (auto FACE : *g_XSplit[SP])
			{
 				if (FACE && FACE->pDeflector == nullptr)
				{
					msF = FACE;

					CDeflector* D = new CDeflector();
 					lc_global_data()->g_deflectors().push_back(D);

					// Start recursion from this face
					start_unwarp_recursion();
					D->OA_SetNormal(FACE->N);

					faces_affected.clear();
					FACE->OA_Unwarp(D, faces_affected);
					// break the cycle to startup again
					D->OA_Export();

					// detaching itself
					Detach(&faces_affected);
					g_XSplit.push_back(new vecFace(faces_affected));
 				}
			}

			if (!g_XSplit[SP]->empty())
			{
   				auto rIT = std::remove_if( g_XSplit[SP]->begin(), g_XSplit[SP]->end(),
					[&](Face* F)
					{
						if (F->pDeflector != nullptr)
 							return true;    // Убираем из контейнера
  						return false;
					}
				);

				if (rIT != g_XSplit[SP]->end())
				{
					g_XSplit[SP]->erase(rIT, g_XSplit[SP]->end());
					g_XSplit[SP]->shrink_to_fit();
				}
			}

			// Cancel infine loop (while)
			if (msF == nullptr)
			{
 				break;
			}
		}		
	}
 
  	clMsg("%d subdivisions...", g_XSplit.size());
 
 	// VALIDATION
	for (auto SP = 0; SP < g_XSplit.size(); SP++)
	{
		if (g_XSplit[SP]->empty())
		{
			xr_delete(g_XSplit[SP]);
			g_XSplit.erase(g_XSplit.begin() + SP);
			SP--;
		}
	}

	err_save();

	clMsg("* UVMap Time: %u ms", PhaseTime.GetElapsed_ms());
}