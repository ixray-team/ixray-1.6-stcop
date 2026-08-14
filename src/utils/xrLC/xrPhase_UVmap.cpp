#include "StdAfx.h"
#include "Build.h"

#include "../xrLC_Light/xrDeflector.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrFace.h"


void Detach(vecFace* S, vecVertex& vertices_storage, bool IsMU)
{
	map_v2v verts;
	verts.clear();

	// Collect vertices
	for (vecFaceIt F = S->begin(); F != S->end(); ++F)
	{
		for (int i = 0; i < 3; ++i)
		{
			TVertex* V = (*F)->v[i];
			TVertex* VNewCreate;
			map_v2v_it W = verts.find(V); // iterator

			if (W == verts.end())
			{																	  // where is no such-vertex
				VNewCreate = V->CreateCopy_NOADJ(vertices_storage, IsMU); // make copy
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

bool sort_faces(TFace* face, TFace* face2)
{
	if (face->CalcArea() > face2->CalcArea())
	{
		return true;
	}
	return false;
}

void CBuild::xrPhase_UVmap(vec2Face& Split, vecVertex& vertices_storage, bool IsMU)
{
	CTimer tState;
	tState.Start();

	// Main loop
	Status("Processing...");
	lc_global_data()->g_deflectors().reserve(64 * 1024);
	float p_cost = 1.f / float(Split.size());
	float p_total = 0.f;
	vecFace faces_affected;

	//se7kills : НЕ ТРОГАТЬ !!! (Ломает сектора !)
	//mnelenpridumivat : Понял, потрогаю, но аккуратно
	int StartPoint = Split.size();
 	for (int SP = 0; SP < int(StartPoint); SP++)
	{
		Progress(p_total += p_cost);

		// Detect vertex-lighting and avoid this subdivision
		if (Split[SP]->empty())
		{
			continue;
		}

		TFace* Fvl = Split[SP]->front();
		if (Fvl->Shader().flags.bLIGHT_Vertex)
		{
			continue; // do-not touch (skip)
		}
		if (!Fvl->Shader().flags.bRendering)
		{
			continue; // do-not touch (skip)
		}
		if (Fvl->hasImplicitLighting())
		{
			continue; // do-not touch (skip)
		}

		while (true)
		{
			// Сортировка списка в перед с больщими зонами.
			std::ranges::sort(*Split[SP], sort_faces);
			if (Split[SP] == nullptr)
			{
				break;
			}
			// Select maximal sized poly
			TFace* msF = nullptr;

			for (auto FACE : *Split[SP])
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
					Detach(&faces_affected, vertices_storage, IsMU);
					Split.push_back(new vecFace(faces_affected));
 				}
			}

			if (!Split[SP]->empty())
			{
				// u32 CapacityOrig = g_XSplit[SP]->capacity();
				auto rIT = std::ranges::remove_if(*Split[SP],
				                                  [&](TFace* F)
				                                  {
					                                  if (F->pDeflector != nullptr)
					                                  {
						                                  // xr_delete(F);   // Освобождаем память
						                                  return true; // Убираем из контейнера
					                                  }
					                                  return false;
				                                  }
				).begin();

				if (rIT != Split[SP]->end())
				{
					Split[SP]->erase(rIT, Split[SP]->end());
					Split[SP]->shrink_to_fit();
				}
			}

			// Cancel infine loop (while)
			if (msF == nullptr)
			{
				break;
			}
		}

		AditionalData("SP[%u], xsp: %u", SP, Split.size());
	}

	clMsg("%d subdivisions...", Split.size());

	// VALIDATION
	for (auto SP = 0; SP < Split.size(); SP++)
	{
		if (Split[SP]->empty())
		{
			xr_delete(Split[SP]);
			Split.erase(Split.begin() + SP);
			SP--;
		}
	}

	err_save();

	clMsg("* UVMap Time: %u ms", tState.GetElapsed_ms());
}