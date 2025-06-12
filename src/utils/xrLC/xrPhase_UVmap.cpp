#include "StdAfx.h"
#include "Build.h"

#include "../xrLC_Light/xrDeflector.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrFace.h"



extern bool CheckInfinity_FBOX(Fbox& box)
{
	constexpr float inf = std::numeric_limits<float>::infinity();

	if (box.min.x == -inf || box.min.x == inf)
		return true;
	if (box.min.y == -inf || box.min.y == inf)
		return true;
	if (box.min.z == -inf || box.min.z == inf)
		return true;

	if (box.max.x == -inf || box.max.x == inf)
		return true;
	if (box.max.y == -inf || box.max.y == inf)
		return true;
	if (box.max.z == -inf || box.max.z == inf)
		return true;


	return false;
}

extern void Validate_gXsplit()
{
	// int IDX_Bbox = 0;
	// for (auto& SP : g_XSplit)
	// {
	// 	Fbox bbox;
	// 	bbox.invalidate();
	// 
	// 	for (auto K : *SP)
	// 	{
	// 		bbox.modify(K->v[0]->P);
	// 		bbox.modify(K->v[1]->P);
	// 		bbox.modify(K->v[2]->P);
	// 	}
	// 	bbox.grow(EPS_L);
	// 
	// 	if (CheckInfinity_FBOX(bbox))
	// 	{
	// 		Msg("SP[%u] is Infinity min{%.2f, %.2f, %.2f} max{%.2f, %.2f, %.2f} ", IDX_Bbox, VPUSH(bbox.min), VPUSH(bbox.max));
	// 	}
	// 	else
	// 	{
	// 	//	Msg("SP[%u] is FBOX:  min{%.2f, %.2f, %.2f} max{%.2f, %.2f, %.2f}", IDX_Bbox, VPUSH(bbox.min), VPUSH(bbox.max));
	// 	}
	// 	IDX_Bbox++;
	// }
}


void Detach(vecFace* S)
{
	map_v2v			verts;
	verts.clear		();
	
	// Collect vertices
	for (vecFaceIt F=S->begin(); F!=S->end(); ++F)
	{
		for (int i=0; i<3; ++i) 
		{
			Vertex*		V=(*F)->v[i];
			Vertex*		VC;
			map_v2v_it	W=verts.find(V);	// iterator
			
			if (W==verts.end()) 
			{	// where is no such-vertex
				VC = V->CreateCopy_NOADJ( lc_global_data()->g_vertices() );	// make copy
				verts.insert(std::make_pair(V, VC));
			} else 
			{
				// such vertex(key) already exists - update its adjacency
				VC = W->second;
			}
			VC->prep_add		(*F);
			V->prep_remove		(*F);
			(*F)->v[i]=VC;
		}
	}
	// vertices are already registered in container
	// so we doesn't need "vers" for this time
	verts.clear	();
}

extern int orig_size = 0;

bool find_AFFECTED(Face* face)
{
	return face->pDeflector != NULL;
}

bool sort_faces(Face* face, Face* face2)
{
	if (face->CalcArea() > face2->CalcArea())
		return true;
	return false;
}

#include <atomic>
xr_atomic_s32 integer_mt;

void MT_FindAttached(CDeflector* defl, vecFace& affected, int SP, int start, int end)
{
	for (int i = start; i != end; i++)
	{
		auto Face = (*g_XSplit[SP])[i];

		if (Face->pDeflector == defl)
		{
			affected.push_back(Face);
			integer_mt.fetch_add(1);
		}
	}
}

#include <thread>
#include <algorithm>
#include <execution>
 
extern bool CheckInfinity_FBOX(Fbox& bbox);

void CBuild::xrPhase_UVmap()
{
	size_t used, rel, free;
	vminfo(&free, &rel, &used);

	clMsg("xrPhase_UVmap: Start %u used", size_t(used / 1024 / 1024));

 	// Main loop
	Status("Processing...");
	lc_global_data()->g_deflectors().reserve(64 * 1024);
	float		p_cost = 1.f / float(g_XSplit.size());
	float		p_total = 0.f;
	vecFace		faces_affected;

	int StartPoint = g_XSplit.size();
 	int DeflectorsAllocated = 0;
	size_t CreatedFaces = 0;

	size_t OriginalFaces = 0;
	for (auto SP : g_XSplit)
		OriginalFaces += SP->size();
	 
	for (int SP = 0; SP < int(StartPoint); SP++)
	{
 		Progress(p_total += p_cost);

		// Detect vertex-lighting and avoid this subdivision
		if (g_XSplit[SP]->empty())				continue;
		Face* Fvl = g_XSplit[SP]->front();
		if (Fvl->Shader().flags.bLIGHT_Vertex) 	continue;	// do-not touch (skip)
		if (!Fvl->Shader().flags.bRendering) 	continue;	// do-not touch (skip)
		if (Fvl->hasImplicitLighting())			continue;	// do-not touch (skip)

		while (TRUE)
		{
			// Сортировка списка в перед с больщими зонами.
			std::sort(g_XSplit[SP]->begin(), g_XSplit[SP]->end(), sort_faces);
			if (g_XSplit[SP] == nullptr)
				break;
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
 					DeflectorsAllocated++;
					CreatedFaces += faces_affected.size();
				}
			}

			if (!g_XSplit[SP]->empty())
			{
				// u32 CapacityOrig = g_XSplit[SP]->capacity();
  				auto rIT = std::remove_if(
					g_XSplit[SP]->begin(),
					g_XSplit[SP]->end(),
					[&](Face* F)
					{
						if (F->pDeflector != nullptr)
						{
							//xr_delete(F);   // Освобождаем память
							return true;    // Убираем из контейнера
						}
 						return false;
					}
				);

				if (rIT != g_XSplit[SP]->end())
				{
					g_XSplit[SP]->erase(rIT, g_XSplit[SP]->end());
					g_XSplit[SP]->shrink_to_fit();
				}
				
				// u32 CapacityNew = g_XSplit[SP]->capacity();
				// 
				// if (CapacityNew != CapacityOrig)
				// 	Msg("Capacity: %u, new : %u", CapacityOrig, CapacityNew);
			}

			// Cancel infine loop (while)
			if (msF == nullptr)
			{
 				break;
			}
		}		

		size_t VSize = lc_global_data()->g_vertices().size() * sizeof(Vertex);
		size_t FSize = lc_global_data()->g_faces().size() * sizeof(Face);

		AditionalData("SP[%u], xsp: %u| V: %u, F: %u", SP, g_XSplit.size(),
			VSize / 1024 / 1024, FSize / 1024 / 1024);
	}
  
	size_t AllocatedDeflectors = 0;
	for (auto D : lc_global_data()->g_deflectors())
	{
		AllocatedDeflectors += D->size_deflector();
	}

	AllocatedDeflectors /= (1024 * 1024); // MB
	clMsg("UV Map is Ended generation[%d], Deflectors Allocated[%llu] MB", g_XSplit.size(), AllocatedDeflectors);
  	clMsg("%d subdivisions...", g_XSplit.size());
	err_save();
 
	// VALIDATION
	for (auto SP = 0; SP < g_XSplit[SP]->size(); SP++)
	{
		if (g_XSplit[SP]->size() == 0)
		{
			xr_delete(g_XSplit[SP]);
			g_XSplit.erase(g_XSplit.begin() + SP);
			SP--;
		}
	}

	vminfo(&free, &rel, &used);
	clMsg("xrPhase_UVmap: Ended %u used", size_t(used / 1024 / 1024));

	size_t NewOriginalFaces = 0;
	for (auto SP : g_XSplit)
		NewOriginalFaces += SP->size();

	size_t VSize = lc_global_data()->g_vertices().size() * sizeof(Vertex);
	size_t FSize = lc_global_data()->g_faces().size() * sizeof(Face);
	

	AditionalData("DF:%umb|Size(%umb)|V(%umb)T(%umb)",
		AllocatedDeflectors,
		(NewOriginalFaces * sizeof(Face*)) / 1024 / 1024,
		VSize/1024/1024,
		FSize/1024/1024
	);
	
	Status("UV SPLITS SP[%u]", g_XSplit.size());
 
	Validate_gXsplit();
}

void CBuild::mem_Compact()
{
	log_vminfo();
	Memory.mem_compact();
	log_vminfo();
}
