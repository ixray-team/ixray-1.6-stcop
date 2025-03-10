#include "StdAfx.h"

#include "Build.h"
#include "../xrLC_Light/xrFace.h"

#include <thread>
#include <ppl.h>

xrCriticalSection csMerge;
extern void Detach(vecFace* S);

struct data_material
{
 	int face_id = 0;
	vecFace* subdiv = nullptr;
	bool merged = false;
	Fbox bbox;
	data_material(int fID, Fbox box, vecFace* faces)
	{
		subdiv = faces;
		bbox = box;
 		face_id = fID;
		merged = false;
	};
};

// Stuff For need
ICF void	MakeCube(Fbox& BB_dest, const Fbox& BB_src)
{
	Fvector C, D;
	BB_src.get_CD(C, D);
	float	max = D.x;
	if (D.y > max)	max = D.y;
	if (D.z > max)	max = D.z;

	BB_dest.set(C, C);
	BB_dest.grow(max);
}

ICF void	CreateBox(vecFace& subdiv, Fbox& bb_base, u32 id)
{ 
	for (u32 it = 0; it < subdiv.size(); it++)
	{
		Face* F = subdiv[it];
		bb_base.modify(F->v[0]->P);
		bb_base.modify(F->v[1]->P);
		bb_base.modify(F->v[2]->P);
	}
} 

ICF BOOL	FaceEqual(Face* F1, Face* F2)
{
	if (F1->dwMaterial != F2->dwMaterial)		return FALSE;
	if (F1->tc.size() != F2->tc.size())			return FALSE;
	if (F1->lmap_layer != F2->lmap_layer)		return FALSE;
	return TRUE;
}

ICF BOOL	NeedMerge(vecFace& subdiv, Fbox& bb_base, u32 id)
{
	// 1. Amount of polygons
	if (subdiv.size() >= u32(3 * c_SS_HighVertLimit / 4))
		return FALSE;

	Fvector sz_base;

	// 2. Bounding box
	bb_base.invalidate();
	CreateBox(subdiv, bb_base, id);

	bb_base.grow(EPS_S);	// Enshure non-zero volume
	bb_base.getsize(sz_base);
	if (sz_base.x < c_SS_maxsize)
		return TRUE;
	if (sz_base.y < c_SS_maxsize)
		return TRUE;
	if (sz_base.z < c_SS_maxsize)
		return TRUE;

	return true;
}


// Без постройки Fbox
ICF BOOL NeedMerge_for(vecFace& subdiv, Fbox bb_base)
{
	// 1. Amount of polygons
	if (subdiv.size() >= u32(3 * c_SS_HighVertLimit / 4))
		return FALSE;

	Fvector sz_base;
	
 	// 2. Bounding box
	bb_base.grow(EPS_S);	// Enshure non-zero volume
	bb_base.getsize(sz_base);
	if (sz_base.x < c_SS_maxsize)
		return TRUE;
	if (sz_base.y < c_SS_maxsize)
		return TRUE;
	if (sz_base.z < c_SS_maxsize)
		return TRUE;

	return true;
}

ICF BOOL	ValidateMergeLinearSize(const Fvector& merged, const Fvector& orig1, const Fvector& orig2, int iAxis)
{
	if ((merged[iAxis] > (4 * c_SS_maxsize / 3)) &&
		(merged[iAxis] > (orig1[iAxis] + 1)) &&
		(merged[iAxis] > (orig2[iAxis] + 1)))
		return FALSE;
	else
		return TRUE;
}

ICF BOOL	ValidateMerge(u32 f1, u32 f2, float& volume, const Fbox& bb_subdiv, const Fbox& bb_base, const Fbox& bb_base_orig)
{
	// Polygons
	if ((f1 + f2) > u32(4 * c_SS_HighVertLimit / 3))
		return FALSE;	// Don't exceed limits (4/3 max POLY)	

	Fbox	merge;
	merge.merge(bb_base, bb_subdiv);

	Fvector sz, orig1, orig2;
	merge.getsize(sz);
	bb_base_orig.getsize(orig1);
	bb_subdiv.getsize(orig2);

	if (!ValidateMergeLinearSize(sz, orig1, orig2, 0))	return FALSE;	// Don't exceed limits (4/3 GEOM)
	if (!ValidateMergeLinearSize(sz, orig1, orig2, 1))	return FALSE;
	if (!ValidateMergeLinearSize(sz, orig1, orig2, 2))	return FALSE;


	// Volume
	Fbox		bb0, bb1;
	MakeCube(bb0, bb_base);
	float	v1 = bb0.getvolume();
	MakeCube(bb1, bb_subdiv);
	float	v2 = bb1.getvolume();

	volume = merge.getvolume();
	if (volume > 8 * (v1 + v2))
		return FALSE;	// Don't merge too distant groups (8 vol)

	// OK
	return TRUE;
}
 
auto Validate = [](u32& CurrentProcessedID, u32& VecIndex, data_material& cmaterial_subdiv, xr_vector<data_material>& data_vector,  Fbox bb_base, Fbox bb_base_orig)
{
	u32 SelectedStart = CurrentProcessedID;
	float SelectedVolume = flt_max;
	
	if (data_vector.size() > 256)
	{
  		xr_parallel_for(size_t(0), size_t(data_vector.size()), [&](size_t I)
		{
 			data_material test = data_vector[I];
			if (SelectedStart == test.face_id || test.merged) return;
	
			float V;
			vecFace& TEST = *(g_XSplit[test.face_id]);
			vecFace* subdiv = (g_XSplit[SelectedStart]);
		
			if (!FaceEqual(subdiv->front(), TEST.front())) return;
			if (!NeedMerge_for(TEST, test.bbox)) return;
			if (!ValidateMerge(subdiv->size(), TEST.size(), V, test.bbox, bb_base, bb_base_orig)) return;
	
			csMerge.Enter();
			if (V < SelectedVolume)
			{
 				CurrentProcessedID = test.face_id;
				VecIndex = I;
				SelectedVolume = V;
 			}
			csMerge.Leave();
  		});
	}
	else
	{
		for (auto Index = 0; Index < data_vector.size(); Index++)
		{
			auto& test = data_vector[Index];

			if (SelectedStart == test.face_id || test.merged) continue;

			float V;
			vecFace& TEST = *(g_XSplit[test.face_id]);
			vecFace* subdiv = (g_XSplit[SelectedStart]);
		 
			if (!FaceEqual(subdiv->front(), TEST.front())) continue;
			if (!NeedMerge_for(TEST, test.bbox)) continue;
			if (!ValidateMerge(subdiv->size(), TEST.size(), V, test.bbox, bb_base, bb_base_orig)) continue;

			if (V < SelectedVolume)
			{
				CurrentProcessedID = test.face_id;
				VecIndex = Index;
				SelectedVolume = V;
				// break;
			}
 		}
	}
};
 
struct GridKey 
{
	int x, y;

	bool operator==(const GridKey& other) const { return x == other.x && y == other.y; }

	struct Hash 
	{
		std::size_t operator()(const GridKey& k) const {
			return std::hash<int>()(k.x) ^ (std::hash<int>()(k.y) << 1);
		}
	};
};

  
void MergeCandidate()
{
 	xr_concurrent_unordered_map<int, xr_vector<data_material>> thread_faces;
	
	// Generate Materials
	for (int split = 0; split < g_XSplit.size(); split++)
	{
		Fbox bbox;

		for (auto F : *g_XSplit[split])
		{
			bbox.modify(F->v[0]->P);
			bbox.modify(F->v[1]->P);
			bbox.modify(F->v[2]->P);
		}

		thread_faces[g_XSplit[split]->front()->dwMaterial].push_back(data_material{ split, bbox, g_XSplit[split] });
	}
	 
	u32 MATERIAL_IDX = 0;
	for (auto MAP : thread_faces)
	{
		MATERIAL_IDX++;

		Progress( float (MATERIAL_IDX) / float(thread_faces.size()) );
		// Строим Хэш мапу для ускорения поиска похожих сабдивов
		std::unordered_map<GridKey, xr_vector<data_material>, GridKey::Hash> grid_map;

		for (auto& data : MAP.second)
		{
 			// Получаем Fbox и вычисляем ключ сетки
 			int cell_x = static_cast<int>(std::floor(data.bbox.min.x / 96.0f));
			int cell_y = static_cast<int>(std::floor(data.bbox.min.z / 96.0f));

			grid_map[{cell_x, cell_y}].push_back(data);
		}

		// Msg("MaterialID [%u]/[%u] Cells Size[%u]", MATERIAL_IDX, thread_faces.size(), grid_map.size());

		int IDX_GRID = 0;
		for (auto& grid : grid_map)
		{
 			// AditionalData("GRID [%u]/[%u] Process[%U]", IDX_GRID, grid.second.size());
 			IDX_GRID++;
			for (auto& Face : grid.second)
			{
 				auto faceID = Face.face_id;
				if (g_XSplit[faceID]->empty() || Face.merged)
					continue;

				vecFace& subdiv = *(g_XSplit[faceID]);
 								
				bool		bb_base_orig_inited = false;
				Fbox		bb_base_orig;
				Fbox		bb_base;

				while (NeedMerge(subdiv, bb_base, faceID))
				{
					//	Save original AABB for later tests
					if (!bb_base_orig_inited)
					{
						bb_base_orig_inited = true;
						bb_base_orig = bb_base;
					}
				 	
					//	Save original AABB for later tests
  					u32	CurrentProcessedID = faceID;
					u32 MatrialIDX = 0;
					
					// Merge-validate
					Validate(
						CurrentProcessedID,
						MatrialIDX,
						Face,
						grid.second,
						bb_base, 
						bb_base_orig
					);

					if (CurrentProcessedID == faceID)
						break;

					// **OK**. Perform merge					 
					if (g_XSplit[CurrentProcessedID])
					{
						subdiv.insert(subdiv.begin(), g_XSplit[CurrentProcessedID]->begin(), g_XSplit[CurrentProcessedID]->end());
						g_XSplit[CurrentProcessedID]->clear();
					}
 					grid.second[MatrialIDX].merged = true;
				}

				grid.second.erase(
					std::remove_if(
						grid.second.begin(),
						grid.second.end(),
						[&](data_material& vec)
						{ return vec.merged; }
					),
					grid.second.end()
				);
			}
		}
	}

	g_XSplit.erase(std::remove_if(g_XSplit.begin(), g_XSplit.end(),
	[](vecFace* ptr)
	{
		if (ptr == nullptr)
			return true;
		return ptr->empty();
	}),
	g_XSplit.end());
}

void CBuild::xrPhase_MergeGeometry()
{
	string128 tmp;
	sprintf(tmp, "Merge Started... [%u]", g_XSplit.size());
	clMsg(tmp);
 
	MergeCandidate();

	// Проверяем на INFINITY
	validate_splits();

	AditionalData("Splits Merged [%u]", g_XSplit.size());
}
