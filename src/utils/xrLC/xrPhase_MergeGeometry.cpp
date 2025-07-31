#include "stdafx.h"

#include "build.h"
#include "../xrLC_Light/xrface.h"
#include <atomic>

extern void Detach(vecFace* S);

xr_map<u32, Fbox > bb_bases;


IC BOOL	FaceEqual(Face* F1, Face* F2)
{
	if (F1->v[0]->P.distance_to(F2->v[0]->P) > 64) return FALSE;
	if (F1->dwMaterial != F2->dwMaterial)		return FALSE;
	if (F1->tc.size() != F2->tc.size())		return FALSE;
	if (F1->lmap_layer != F2->lmap_layer)		return FALSE;
	return TRUE;
}

ICF void CreateBox(vecFace& subdiv, Fbox& bb_base, u32 id)
{
	if (bb_bases.find(id) == bb_bases.end())
	{
		for (u32 it = 0; it < subdiv.size(); it++)
		{
			Face* F = subdiv[it];
			bb_base.modify(F->v[0]->P);
			bb_base.modify(F->v[1]->P);
			bb_base.modify(F->v[2]->P);

		}
		// csBoxes.Enter();
		bb_bases[id] = bb_base;
		// csBoxes.Leave();
	}
	else
	{
		bb_base = bb_bases[id];
	}
}

BOOL	NeedMerge(vecFace& subdiv, Fbox& bb_base, u32 id)
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
	 
	if (sz_base.x < c_SS_maxsize)		return TRUE;
	if (sz_base.y < c_SS_maxsize)		return TRUE;
	if (sz_base.z < c_SS_maxsize)		return TRUE;
	return FALSE;
}

IC void	MakeCube(Fbox& BB_dest, const Fbox& BB_src)
{
	Fvector C, D;
	BB_src.get_CD(C, D);
	float	max = D.x;
	if (D.y > max)	max = D.y;
	if (D.z > max)	max = D.z;

	BB_dest.set(C, C);
	BB_dest.grow(max);
}

IC BOOL ValidateMergeLinearSize(const Fvector& merged, const Fvector& orig1, const Fvector& orig2, int iAxis)
{
	if ((merged[iAxis] > (4 * c_SS_maxsize / 3)) &&
		(merged[iAxis] > (orig1[iAxis] + 1)) &&
		(merged[iAxis] > (orig2[iAxis] + 1)))
		return FALSE;
	else
		return TRUE;
}

IC BOOL	ValidateMerge(u32 f1, const Fbox& bb_base, const Fbox& bb_base_orig, u32 f2, const Fbox& bb, float& volume)
{
	// Polygons
	if ( (f1 + f2) > u32(8192) )	
		return FALSE;	// Don't exceed limits (4/3 max POLY)	

 

	// Size
	Fbox	merge;
	merge.merge(bb_base, bb);

	Fvector sz, orig1, orig2;
	merge.getsize(sz);
	bb_base_orig.getsize(orig1);
	bb.getsize(orig2);

	if (!ValidateMergeLinearSize(sz, orig1, orig2, 0))	return FALSE;	// Don't exceed limits (4/3 GEOM)
	if (!ValidateMergeLinearSize(sz, orig1, orig2, 1))	return FALSE;
	if (!ValidateMergeLinearSize(sz, orig1, orig2, 2))	return FALSE;

	// Volume
	Fbox		bb0, bb1;
	MakeCube(bb0, bb_base);
	float	v1 = bb0.getvolume();
	MakeCube(bb1, bb);
	float	v2 = bb1.getvolume();

	volume = merge.getvolume();

	if (volume > 8 * (v1 + v2)) // 2 * 2 * 2		
		return FALSE;	// Don't merge too distant groups (8 vol)

	// OK
	return TRUE;
}
 
// Новый метод для расщета Merging Geom (se7kills) 

void CBuild::xrPhase_MergeGeometry()
{
	string128 tmp;
	sprintf(tmp, "Merge... [%d]", g_XSplit.size());
	Phase(tmp);

	validate_splits();
 
	u16 max_threads = CPU::ID.n_threads;

	// se7kills (Возврат на старую версию мерджа геометрии)
	{
		struct data_vec
		{
			int face_id;
			bool merged = false;
		};

		xr_hash_map<int, xr_vector<data_vec>> thread_faces;

		for (int split = 0; split < g_XSplit.size(); split++)
		{
			thread_faces[g_XSplit[split]->front()->dwMaterial].push_back(data_vec{ split });
		}

		auto Validate = [](u32& CurrentProcessedID, xr_vector<data_vec>& vec, Fbox& bb_base, Fbox& bb_base_orig)
		{
			float	selected_volume = flt_max;

			u32 SelectedStart = CurrentProcessedID;

			for (auto test : vec)
			{
				if (SelectedStart == test.face_id)
					continue;
  				if (test.merged)
					continue;
				Fbox bb;
				vecFace& TEST = *(g_XSplit[test.face_id]);
				vecFace* subdiv = (g_XSplit[SelectedStart]);

				if (!FaceEqual(subdiv->front(), TEST.front()))
					continue;
				if (!NeedMerge(TEST, bb, test.face_id))
					continue;
				
				float value;
				if (!ValidateMerge(subdiv->size(), bb_base, bb_base_orig, TEST.size(), bb, value))
					continue;

				if (value > selected_volume)
					continue;

				CurrentProcessedID = test.face_id;
				selected_volume = value;
			} 
		};

 
		int IDX = 0;
		int Merged = 0;
		for (auto& mapMAT : thread_faces)
		{
 			Msg("MergeMat: faces(%u) progress(%u/%u) Merged(%u)", mapMAT.second.size(), IDX, thread_faces.size(), Merged);
			IDX++;
			for (auto& Face : thread_faces[mapMAT.first])
			{
 				auto faceID = Face.face_id;
				if (g_XSplit[faceID]->empty() || Face.merged)
					continue;

				vecFace&	subdiv = *(g_XSplit[faceID]);
				
				
				bool		bb_base_orig_inited = false;
				Fbox		bb_base_orig;
				Fbox		bb_base;
				
				// int CountTryes = 0;
	 
				while (NeedMerge(subdiv, bb_base, faceID))
				{
					//	Save original AABB for later tests
					if (!bb_base_orig_inited)
					{
						bb_base_orig_inited = true;
						bb_base_orig = bb_base;
					}
					u32		CurrentProcessedID = faceID;

					// Merge-validate
					Validate (
						CurrentProcessedID,
						thread_faces[mapMAT.first],
						bb_base, bb_base_orig
					);
					 
					if (CurrentProcessedID == faceID)
  						break;

					// **OK**. Perform merge
					subdiv.insert(subdiv.begin(), g_XSplit[CurrentProcessedID]->begin(), g_XSplit[CurrentProcessedID]->end());
					g_XSplit[CurrentProcessedID]->clear();
					bb_bases.erase(CurrentProcessedID);
					Face.merged = true;

					Merged++;
				}
 
				thread_faces[mapMAT.first].erase(
					std::remove_if(
 						thread_faces[mapMAT.first].begin(), 
						thread_faces[mapMAT.first].end(), 
						[&](data_vec& vec)
						{ return vec.merged; }
					), 
					thread_faces[mapMAT.first].end()
				);
 			}
		}
	 
		// Clear Data
		thread_faces.clear();

		g_XSplit.erase(std::remove_if(g_XSplit.begin(), g_XSplit.end(),
		[](vecFace* ptr)
		{
			if (ptr == nullptr)
				return true;
			return ptr->empty();
		}),
		g_XSplit.end());

	}
	 
	

	string128 tmp2;
	sprintf_s(tmp2, "Merged %u", g_XSplit.size());
	Phase(tmp2);
	validate_splits();

}


