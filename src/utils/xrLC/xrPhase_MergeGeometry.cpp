#include "stdafx.h"

#include "build.h"
#include "../xrLC_Light/xrface.h"

#include <thread>

extern void Detach(vecFace* S);

IC BOOL	FaceEqual(Face* F1, Face* F2)
{
 	if (F1->dwMaterial != F2->dwMaterial)		return FALSE;
	if (F1->tc.size() != F2->tc.size())		return FALSE;
	if (F1->lmap_layer != F2->lmap_layer)		return FALSE;
	return TRUE;
}
 
ICF void CreateBox(vecFace& subdiv, Fbox& bb_base, u32 id)
{
	for (u32 it = 0; it < subdiv.size(); it++)
	{
		Face* F = subdiv[it];
		bb_base.modify(F->v[0]->P);
		bb_base.modify(F->v[1]->P);
		bb_base.modify(F->v[2]->P);
 	}
}
 
BOOL	NeedMerge(vecFace& subdiv, Fbox& bb_base, u32 id)
{
	// 1. Amount of polygons
	if (subdiv.size() >= u32(3 * c_SS_HighVertLimit / 4))	return FALSE;

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
	if ((f1 + f2) > u32(4 * c_SS_HighVertLimit / 3))		return FALSE;	// Don't exceed limits (4/3 max POLY)	


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


#include <ppl.h>


struct data_vec
{
	int face_id = 0;
	bool merged = false;
	data_vec(int fID)
	{
		face_id = fID;
		merged = false;
	};
};

auto Validate = [](u32& CurrentProcessedID, u32& FaceIndex, xr_vector<data_vec>& vec, Fbox& bb_base, Fbox& bb_base_orig)
	{
		float	selected_volume = flt_max;

		u32 SelectedStart = CurrentProcessedID;

		for (auto Index = 0; Index < vec.size(); Index++)
		{
			auto& test = vec[Index];

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

			FaceIndex = Index;
			CurrentProcessedID = test.face_id;
			selected_volume = value;
			break;
		}
	};

typedef xr_hash_map<int, xr_hash_map<int, xr_vector<data_vec>> > VectorSplited;

typedef xr_hash_map<int, xr_vector<data_vec>> MergingVector;


xrCriticalSection csMerge;

void BasicMerge()
{
	MergingVector thread_faces;

	// Generate Materials
	for (int split = 0; split < g_XSplit.size(); split++)
	{
		thread_faces[g_XSplit[split]->front()->dwMaterial].push_back(data_vec{ split });
	}
	 
	CTimer t;
	t.Start();
	// se7kills (Возврат на старую версию мерджа геометрии)
	{
		int IDX = 0;
		int Merged = 0;
		
		xr_atomic_u32 progress__ = 0;
		int SIZE = thread_faces.size();

			concurrency::parallel_for(size_t(0), size_t(thread_faces.size()), [&](size_t IDX)
			{
				auto& mapMAT = thread_faces[IDX];

				IDX++;
				CTimer t; t.Start();
				for (auto& Face : thread_faces[IDX])
				{
					auto faceID = Face.face_id;
					if (g_XSplit[faceID]->empty() || Face.merged)
						continue;

					vecFace& subdiv = *(g_XSplit[faceID]);


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
						u32	CurrentProcessedID = faceID;
						u32 FaceMaterialID = -1;

						// Merge-validate
						Validate(
							CurrentProcessedID,
							FaceMaterialID,
							thread_faces[IDX],
							bb_base, bb_base_orig
						);

						if (CurrentProcessedID == faceID)
							break;

						// **OK**. Perform merge
						csMerge.Enter();

						if (g_XSplit[CurrentProcessedID])
						{
							subdiv.insert(subdiv.begin(), g_XSplit[CurrentProcessedID]->begin(), g_XSplit[CurrentProcessedID]->end());
							g_XSplit[CurrentProcessedID]->clear();
							thread_faces[IDX][FaceMaterialID].merged = true;
							Merged++;
						}

						csMerge.Leave();
					}

					csMerge.Enter();
					thread_faces[IDX].erase(
						std::remove_if(
							thread_faces[IDX].begin(),
							thread_faces[IDX].end(),
							[&](data_vec& vec)
							{ return vec.merged; }
						),
						thread_faces[IDX].end()
					);
					csMerge.Leave();
				}
			
				progress__.fetch_add(1);

				Progress( float (progress__ / SIZE) );
			});
	}

	g_XSplit.erase(std::remove_if(g_XSplit.begin(), g_XSplit.end(),
		[](vecFace* ptr)
		{
			if (ptr == nullptr)
				return true;
			return ptr->empty();
		}),
		g_XSplit.end());
	 
	clMsg("Merging OGFs size [%u], time (%u)", g_XSplit.size(), t.GetElapsed_ms());
}

void SplittedMerge()
{
	MergingVector thread_faces;

	// Generate Materials
	for (int split = 0; split < g_XSplit.size(); split++)
	{
		thread_faces[g_XSplit[split]->front()->dwMaterial].push_back(data_vec{ split });
	}

	// Generate Splits
	VectorSplited splited_faces;
	for (auto& HASH : thread_faces)
	{
		int ID = 0;
		if (HASH.second.size() > 4096 * 8)
		{
			int Packed = 0;
			for (auto& map_ : HASH.second)
			{
				splited_faces[HASH.first][ID].push_back(map_);

				Packed++;

				if (Packed > 4096)
				{
					Packed = 0;
					ID++;
				}
			}

			Msg("Packed Material[%u] IDS: %u", HASH.first, ID);
			thread_faces[HASH.first].clear();
		}
	}
 
	CTimer t; 
	t.Start();
	// SPLITTED DATA
	{
		int IDx = 0;
		for (auto& vec_split : splited_faces)
		{
			CTimer t;
			t.Start();

			int Merged = 0;
			int IDX_Vec = 0;

			IDx++;

			clMsg("BigFaces Materials: StartProcess progress(%u/%u) Splits(%u)", IDx, splited_faces.size(), vec_split.second.size());

			int IDX_material = vec_split.first;
 			concurrency::parallel_for(size_t(0), size_t(vec_split.second.size()), [&] (size_t IDX_curent)
 			{
  				IDX_Vec = IDX_curent;
				for (auto& Face : splited_faces[IDX_material][IDX_curent])
				{
					auto faceID = Face.face_id;
					if (g_XSplit[faceID]->empty() || Face.merged)
						continue;

					vecFace& subdiv = *(g_XSplit[faceID]);

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
						u32	CurrentProcessedID = faceID;
						u32 FaceMaterialID = -1;

						// Merge-validate
						Validate(
							CurrentProcessedID,
							FaceMaterialID,
							splited_faces[IDX_material][IDX_curent],
							bb_base, bb_base_orig
						);

						if (CurrentProcessedID == faceID)
							break;

						// **OK**. Perform merge
							
						csMerge.Enter();
 						subdiv.insert(subdiv.begin(), g_XSplit[CurrentProcessedID]->begin(), g_XSplit[CurrentProcessedID]->end());
						g_XSplit[CurrentProcessedID]->clear();
 						splited_faces[IDX_material][IDX_curent][FaceMaterialID].merged = true;
						Merged++;
						csMerge.Leave();

					}

					csMerge.Enter();
					splited_faces[IDX_material][IDX_curent].erase(
						std::remove_if(
							splited_faces[IDX_material][IDX_curent].begin(),
							splited_faces[IDX_material][IDX_curent].end(),
							[&](data_vec& vec)
							{ return vec.merged; }
						),
						splited_faces[IDX_material][IDX_curent].end()
					);
					csMerge.Leave();
				}
			});

			int MergedOGF = 0;
			for (auto V : splited_faces[vec_split.first])
			{
				MergedOGF += V.second.size();
			}

			clMsg("BigFaces Materials: progress(%u/%u) Splits(%u) OGF_Merged(%u) Merged(%u), time: %u ms", IDx, splited_faces.size(), IDX_Vec, MergedOGF, Merged, t.GetElapsed_ms());
		}

		//for (auto K : splited_faces)
		//if (K.second.size())
		// 	K.second.clear();
 		// splited_faces.clear();
	}

	g_XSplit.erase(std::remove_if(g_XSplit.begin(), g_XSplit.end(),
		[](vecFace* ptr)
		{
			if (ptr == nullptr)
				return true;
			return ptr->empty();
		}),
		g_XSplit.end());

	clMsg("(Split) Merging OGFs size [%u], time (%u)", g_XSplit.size(), t.GetElapsed_ms());
}

 
// Новый метод для расщета Merging Geom (se7kills) 

void CBuild::xrPhase_MergeGeometry()
{
	string128 tmp;
	sprintf(tmp, "Merge Started... [%u]", g_XSplit.size());
	Phase(tmp);
	
	// SPLIT FOR FASTER MERGING
	// Сплитим очень большие буфера по 4096 и обрабатываем в MT
  	SplittedMerge();

	u32 LastGXSplits = 0;
	u32 FirstGXSplits = -1;
	u32 idX = 0;

	// Крутим в while пока нечего будет приклеить
	while (LastGXSplits != FirstGXSplits)
	{
 		clMsg("Procesing MergeID: %u", idX);
		FirstGXSplits = g_XSplit.size();
		// SECONDARY MERGER

		BasicMerge();
 
		LastGXSplits = g_XSplit.size();
		idX++;
	}

	// Проверяем на INFINITY
 	validate_splits();

	AditionalData("Splits Merged [%u]", g_XSplit.size());
}