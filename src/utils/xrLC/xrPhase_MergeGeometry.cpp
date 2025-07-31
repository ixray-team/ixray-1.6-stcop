#include "StdAfx.h"

#include "Build.h"
#include "../xrLC_Light/xrFace.h"

#include <thread>
#include <ppl.h>

xrCriticalSection csMerge;
 
#define USE_PRECOMPUTED_BBOX 
// #define USE_GRID_SYSTEM

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
 
ICF void	CreateBox(vecFace& subdiv, Fbox& bb_base)
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

ICF BOOL	NeedMerge(vecFace& subdiv, Fbox& bb_base)
{
	// 1. Amount of polygons
	if (subdiv.size() >= u32(3 * c_SS_HighVertLimit / 4))
		return FALSE;

	Fvector sz_base;

	// 2. Bounding box
	bb_base.invalidate();
	CreateBox(subdiv, bb_base);

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
ICF BOOL	NeedMerge_for(vecFace& subdiv, Fbox bb_base)
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
 
	auto FunctionItem = [&](u32 Index)
	{
		auto& test = data_vector[Index];

		if (SelectedStart == test.face_id || test.merged)
			return;

		float Volume = flt_max;
		vecFace& TEST = *(g_XSplit[test.face_id]);
		vecFace* subdiv = (g_XSplit[SelectedStart]);

		if (!FaceEqual(subdiv->front(), TEST.front())) return;

#ifdef USE_PRECOMPUTED_BBOX
		Fbox box = test.bbox;
		if (!NeedMerge_for(TEST, box)) return;
#else 
		Fbox box;
		if (!NeedMerge(TEST, box)) return;
#endif

		if (!ValidateMerge(subdiv->size(), TEST.size(), Volume, box, bb_base, bb_base_orig)) return;

		csMerge.Enter();
		if (Volume < SelectedVolume)
		{
			CurrentProcessedID = test.face_id;
			VecIndex = Index;
			SelectedVolume = Volume;
		}
		csMerge.Leave();
	};


	if (data_vector.size() > 256)
	{
		xr_parallel_for(size_t(0), size_t(data_vector.size()), [&](size_t Index)
		{
			FunctionItem(Index);
		});
	}
	else
	{
		for (auto Index = 0; Index < data_vector.size(); Index++)
		{
			FunctionItem(Index);
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
 
void MergeCandidate(u32 GridMAX, bool use_grid)
{
	xr_concurrent_unordered_map<int, xr_vector<data_material>> thread_faces;
	
	// Generate Materials
	CTimer t; t.Start();
	for (int split = 0; split < g_XSplit.size(); split++)
	{
		Fbox bbox;
#ifdef  USE_PRECOMPUTED_BBOX
		for (auto F : *g_XSplit[split])
		{
			bbox.modify(F->v[0]->P);
			bbox.modify(F->v[1]->P);
			bbox.modify(F->v[2]->P);
		}
#endif
		thread_faces[g_XSplit[split]->front()->dwMaterial].push_back(data_material{ split, bbox, g_XSplit[split] });
	}

	Msg("Calcuation Matrials Splits Bboxes: %u ms", t.GetElapsed_ms());

	int IndexMap = 0;
	u32 TotalErased = 0;
	for (auto& MAP : thread_faces)
	{
		Progress( float (IndexMap) / float(thread_faces.size()) );
		IndexMap++;
 
#ifdef USE_GRID_SYSTEM
		// Строим Хэш мапу для ускорения поиска похожих сабдивов
		std::unordered_map<GridKey, xr_vector<data_material>, GridKey::Hash> grid_map;

		CTimer t; t.Start();

		u32 SplitSize = MAP.second.size();

		if (use_grid)
		{
			for (auto& data : MAP.second)
			{
				// Получаем Fbox и вычисляем ключ сетки
				int cell_x = static_cast<int>(std::floor(data.bbox.min.x / GridMAX));
				int cell_y = static_cast<int>(std::floor(data.bbox.min.z / GridMAX));

				grid_map[{cell_x, cell_y}].push_back(data);
			}
		}
		else 
		{
			for (auto& data : MAP.second)
				grid_map[{0, 0}].push_back(data);
		}

		u32 msElapsed = t.GetElapsed_ms();
		
		t.Start();

		int IDX_GRID = 0;
		for (auto& grid : grid_map)
		{
			
			IDX_GRID++;

			for (auto& S_MERGE_DATA : grid.second)
			{
				AditionalData("MP(%u/%u) SP(%u) GRID[%u](%u/%u) SP(%u)", IndexMap, thread_faces.size(), MAP.second.size(), GridMAX, IDX_GRID, grid_map.size(), grid.second.size());

				auto faceID = S_MERGE_DATA.face_id;
				if (g_XSplit[faceID]->empty() || S_MERGE_DATA.merged)
					continue;

				vecFace& subdiv = *(g_XSplit[faceID]);

				bool		bb_base_orig_inited = false;
				Fbox		bb_base_orig;
				Fbox		bb_base;
				while (NeedMerge(subdiv, bb_base))
				{
					//	Save original AABB for later tests
					if (!bb_base_orig_inited)
					{
						bb_base_orig_inited = true;
						bb_base_orig = bb_base;
					}

					//	Save original AABB for later tests
					u32	CurrentProcessedID = faceID;
					u32 VDataIndex = 0;
					// Merge-validate
					Validate(
						CurrentProcessedID,
						VDataIndex,
						S_MERGE_DATA,
						grid.second,
						bb_base, bb_base_orig
					);

					if (CurrentProcessedID == faceID)
						break;

					// **OK**. Perform merge					 
					if (g_XSplit[CurrentProcessedID])
					{
						subdiv.insert(subdiv.begin(), g_XSplit[CurrentProcessedID]->begin(), g_XSplit[CurrentProcessedID]->end());
						g_XSplit[CurrentProcessedID]->clear();
						grid.second[VDataIndex].merged = true;
						TotalErased++;
					}
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

		if (SplitSize > 16 * 1024)
		{
			Msg("Calculating grid[%u]: Splits[%u], Time: %u ms", IndexMap, SplitSize, msElapsed);
			Msg("Calculating MERGE: %u ms, Total Splits Calculated: %u", t.GetElapsed_ms(), TotalErased);
		}
#else 
 
		for (auto& S_MERGE_DATA : MAP.second)
		{
			AditionalData("MP(%u/%u) SP(%u)", IndexMap, thread_faces.size(), MAP.second.size());

			auto faceID = S_MERGE_DATA.face_id;
			if (g_XSplit[faceID]->empty() || S_MERGE_DATA.merged)
				continue;

			vecFace& subdiv = *(g_XSplit[faceID]);

			bool		bb_base_orig_inited = false;
			Fbox		bb_base_orig;
			Fbox		bb_base;
			while (NeedMerge(subdiv, bb_base))
			{
				//	Save original AABB for later tests
				if (!bb_base_orig_inited)
				{
					bb_base_orig_inited = true;
					bb_base_orig = bb_base;
				}

				//	Save original AABB for later tests
				u32	CurrentProcessedID = faceID;
				u32 VDataIndex = 0;
				// Merge-validate
				Validate(
					CurrentProcessedID,
					VDataIndex,
					S_MERGE_DATA,
					MAP.second,
					bb_base, bb_base_orig
				);

				if (CurrentProcessedID == faceID)
					break;

				// **OK**. Perform merge					 
				if (g_XSplit[CurrentProcessedID])
				{
					subdiv.insert(subdiv.begin(), g_XSplit[CurrentProcessedID]->begin(), g_XSplit[CurrentProcessedID]->end());
					g_XSplit[CurrentProcessedID]->clear();
					MAP.second[VDataIndex].merged = true;
					TotalErased++;
				}
			}

			MAP.second.erase(
				std::remove_if(
					MAP.second.begin(),
					MAP.second.end(),
					[&](data_material& vec)
					{ return vec.merged; }
				),
				MAP.second.end()
			);
		}
#endif 

	
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
// TBB Helpers

Fvector Center(const Fbox& bb)
{
	Fvector result;
	bb.getcenter(result);
	return result;
}

void RemoveEmptySplits()
{
	auto it = std::partition(g_XSplit.begin(), g_XSplit.end(), [](auto split) { return !split->empty(); });
	std::for_each(it, g_XSplit.end(), [](auto& split) { xr_delete(split); });
	g_XSplit.erase(it, g_XSplit.end());
}

// TBB Code
#include <string_view>
struct SplitKey
{
	CLightmap* lmapLayer;
	u32 tcSize;
	u16 material;
};

bool operator==(const SplitKey& l, const SplitKey& r)
{
	return l.material == r.material && l.tcSize == r.tcSize && l.lmapLayer == r.lmapLayer;
}
 
template <> struct std::hash<SplitKey> 
{
	std::size_t operator()(SplitKey const& key) const noexcept
	{
		return std::hash<std::string_view> {}(std::string_view((const char*)&key, sizeof(void*) + 6));
	};
};
struct Cell {
	int x, z;
	static Cell FromVector(const Fvector& p)
	{
		// const float cellSize = 4 * c_SS_maxsize / 3;
		const float cellSize = 256;
		return { static_cast<int>(p.x / cellSize), static_cast<int>(p.z / cellSize) };
	}
};
bool operator==(const Cell& l, const Cell& r) { return l.x == r.x && l.z == r.z; }

template <> struct std::hash<Cell> 
{

	std::size_t operator()(const Cell& key) const noexcept
	{
		return std::hash<std::string_view> {}(std::string_view((const char*)&key, 8));
	};
};
 
using Hash = std::unordered_map<Cell, xr_vector<u32>>;
struct SplitValue
{
	xr_vector<u32> splits;
	Hash hash;
	void merge(const SplitValue& other)
	{
		splits.insert(splits.end(), other.splits.begin(), other.splits.end());
		for (const auto& [k, v] : other.hash)
		{
			hash[k].insert(hash[k].end(), v.begin(), v.end());
		}
	}

	xr_vector<u32> GetCandidates(const Fvector& center, u32 min) const
	{
		xr_vector<u32> result;
		Cell p = Cell::FromVector(center);
		for (int dx = -1; dx <= 1; dx++) 
		{
			for (int dz = -1; dz <= 1; dz++) 
			{
				auto i = hash.find(Cell{ p.x + dx, p.z + dz });
				if (i != hash.end()) 
					std::copy_if(i->second.begin(), i->second.end(), std::back_inserter(result), [&](u32 id) { return id > min; });
			}
		}
		return result;
	}
};

using SplitMap = std::unordered_map<SplitKey, SplitValue>;
SplitKey CalcSplitKey(const vecFace* split)
{
	auto& face = split->front();
	return { face->lmap_layer, face->tc.size(), face->dwMaterial };
}

struct SplitInfo 
{
	Fbox bb;
	bool needMerge;
};
 
IC BOOL	ValidateMergeTBB(u32 f1, const Fbox& bb_base, const Fbox& bb_base_orig, u32 f2, const Fbox& bb, float& volume)
{
	// Polygons
	if ((f1 + f2) > u32(4 * c_SS_HighVertLimit / 3))		return FALSE;	// Don't exceed limits (4/3 max POLY)	

	// Size
	Fbox	merge;	merge.merge(bb_base, bb);
	Fvector sz;		merge.getsize(sz);
	Fvector orig1;	bb_base_orig.getsize(orig1);
	Fvector orig2;	bb.getsize(orig2);
	if (sz.x > (4 * c_SS_maxsize / 3))			return FALSE;	// Don't exceed limits (4/3 GEOM)
	if (sz.y > (4 * c_SS_maxsize / 3))			return FALSE;
	if (sz.z > (4 * c_SS_maxsize / 3))			return FALSE;

	if (!ValidateMergeLinearSize(sz, orig1, orig2, 0))	return FALSE;	// Don't exceed limits (4/3 GEOM)
	if (!ValidateMergeLinearSize(sz, orig1, orig2, 1))	return FALSE;
	if (!ValidateMergeLinearSize(sz, orig1, orig2, 2))	return FALSE;

	// Volume
	Fbox		bb0, bb1;
	MakeCube(bb0, bb_base);	
	MakeCube(bb1, bb);		

	float	v1 = bb0.getvolume();
	float	v2 = bb1.getvolume();
	volume = merge.getvolume(); // / Cuboid(merge);
	if (volume > 8 * (v1 + v2))				
		return FALSE;	// Don't merge too distant groups (8 vol)

	// OK
	return TRUE;
}

#include <tbb/combinable.h>
#include <tbb/parallel_for.h>
 

/* ForeserX чет сломал !!!!
void xrPhase_MergeGeometry_Tbb()
{
	xr_vector<SplitInfo> info(g_XSplit.size());

	tbb::combinable<SplitMap> tempMappings;
	size_t grain = _max(size_t(1), g_XSplit.size() / xr_max_concurrency() / 10);
	xr_parallel_for(0ull, g_XSplit.size(), grain, [&](size_t i)
		{
			auto& local = tempMappings.local();
			info[i].needMerge = NeedMerge(*g_XSplit[i], info[i].bb);
			auto& value = local[CalcSplitKey(g_XSplit[i])];
			value.splits.push_back(i);
			value.hash[Cell::FromVector(Center(info[i].bb))].push_back(i);
		});

	SplitMap mappings;
	tempMappings.combine_each([&mappings](const SplitMap& x)
		{
			for (const auto& [k, v] : x)
				mappings[k].merge(v);
		});

	xr_atomic_u32 progress = 0;
	xr_vector<SplitKey> keys;
	keys.reserve(mappings.size());
	std::transform(mappings.begin(), mappings.end(), std::back_inserter(keys), [](const auto& x) { return x.first; });

	size_t need_merge = 0;
	for (auto V : info)
	{
		if (V.needMerge)
			need_merge++;
	}

	clMsg("* Need merge size: %u", need_merge);
	xr_parallel_for(0ull, keys.size(), [&](size_t i)
		{
			const auto& key = keys[i];
			auto& value = mappings[key];
			std::sort(value.splits.begin(), value.splits.end());

			for (u32 split : value.splits)
			{
				if (info[split].needMerge)
				{
					auto& subdiv = *g_XSplit[split];
					Fbox bb_base_orig = info[split].bb;
					auto candidates = value.GetCandidates(Center(info[split].bb), split);

					while (info[split].needMerge)
					{
						u32 selected = split;
						float selected_volume = flt_max;
						xr_vector<u32> next;
						next.reserve(candidates.size());

						for (auto& test : candidates) {
							auto& TEST = *g_XSplit[test];
							float volume = 0.0f;

							if (!info[test].needMerge)
								continue;

							if (!ValidateMergeTBB(subdiv.size(), info[split].bb, bb_base_orig,
								TEST.size(), info[test].bb, volume))
								continue;

							next.push_back(test);
							if (volume < selected_volume)
							{
								selected = test;
								selected_volume = volume;
							}
						}

						// No candidates for merge
						if (selected == split)
							break;

						// Perform merge
						vecFace& SELECTED = *g_XSplit[selected];
						subdiv.insert(subdiv.end(), SELECTED.begin(), SELECTED.end());
						SELECTED.clear();
						info[selected].needMerge = false;
						info[split].needMerge = NeedMerge(subdiv, info[split].bb);
						candidates = std::move(next);
					}
				}
				progress.fetch_add(1);
				Progress((float)progress.load() / g_XSplit.size());
			}
		});
	RemoveEmptySplits();
}
*/


void xrPhase_MergeGeometry_Tbb()
{
	xr_vector<SplitInfo> info(g_XSplit.size());

	tbb::combinable<SplitMap> tempMappings;
	auto grain = _max(size_t(1), g_XSplit.size() / tbb::task_arena().max_concurrency() / 10);
	tbb::parallel_for(tbb::blocked_range<u32>(0, g_XSplit.size(), grain), [&](const auto& r) {
		auto& local = tempMappings.local();
		for (auto i = r.begin(); i != r.end(); i++)
		{
			info[i].needMerge = NeedMerge(*g_XSplit[i], info[i].bb);
			auto& value = local[CalcSplitKey(g_XSplit[i])];
			value.splits.push_back(i);
			value.hash[Cell::FromVector(Center(info[i].bb))].push_back(i);
		}
		});

	SplitMap mappings;
	tempMappings.combine_each([&mappings](const SplitMap& x)
		{
			for (const auto& [k, v] : x)
				mappings[k].merge(v);
		});

	std::atomic<u32> progress{ 0 };
	xr_vector<SplitKey> keys;
	keys.reserve(mappings.size());
	std::transform(mappings.begin(), mappings.end(), std::back_inserter(keys), [](const auto& x) { return x.first; });

	size_t need_merge = 0;
	for (auto V : info)
	{
		if (V.needMerge)
			need_merge++;
	}

	clMsg("* Need merge size: %u", need_merge);
	tbb::parallel_for(tbb::blocked_range<u32>(0, keys.size()), [&](const auto& r)
		{
			for (auto i = r.begin(); i != r.end(); i++) {
				const auto& key = keys[i];
				auto& value = mappings[key];
				std::sort(value.splits.begin(), value.splits.end());

				for (u32 split : value.splits)
				{
					if (info[split].needMerge)
					{
						auto& subdiv = *g_XSplit[split];
						Fbox bb_base_orig = info[split].bb;
						auto candidates = value.GetCandidates(Center(info[split].bb), split);
						while (info[split].needMerge)
						{
							u32 selected = split;
							float selected_volume = flt_max;
							xr_vector<u32> next;
							next.reserve(candidates.size());
							for (auto& test : candidates)
							{
								auto& TEST = *g_XSplit[test];
								float volume = 0.0f;
								if (!info[test].needMerge)
									continue;
								if (!ValidateMergeTBB(subdiv.size(), info[split].bb, bb_base_orig, TEST.size(), info[test].bb, volume))
									continue;
								next.push_back(test);
								if (volume < selected_volume) {
									selected = test;
									selected_volume = volume;
								}
							}
							if (selected == split)
								break; // No candidates for merge

							// **OK**. Perform merge
							auto& SELECTED = *g_XSplit[selected];
							subdiv.insert(subdiv.end(), SELECTED.begin(), SELECTED.end());
							SELECTED.clear();
							info[selected].needMerge = false;
							info[split].needMerge = NeedMerge(subdiv, info[split].bb);
							candidates = std::move(next);
						}
					}
					progress.fetch_add(1);
					Progress((float)progress.load() / g_XSplit.size());
				}
			}
		});


	RemoveEmptySplits();
}


void CBuild::xrPhase_MergeGeometry()
{
	string128 tmp;
	sprintf(tmp, "Merge Started... [%zu]", g_XSplit.size());
	clMsg(tmp);

	// MergeCandidate(4, true);					// Если сделать меньше грид сначало ближнее приклеит потом уже то что осталось поэтому и столько проходов  
	// MergeCandidate(8, true);					 
	// MergeCandidate(32, true);					 
	// MergeCandidate(64, true);					 
	// MergeCandidate(256, true);					 
	// MergeCandidate(512, true);
	// MergeCandidate(2048, true);
	// MergeCandidate(8192, false);
	// Без грида собрать что осталось 

	u32 Recalculated = 0;
	while (g_XSplit.size() != Recalculated)
	{
		Msg("Start Merging: %u", Recalculated);
		Recalculated = g_XSplit.size();
		xrPhase_MergeGeometry_Tbb();
	}

	// Проверяем на INFINITY
	validate_splits();

	AditionalData("Splits Merged [%u]", g_XSplit.size());
}