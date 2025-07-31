#include "StdAfx.h"
#include "../xrLC_Light/Lightmap.h"
#include "Build.h"
#include "../xrLC_Light/xrDeflector.h"

// TBB Helpers
extern ICF BOOL	ValidateMergeLinearSize(const Fvector& merged, const Fvector& orig1, const Fvector& orig2, int iAxis);
extern ICF void	MakeCube(Fbox& BB_dest, const Fbox& BB_src);
extern ICF BOOL	NeedMerge(vecFace& subdiv, Fbox& bb_base);

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

// Grids TBB

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

template <> 
struct std::hash<SplitKey>
{
	std::size_t operator()(SplitKey const& key) const noexcept
	{
		return std::hash<std::string_view> {}(std::string_view((const char*)&key, sizeof(void*) + 6));
	};
};


struct Cell
{
	int x, z;
	static Cell FromVector(const Fvector& p)
	{
		// const float cellSize = 4 * c_SS_maxsize / 3;
		const float cellSize = 256;
		return { static_cast<int>(p.x / cellSize), static_cast<int>(p.z / cellSize) };
	}
};
bool operator==(const Cell& l, const Cell& r) { return l.x == r.x && l.z == r.z; }

template <> 
struct std::hash<Cell>
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
 
#include <tbb/combinable.h>
#include <tbb/parallel_for.h>

// ForserX Не трогай то что и так работает :)
void xrPhase_MergeGeometry_Tbb()
{
	xr_vector<SplitInfo> info(g_XSplit.size());

	tbb::combinable<SplitMap> tempMappings;
	auto grain = _max(size_t(1), g_XSplit.size() / tbb::task_arena().max_concurrency() / 10);
	tbb::parallel_for(tbb::blocked_range<u32>(0, g_XSplit.size(), grain), [&](const auto& r)
	{
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
		for (auto i = r.begin(); i != r.end(); i++)
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
