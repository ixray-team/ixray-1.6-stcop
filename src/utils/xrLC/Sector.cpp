#include "stdafx.h"
#include "build.h"
#include "Sector.h"
#include "OGF_Face.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CSector::CSector(u32 ID)
{
	SelfID = ID;
	TreeRoot = 0;
}

CSector::~CSector()
{

}

IC BOOL	ValidateMerge(Fbox& bb_base, Fbox& bb, float& volume, float SLimit)
{
	// Size
	Fbox	merge;	merge.merge(bb_base, bb);
	Fvector sz;		merge.getsize(sz);	sz.add(EPS_L);
	if (sz.x > SLimit)		return FALSE;	// Don't exceed limits (4/3 GEOM)
	if (sz.y > SLimit)		return FALSE;
	if (sz.z > SLimit)		return FALSE;

	// Volume
	volume = merge.getvolume();

	// OK
	return TRUE;
}

struct GridKey
{
	int x, y, z;

	bool operator==(const GridKey& o) const
	{
		return x == o.x && y == o.y && z == o.z;
	}
};

struct GridKeyHash
{
	size_t operator()(const GridKey& k) const
	{
		return (size_t)k.x * 73856093 ^
			(size_t)k.y * 19349663 ^
			(size_t)k.z * 83492791;
	}
};

using Bucket = std::vector<int>;
using GridMap = std::unordered_map<GridKey, Bucket, GridKeyHash>;

static inline GridKey GetKey(const Fbox& b, float cellSize)
{
	Fvector c;
	b.getcenter(c);

	return {
		(int)floorf(c.x / cellSize),
		(int)floorf(c.y / cellSize),
		(int)floorf(c.z / cellSize)
	};
}

static inline void GatherNeighbors(
	const GridKey& k,
	const GridMap& grid,
	std::vector<int>& out)
{
	for (int dx = -1; dx <= 1; dx++)
		for (int dy = -1; dy <= 1; dy++)
			for (int dz = -1; dz <= 1; dz++)
			{
				GridKey nk{ k.x + dx, k.y + dy, k.z + dz };

				auto it = grid.find(nk);
				if (it == grid.end()) continue;

				const Bucket& b = it->second;
				out.insert(out.end(), b.begin(), b.end());
			}
}

void CSector::BuildHierrarhy()
{
	Fvector		scene_size;
	float		delimiter;
	BOOL		bAnyNode = FALSE;

	// calc scene BB
	Fbox& scene_bb = pBuild->scene_bb;
	scene_bb.invalidate();
	for (int I = 0; I < s32(g_tree.size()); I++)
	{
		auto elem = g_tree[I];
		IVERIFY(elem->bbox.is_valid());
		// If any of these triggers - might be something wrong with AABB
		IVERIFY(elem->bbox.min.x > -1000000.0f && elem->bbox.min.y > -1000000.0f && elem->bbox.min.z > -1000000.0f);
		IVERIFY(elem->bbox.max.x < 1000000.0f && elem->bbox.max.y < 1000000.0f && elem->bbox.max.z < 1000000.0f);
		scene_bb.merge(g_tree[I]->bbox);
	}
	scene_bb.grow(EPS_L);

	// 
	scene_bb.getsize(scene_size);
	delimiter = std::max(scene_size.x, std::max(scene_size.y, scene_size.z));
	delimiter *= 2;

	int iLevel = 2;
	float SizeLimit = c_SS_maxsize / 4.f;

	if (SizeLimit < 4.f)
		SizeLimit = 4.f;

	if (delimiter <= SizeLimit)
		delimiter *= 2;

	// ================================
	// MAIN LOOP (your logic)
	// ================================
	for (; SizeLimit <= delimiter; SizeLimit *= 2)
	{
		int iSize = (int)g_tree.size();

		// ================================
		// GRID BUILD
		// ================================
		GridMap grid;
		float cellSize = SizeLimit * 0.5f;

		for (int i = 0; i < g_tree.size(); i++)
		{
			if (g_tree[i]->bConnected) continue;
			if (g_tree[i]->Sector != SelfID) continue;

			GridKey key = GetKey(g_tree[i]->bbox, cellSize);
			grid[key].push_back(i);
		}

		for (int I = 0; I < iSize; I++)
		{
			if (g_tree[I]->bConnected) continue;
			if (g_tree[I]->Sector != SelfID) continue;

			OGF_Node* pNode = new OGF_Node(iLevel, u16(SelfID));
			pNode->AddChield(I);

			AditionalData("Capturing[%.0f/%.0f] SectorsBest: %d/%d", SizeLimit, delimiter, I, iSize);

			for (;;)
			{
				int best_id = -1;
				float best_volume = flt_max;

				std::vector<int> candidates;
				candidates.reserve(64);

				GridKey baseKey = GetKey(pNode->bbox, cellSize);
				GatherNeighbors(baseKey, grid, candidates);

				for (int k = 0; k < candidates.size(); k++)
				{
					int J = candidates[k];

					OGF_Base* candidate = g_tree[J];

					if (candidate->bConnected) continue;
					if (candidate->Sector != SelfID) continue;

					float V;
					if (ValidateMerge(pNode->bbox, candidate->bbox, V, SizeLimit))
					{
						if (V < best_volume)
						{
							best_volume = V;
							best_id = J;
						}
					}
				}

				if (best_id < 0)
					break;

				pNode->AddChield(best_id);
				g_tree[best_id]->bConnected = true;
			}

			if (pNode->chields.size() > 1)
			{
				pNode->CalcBounds();
				g_tree.push_back(pNode);
				bAnyNode = true;
			}
			else
			{
				g_tree[I]->bConnected = false;
				xr_delete(pNode);
			}
		}

		if (iSize != (int)g_tree.size())
			iLevel++;
	}


	TreeRoot = 0;
	if (bAnyNode)
		TreeRoot = g_tree.back();
	else {
		for (u32 I = 0; I < g_tree.size(); I++)
		{
			if (g_tree[I]->bConnected)		 continue;
			if (g_tree[I]->Sector != SelfID) continue;
			R_ASSERT(0 == TreeRoot);
			TreeRoot = g_tree[I];
		}
	}
	if (0 == TreeRoot) {
		clMsg("Can't build hierrarhy for sector #%d", SelfID);
	}
}

void CSector::Validate()
{
	std::sort(Portals.begin(), Portals.end());
	R_ASSERT(std::unique(Portals.begin(), Portals.end()) == Portals.end());
	R_ASSERT(TreeRoot);
	R_ASSERT(TreeRoot->Sector == SelfID);
}

void CSector::Save(IWriter& fs)
{
	// Root
	xr_vector<OGF_Base*>::iterator F = std::find(g_tree.begin(), g_tree.end(), TreeRoot);
	R_ASSERT(F != g_tree.end());
	u32 ID = u32(F - g_tree.begin());
	fs.w_chunk(fsP_Root, &ID, sizeof(u32));

	// Portals
	fs.w_chunk(fsP_Portals, &*Portals.begin(), Portals.size() * sizeof(u16));
}
