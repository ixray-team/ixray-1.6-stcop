#include "StdAfx.h"
#include "compiler_cover_helper.h"
#include "compiler.h"
#include "../../xrGame/quadtree.h"
#include "cover_point.h"
#include "object_broker.h"

extern float	CalculateHeight(Fbox& BB);
typedef CQuadTree<CCoverPoint> CPointQuadTree;
static CPointQuadTree* g_covers = 0;

xr_vector<bool> g_cover_nodes;
 
void compute_cover_nodes()
{
	Fbox					aabb;
	CalculateHeight(aabb);
	VERIFY(!g_covers);
	g_covers = new CPointQuadTree(aabb, g_params.fPatchSize * .5f, 8 * 65536, 4 * 65536);

	g_cover_nodes.assign(g_nodes.size(), false);

	Nodes::const_iterator	B = g_nodes.begin(), I = B;
	Nodes::const_iterator	E = g_nodes.end();
	auto	J = g_cover_nodes.begin();
	for (; I != E; ++I, ++J) {
		if (!CoverBuilder::is_cover(*I)) continue;

		*J = true;
		g_covers->insert(new CCoverPoint((*I).Pos, u32(I - B)));
	}
}

void compute_non_covers()
{
	VERIFY(g_covers);

	xr_vector<CCoverPoint*> nearest;

	{
		g_covers->all(nearest);
		delete_data(nearest);
		xr_delete(g_covers);

		Fbox					aabb;
		CalculateHeight(aabb);
		VERIFY(!g_covers);
		g_covers = new CPointQuadTree(aabb, g_params.fPatchSize * .5f, 8 * 65536, 4 * 65536);

		Nodes::iterator			B = g_nodes.begin(), I = B;
		Nodes::iterator			E = g_nodes.end();
		auto	J = g_cover_nodes.begin();
		for (; I != E; ++I, ++J) {
			if (!*J)
				continue;

			if (((*I).high_cover[0] + (*I).high_cover[1] + (*I).high_cover[2] + (*I).high_cover[3]) >= 4 * .999f) {
				if (((*I).low_cover[0] + (*I).low_cover[1] + (*I).low_cover[2] + (*I).low_cover[3]) >= 4 * .999f)
					continue;
			}

			g_covers->insert(new CCoverPoint((*I).Pos, u32(I - B)));
		}

		VERIFY(g_covers->size());
	}

	typedef std::pair<float, CCoverPoint*>	COVER_PAIR;
 	xr_vector<COVER_PAIR>				cover_pairs;

	Nodes::iterator			B = g_nodes.begin(), I = B;
	Nodes::iterator			E = g_nodes.end();
	auto					J = g_cover_nodes.begin();
	for (; I != E; ++I, ++J) {
		if (*J) continue;

		g_covers->nearest((*I).Pos, cover_distance, nearest);
		if (nearest.empty()) {
			for (int i = 0; i < 4; ++i) {
				VERIFY((*I).high_cover[i] == flt_max);
				(*I).high_cover[i] = 1.f;

				VERIFY((*I).low_cover[i] == flt_max);
				(*I).low_cover[i] = 1.f;
			}
			continue;
		}

		cover_pairs.clear();
		cover_pairs.reserve(nearest.size());

		float				cumulative_weight = 0.f;
		{
 			for (auto O : nearest) {
				if (!CoverBuilder::vertex_in_direction(u32(I - B), O->level_vertex_id()))
					continue;

				float					weight = 1.f / O->position().distance_to((*I).Pos);
				cumulative_weight += weight;
				cover_pairs.push_back(
					std::make_pair(
						weight,
						O
					)
				);
			}
		}

		// this is incorrect
		if (cover_pairs.empty()) {
			for (int i = 0; i < 4; ++i) {
				VERIFY((*I).high_cover[i] == flt_max);
				(*I).high_cover[i] = 1.f;

				VERIFY((*I).low_cover[i] == flt_max);
				(*I).low_cover[i] = 1.f;
			}
			continue;
		}

		for (int j = 0; j < 4; ++j) {
			VERIFY((*I).high_cover[j] == flt_max);
			(*I).high_cover[j] = 0.f;

			VERIFY((*I).low_cover[j] == flt_max);
			(*I).low_cover[j] = 0.f;
		}

		auto i = cover_pairs.begin();
		auto e = cover_pairs.end();
		for (; i != e; ++i) {
			vertex& current = g_nodes[(*i).second->level_vertex_id()];
			float						factor = (*i).first / cumulative_weight;
			for (int j = 0; j < 4; ++j) {
				(*I).high_cover[j] += factor * current.high_cover[j];
				(*I).low_cover[j] += factor * current.low_cover[j];
			}
		}

		for (int i_ = 0; i_ < 4; ++i_) {
			clamp((*I).high_cover[i_], 0.f, 1.f);
			clamp((*I).low_cover[i_], 0.f, 1.f);
		}
	}


	VERIFY(g_covers);
	g_covers->all(nearest);
	delete_data(nearest);
	xr_delete(g_covers);
}