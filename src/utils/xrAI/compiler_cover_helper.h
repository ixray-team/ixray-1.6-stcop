#pragma once 
#include "compiler.h"

typedef float	Cover[4];


namespace CoverBuilder
{
	static bool valid_vertex_id(const u32& vertex_id)
	{
		return					(vertex_id != InvalidNode);
	}

	static bool cover(const vertex& v, u32 index0, u32 index1)
	{
		return					(
			valid_vertex_id(v.n[index0]) &&
			valid_vertex_id(g_nodes[v.n[index0]].n[index1])
			);
	}

	static bool critical_point(const vertex& v, u32 index, u32 index0, u32 index1)
	{
		return					(
			!valid_vertex_id(v.n[index]) &&
			(
				!valid_vertex_id(v.n[index0]) ||
				!valid_vertex_id(v.n[index1]) ||
				cover(v, index0, index) ||
				cover(v, index1, index)
				)
			);
	}

	static bool is_cover(const vertex& v)
	{
		return					(
			critical_point(v, 0, 1, 3) ||
			critical_point(v, 2, 1, 3) ||
			critical_point(v, 1, 0, 2) ||
			critical_point(v, 3, 0, 2)
			);
	}


	static bool vertex_in_direction(const u32& start_vertex_id, const u32& target_vertex_id)
	{
		const Fvector& finish_position = g_nodes[target_vertex_id].Pos;
		u32						cur_vertex_id = start_vertex_id, prev_vertex_id = u32(-1);
		Fbox2					box;
		Fvector2				identity, start, dest, dir;

		identity.x = identity.y = g_params.fPatchSize * .5f;
		const Fvector& start_position = g_nodes[start_vertex_id].Pos;
		start = Fvector2().set(start_position.x, start_position.z);
		dest.set(finish_position.x, finish_position.z);
		dir.sub(dest, start);
		Fvector2				temp;
		temp = start;

		float					cur_sqr = _sqr(temp.x - dest.x) + _sqr(temp.y - dest.y);
		for (;;) {
			bool				found = false;
			for (int I = 0, E = 4; I != E; ++I) {
				u32				next_vertex_id = g_nodes[cur_vertex_id].n[I];
				if ((next_vertex_id == prev_vertex_id) || !valid_vertex_id(next_vertex_id))
					continue;

				const Fvector& position = g_nodes[next_vertex_id].Pos;
				temp = Fvector2().set(position.x, position.z);
				box.min = box.max = temp;
				box.grow(identity);
				if (box.pick_exact(start, dir)) {
					if (next_vertex_id == target_vertex_id)
						return		(true);

					Fvector2		temp_;
					temp_.add(box.min, box.max);
					temp_.mul(.5f);
					float			dist = _sqr(temp_.x - dest.x) + _sqr(temp_.y - dest.y);
					if (dist > cur_sqr)
						continue;

					cur_sqr = dist;
					found = true;
					prev_vertex_id = cur_vertex_id;
					cur_vertex_id = next_vertex_id;
					break;
				}
			}

			if (!found)
				return			(false);
		}
	}


	static int	calcSphereSector(Fvector& dir)
	{
		Fvector2			flat;

		// flatten
		flat.set(dir.x, dir.z);
		flat.norm();

		// analyze
		if (std::abs(flat.x) > std::abs(flat.y))
		{
			// sector 0,7,3,4
			if (flat.x < 0) {
				// sector 3,4
				if (flat.y > 0)	return 3;
				else			return 4;
			}
			else {
				// sector 0,7
				if (flat.y > 0)	return 0;
				else			return 7;
			}
		}
		else {
			// sector 1,2,6,5
			if (flat.x < 0) {
				// sector 2,5
				if (flat.y > 0)	return 2;
				else			return 5;
			}
			else {
				// sector 1,6
				if (flat.y > 0)	return 1;
				else			return 6;
			}
		}
	}


 	class			Query
	{
	public:
		xr_vector<u32>		q_List;
		xr_vector<u32>		q_Clear;
		Marks		q_Marks;
		Fvector		q_Base;

		IC void		Begin(int count)
		{
			q_List.reserve(8192);
			q_Clear.reserve(8192);
			q_Marks.assign(count, false);
		}

		IC void		Init(Fvector& P)
		{
			q_Base.set(P);
			q_List.clear();
			q_Clear.clear();
		}

		IC void		Perform(u32 ID)
		{
			if (ID == InvalidNode)		return;
			if (ID >= q_Marks.size())		return;
			if (q_Marks[ID])			return;

			q_Marks[ID] = true;
			q_Clear.push_back(ID);

			vertex& N = g_nodes[ID];
			if (q_Base.distance_to_sqr(N.Pos) > cover_sqr_dist)	return;

			// ok
			q_List.push_back(ID);

			Perform(N.n1);
			Perform(N.n2);
			Perform(N.n3);
			Perform(N.n4);
		}

		IC void		Clear()
		{
			for (auto it = q_Clear.begin(); it != q_Clear.end(); it++)
				q_Marks[*it] = false;
		}
	};
};