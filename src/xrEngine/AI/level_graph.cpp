#include "stdafx.h"
#include "level_graph.h"
#include "Stats.h"
ILevelGraph::ILevelGraph(): m_header(nullptr), m_nodes(nullptr), m_level_id(0), m_row_length(0), m_column_length(0),
                            m_max_x(0), m_max_z(0)
{
}

ILevelGraph::~ILevelGraph()
{
}

bool ILevelGraph::Search(u32 start_vertex_id, u32 dest_vertex_id, xr_vector<u32>& OutPath,float MaxRange, u32 MaxIterationCount, u32 MaxVisitedNodeCount) const
{
	thread_local	xr_vector<std::pair<float, u32>>	TempPriorityNode;
	thread_local	xr_hash_map<u32, u32>				TempCameFrom;
	thread_local	xr_hash_map<u32, float>				TempCostSoFar;
					float								m_distance_xz = header().cell_size();

	TempPriorityNode.clear();
	TempCameFrom.clear();
	TempCostSoFar.clear();
	OutPath.clear();
	
	
	u32 FromID = start_vertex_id;
	u32 ToID = dest_vertex_id;
		
	if (FromID == ToID)
	{
		OutPath.push_back(start_vertex_id);
		return true;
	}
		
	if(!is_accessible(FromID) || !is_accessible(ToID))
	{
		return false;
	}

	TempPriorityNode.push_back({0.f, FromID});
	TempCameFrom.insert({FromID, FromID});
	TempCostSoFar.insert( {FromID, 0.f});

	auto CalcCost = [m_distance_xz](CVertex* Node1,CVertex* Node2)
	{
		return m_distance_xz;
	};
	auto DistanceNode = [this,m_distance_xz](CVertex* Node1,CVertex* Node2)
	{
		float x1; float y1;
		float x2; float y2;
		unpack_xz(Node1,x1,y1);
		unpack_xz(Node2,x2,y2);
		return m_distance_xz*2*(fabs(x1-x2)+fabs(y1-y2));
	};
	while (!TempPriorityNode.empty())
	{
		u32 CurrentNodeID = TempPriorityNode.back().second;
		TempPriorityNode.pop_back();
		if (CurrentNodeID == ToID)
		{
			u32 NextNode = ToID;
			while (NextNode != FromID)
			{
				OutPath.insert( OutPath.begin(),NextNode);
				NextNode = TempCameFrom[NextNode];
			}
			OutPath.insert( OutPath.begin(),NextNode);
			return true;
		}
		
		CVertex* Node = vertex(CurrentNodeID);
		for (s32 NeighborIndex = 0; NeighborIndex < 4; NeighborIndex++)
		{
			u32 NeighborID = Node->link(NeighborIndex);
			if (!is_accessible(NeighborID)) continue;


			if(MaxIterationCount == 0) continue;
			MaxIterationCount--;

			CVertex* Neighbor = vertex(NeighborID);
			float NewCost = TempCostSoFar[CurrentNodeID] + CalcCost(Node, Neighbor);
			auto TempCostSoFarIterator = TempCostSoFar.find(NeighborID);
			if ((TempCostSoFarIterator != TempCostSoFar.end() &&TempCostSoFarIterator->second > NewCost)|| (TempCostSoFarIterator == TempCostSoFar.end() &&MaxVisitedNodeCount > TempCostSoFar.size()))
			{
				const float Distance = DistanceNode(vertex(ToID), Neighbor);

				if(Distance>MaxRange)
				{
					continue;
				}

				if(TempCostSoFarIterator != TempCostSoFar.end())
				{
					TempCostSoFarIterator->second = NewCost; 
				}
				else
				{
					TempCostSoFar.insert({NeighborID,NewCost});
				}

				float  priority = NewCost + Distance;
				TempPriorityNode.insert(std::upper_bound(TempPriorityNode.begin(),TempPriorityNode.end(),std::pair<float, u32>{priority,NeighborID},[](const std::pair<float, u32>& Left, const std::pair<float, u32>& Right) {return Left.first > Right.first; }),{priority,NeighborID});

				
				auto TempCameFromIterator = TempCameFrom.find(NeighborID);
				if(TempCameFromIterator!=TempCameFrom.end())
				{
					TempCameFromIterator->second = CurrentNodeID; 
				}
				else
				{
					TempCameFrom.insert({NeighborID,CurrentNodeID});
				}
			}
		}
	}
	return false;
}

u32 ILevelGraph::SearchNearestVertex(u32 VertexID, const Fvector& TargetPosition, float Range) const
{
	thread_local	xr_vector<std::pair<float, u32>>	TempPriorityNode;
	thread_local	xr_map<u32, u32>					TempCameFrom;
	thread_local	xr_map<u32, float>					TempCostSoFar;
					float								m_distance_xz			= header().cell_size();

	float BestDistanceToTarget = flt_max;

	TempPriorityNode.clear();
	TempCameFrom.clear();
	TempCostSoFar.clear();

	u32 FromID = VertexID;
	u32 BestResult = VertexID;

	u32 x0,y0;
	unpack_xz(vertex(VertexID),x0,y0);
	
	int MaxRangeSqr = iFloor(_sqr(Range)/ _sqr(m_distance_xz) + .5f);

	TempPriorityNode.push_back({0.f, FromID});
	TempCameFrom.insert({FromID, FromID});
	TempCostSoFar.insert( {FromID, 0.f });

	auto CalcCost = [m_distance_xz](CVertex* Node1,CVertex* Node2)
	{
		return m_distance_xz;
	};
	auto IsAccessible = [this,x0,y0,MaxRangeSqr](u32 NodeID)
	{
		if(!is_accessible(NodeID))
		{
			return false;
		}
		int x4,y4;
		unpack_xz(vertex(NodeID),x4,y4);
		return (static_cast<u32>(_sqr(x0 - x4) + _sqr(y0 - y4)) <= MaxRangeSqr);
	};

	while (!TempPriorityNode.empty())
	{
		u32 CurrentNodeID = TempPriorityNode.back().second;
		TempPriorityNode.pop_back();

		{
			float current_distance = TargetPosition.distance_to_xz_sqr(vertex_position(CurrentNodeID));
			if (current_distance < BestDistanceToTarget) 
			{
				BestDistanceToTarget	= current_distance;
				BestResult = CurrentNodeID;
			}
		}
		
		CVertex* Node = vertex(CurrentNodeID);
		for (s32 NeighborIndex = 0; NeighborIndex < 4; NeighborIndex++)
		{
			u32 NeighborID = Node->link(NeighborIndex);
			if (!IsAccessible(NeighborID)) continue;


			CVertex* Neighbor = vertex(NeighborID);
			float NewCost = TempCostSoFar[CurrentNodeID] + CalcCost(Node, Neighbor);
			auto TempCostSoFarIterator = TempCostSoFar.find(NeighborID);
			if ((TempCostSoFarIterator != TempCostSoFar.end() &&TempCostSoFarIterator->second > NewCost)|| (TempCostSoFarIterator == TempCostSoFar.end()))
			{

				if(TempCostSoFarIterator != TempCostSoFar.end())
				{
					TempCostSoFarIterator->second = NewCost; 
				}
				else
				{
					TempCostSoFar.insert({NeighborID,NewCost});
				}

				float  priority = NewCost;
				TempPriorityNode.insert(std::upper_bound(TempPriorityNode.begin(),TempPriorityNode.end(),std::pair<float, u32>{priority,NeighborID},[](const std::pair<float, u32>& Left, const std::pair<float, u32>& Right) {return Left.first > Right.first; }),{priority,NeighborID});

				
				auto TempCameFromIterator = TempCameFrom.find(NeighborID);
				if(TempCameFromIterator!=TempCameFrom.end())
				{
					TempCameFromIterator->second = CurrentNodeID; 
				}
				else
				{
					TempCameFrom.insert({NeighborID,CurrentNodeID});
				}
			}
		}
	}
	return BestResult;
}

u32	ILevelGraph::vertex(const Fvector& position) const
{
	ILevelGraph::CPosition	_node_position;
	vertex_position(_node_position, position);
	float					min_dist = flt_max;
	u32						selected;
	set_invalid_vertex(selected);
	for (u32 i = 0; i < header().vertex_count(); ++i) {
		float				dist = distance(i, position);
		if (dist < min_dist) {
			min_dist = dist;
			selected = i;
		}
	}

	VERIFY(valid_vertex_id(selected));
	return					(selected);
}

u32 ILevelGraph::vertex(u32 current_node_id, const Fvector& position) const
{
#ifndef AI_COMPILER
	if (DevicePtr)
	{
		Device.Statistic->AI_Node.Begin();
	}
#endif

	u32						id;

	if (valid_vertex_position(position)) {
		// so, our position is inside the level graph bounding box
		if (valid_vertex_id(current_node_id) && inside(vertex(current_node_id), position)) {
			// so, our node corresponds to the position
#ifndef AI_COMPILER
			if (DevicePtr)
			{
				Device.Statistic->AI_Node.End();
			}
#endif
			return				(current_node_id);
		}

		// so, our node doesn't correspond to the position
		// try to search it with O(logN) time algorithm
		u32						_vertex_id = vertex_id(position);
		if (valid_vertex_id(_vertex_id)) {
			// so, there is a node which corresponds with x and z to the position
			bool				ok = true;
			if (valid_vertex_id(current_node_id)) {
				{
					CVertex const& vertex = *this->vertex(current_node_id);
					for (u32 i = 0; i < 4; ++i) {
						if (vertex.link(i) == _vertex_id) {
#ifndef AI_COMPILER
							if (DevicePtr)
							{
								Device.Statistic->AI_Node.End();
							}
#endif // AI_COMPILER
							return			(_vertex_id);
						}
					}
				}
				{
					CVertex const& vertex = *this->vertex(_vertex_id);
					for (u32 i = 0; i < 4; ++i) {
						if (vertex.link(i) == current_node_id) {
#ifndef AI_COMPILER
							if (DevicePtr)
							{
								Device.Statistic->AI_Node.End();
							}
#endif // AI_COMPILER
							return			(_vertex_id);
						}
					}
				}

				float				y0 = vertex_plane_y(current_node_id, position.x, position.z);
				float				y1 = vertex_plane_y(_vertex_id, position.x, position.z);
				bool				over0 = position.y > y0;
				bool				over1 = position.y > y1;
				float				y_dist0 = position.y - y0;
				float				y_dist1 = position.y - y1;
				if (over0) {
					if (over1) {
						if (y_dist1 - y_dist0 > 1.f)
							ok = false;
						else
							ok = true;
					}
					else {
						if (y_dist0 - y_dist1 > 1.f)
							ok = false;
						else
							ok = true;
					}
				}
				else {
					ok = true;
				}
			}
			if (ok) {
#ifndef AI_COMPILER
				if (DevicePtr)
				{
					Device.Statistic->AI_Node.End();
				}
#endif
				return			(_vertex_id);
			}
		}
	}

	if (!valid_vertex_id(current_node_id)) {
		// so, we do not have a correct current node
		// performing very slow full search
		id = vertex(position);
		VERIFY(valid_vertex_id(id));
#ifndef AI_COMPILER
		if (DevicePtr)
		{
			Device.Statistic->AI_Node.End();
		}
#endif
		return				(id);
	}

	u32					new_vertex_id = guess_vertex_id(current_node_id, position);
	if (new_vertex_id != current_node_id)
		return			(new_vertex_id);

	// so, our position is outside the level graph bounding box
	// or
	// there is no node for the current position
	// try to search the nearest one iteratively
	SContour			_contour;
	Fvector				point;
	u32					best_vertex_id = current_node_id;
	contour(_contour, current_node_id);
	nearest(point, position, _contour);
	float				best_distance_sqr = position.distance_to_sqr(point);
	const_iterator		i, e;
	begin(current_node_id, i, e);
	for (; i != e; ++i) {
		u32				level_vertex_id = value(current_node_id, i);
		if (!valid_vertex_id(level_vertex_id))
			continue;

		contour(_contour, level_vertex_id);
		nearest(point, position, _contour);
		float			distance_sqr = position.distance_to_sqr(point);
		if (best_distance_sqr > distance_sqr) {
			best_distance_sqr = distance_sqr;
			best_vertex_id = level_vertex_id;
		}
	}

#ifndef AI_COMPILER
	if (DevicePtr)
	{
		Device.Statistic->AI_Node.End();
	}
#endif
	return					(best_vertex_id);

}

u32	ILevelGraph::vertex_id(const Fvector& position) const
{
	VERIFY2(valid_vertex_position(position),make_string<const char*>("invalid position for ILevelGraph::vertex_id specified: [%f][%f][%f]",	VPUSH(position)));

	CPosition			_vertex_position = vertex_position(position);
	CVertex* B = m_nodes;
	CVertex* E = m_nodes + header().vertex_count();
	CVertex* I = std::lower_bound(B, E, _vertex_position.xz());
	if ((I == E) || ((*I).position().xz() != _vertex_position.xz()))
		return			(u32(-1));

	u32					best_vertex_id = u32(I - B);
	float				y = vertex_plane_y(best_vertex_id, position.x, position.z);
	for (++I; I != E; ++I) {
		if ((*I).position().xz() != _vertex_position.xz())
			break;

		u32				new_vertex_id = u32(I - B);
		float			_y = vertex_plane_y(new_vertex_id, position.x, position.z);
		if (y <= position.y) {
			// so, current node is under the specified position
			if (_y <= position.y) {
				// so, new node is under the specified position
				if (position.y - _y < position.y - y) {
					// so, new node is closer to the specified position
					y = _y;
					best_vertex_id = new_vertex_id;
				}
			}
		}
		else
			// so, current node is over the specified position
			if (_y <= position.y) {
				// so, new node is under the specified position
				y = _y;
				best_vertex_id = new_vertex_id;
			}
			else
				// so, new node is over the specified position
				if (_y - position.y < y - position.y) {
					// so, new node is closer to the specified position
					y = _y;
					best_vertex_id = new_vertex_id;
				}
	}

	return			(best_vertex_id);
}

static const int max_guess_vertex_count = 4;

u32 ILevelGraph::guess_vertex_id(u32 const& current_vertex_id, Fvector const& position) const
{
	VERIFY(valid_vertex_id(current_vertex_id));

	CPosition				vertex_position;
	if (valid_vertex_position(position))
		vertex_position = this->vertex_position(position);
	else
		vertex_position = vertex(current_vertex_id)->position();

	u32						x, z;
	unpack_xz(vertex_position, x, z);

	SContour				vertex_contour;
	contour(vertex_contour, current_vertex_id);
	Fvector					best_point;
	float					result_distance = nearest(best_point, position, vertex_contour);
	u32						result_vertex_id = current_vertex_id;

	CVertex const* B = m_nodes;
	CVertex const* E = m_nodes + header().vertex_count();
	u32						start_x = (u32)_max(0, int(x) - max_guess_vertex_count);
	u32						stop_x = _min(max_x(), x + (u32)max_guess_vertex_count);
	u32						start_z = (u32)_max(0, int(z) - max_guess_vertex_count);
	u32						stop_z = _min(max_z(), z + (u32)max_guess_vertex_count);
	for (u32 i = start_x; i <= stop_x; ++i) {
		for (u32 j = start_z; j <= stop_z; ++j) {
			u32				test_xz = i * m_row_length + j;
			CVertex const* I = std::lower_bound(B, E, test_xz);
			if (I == E)
				continue;

			if ((*I).position().xz() != test_xz)
				continue;

			u32				best_vertex_id = u32(I - B);
			contour(vertex_contour, best_vertex_id);
			float			best_distance = nearest(best_point, position, vertex_contour);
			for (++I; I != E; ++I) {
				if ((*I).position().xz() != test_xz)
					break;

				u32				vertex_id = u32(I - B);
				Fvector			point;
				contour(vertex_contour, vertex_id);
				float			distance = nearest(point, position, vertex_contour);
				if (distance >= best_distance)
					continue;

				best_point = point;
				best_distance = distance;
				best_vertex_id = vertex_id;
			}

			if (_abs(best_point.y - position.y) >= 3.f)
				continue;

			if (result_distance <= best_distance)
				continue;

			result_distance = best_distance;
			result_vertex_id = best_vertex_id;
		}
	}

	return					(result_vertex_id);
}
