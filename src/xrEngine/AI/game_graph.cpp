#include "stdafx.h"
#include "game_graph.h"
IGameGraph::IGameGraph()
{
}
IGameGraph::~IGameGraph()
{
}
void IGameGraph::save(IWriter& stream)
{
	m_header.save(&stream);

	u8* buffer = (u8*)m_nodes;
	stream.w(buffer, header().vertex_count() * sizeof(CVertex));
	buffer += header().vertex_count() * sizeof(CVertex);

	stream.w(buffer, header().edge_count() * sizeof(IGameGraph::CEdge));
	buffer += header().edge_count() * sizeof(IGameGraph::CEdge);

	stream.w(buffer, header().death_point_count() * sizeof(CLevelPoint));
	buffer += header().death_point_count() * sizeof(CLevelPoint);

	VERIFY((u8*)m_cross_tables == buffer);

	GameGraph::LEVEL_MAP::const_iterator	I = header().levels().begin();
	GameGraph::LEVEL_MAP::const_iterator	E = header().levels().end();
	for (; I != E; ++I) {
		u32						size = *(u32*)buffer;
		stream.w(buffer, size);
		buffer += size;
	}
}

bool IGameGraph::Search(u32 start_vertex_id, u32 dest_vertex_id, xr_vector<u32>& OutPath,const xr_vector<GameGraph::STerrainPlace>* VertexTypes, float MaxRange, u32 MaxIterationCount,u32 MaxVisitedNodeCount) const
{
	bool StartIsAccessible = true;
	auto IsAccessible = [this,&StartIsAccessible,VertexTypes](u32 VertexID)
	{
		if(!accessible(VertexID))
		{
			return false;
		}

		if (!StartIsAccessible)
		{
			return true;
		}

#ifdef DEBUG
		if (!VertexTypes||VertexTypes->empty()) 
		{
			Msg("! warning : empty vertex types");
			return false;
		}
#endif

		xr_vector<GameGraph::STerrainPlace>::const_iterator I = VertexTypes->begin();
		xr_vector<GameGraph::STerrainPlace>::const_iterator E = VertexTypes->end();
		const u8* type = vertex(VertexID)->vertex_type();
		for (; I != E; ++I)
		{
			if (mask((*I).tMask, type))
			{
				return true;
			}
		}

		return false;
	};

	auto CalcCost = [](const CVertex* Node1,const CVertex* Node2, const CEdge* Edge)
	{
		return Edge->distance();
	};

	auto DistanceNode = [this](const CVertex* Node1,const CVertex* Node2)
	{
		return Node1->game_point().distance_to(Node2->game_point());
	};

	thread_local	xr_vector<std::pair<float, u32>>	TempPriorityNode;
	thread_local	xr_map<u32, u32>					TempCameFrom;
	thread_local	xr_map<u32, float>					TempCostSoFar;

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
		
	TempPriorityNode.push_back({0.f, FromID});
	TempCameFrom.insert({FromID, FromID});
	TempCostSoFar.insert( {FromID, 0.f});

	
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
		
		const CVertex* Node = vertex(CurrentNodeID);

		IGameGraph::const_iterator	i,e;
		begin		(CurrentNodeID,i,e);

		for (; i != e; i++)
		{
			u32 NeighborID = i->vertex_id();
			if (!IsAccessible(NeighborID)) continue;

			if(MaxIterationCount == 0) continue;
			MaxIterationCount--;

			const CVertex* Neighbor = vertex(NeighborID);
			float NewCost = TempCostSoFar[CurrentNodeID] + CalcCost(Node, Neighbor,i);
			auto TempCostSoFarIterator = TempCostSoFar.find(NeighborID);
			if ((TempCostSoFarIterator != TempCostSoFar.end() &&TempCostSoFarIterator->second > NewCost)|| (TempCostSoFarIterator == TempCostSoFar.end() &&MaxVisitedNodeCount > TempCostSoFar.size()))
			{
				const float Distance = DistanceNode(vertex(ToID), Neighbor);
				if(Distance>MaxRange)
				{
					continue;
				}
;
				if(TempCostSoFarIterator!=TempCostSoFar.end())
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

bool IGameGraph::SearchNearestVertex(u32 start_vertex_id, u8 LevelID, u32& Result) const
{
	auto IsAccessible = [this](u32 VertexID)
	{
		if(!accessible(VertexID))
		{
			return false;
		}

		return true;
	};

	auto CalcCost = [](const CVertex* Node1,const CVertex* Node2, const CEdge* Edge)
	{
		return Edge->distance();
	};

	auto DistanceNode = [this](const CVertex* Node1,const CVertex* Node2)
	{
		return Node1->game_point().distance_to(Node2->game_point());
	};

	thread_local	xr_vector<std::pair<float, u32>>	TempPriorityNode;
	thread_local	xr_map<u32, u32>					TempCameFrom;
	thread_local	xr_map<u32, float>					TempCostSoFar;

	TempPriorityNode.clear();
	TempCameFrom.clear();
	TempCostSoFar.clear();

	u32 FromID = start_vertex_id;
		
	TempPriorityNode.push_back({0.f, FromID});
	TempCameFrom.insert({FromID, FromID});
	TempCostSoFar.insert( {FromID, 0.f });

	
	while (!TempPriorityNode.empty())
	{
		u32 CurrentNodeID = TempPriorityNode.back().second;
		TempPriorityNode.pop_back();

		if(vertex(CurrentNodeID)->level_id() == LevelID)
		{
			Result = CurrentNodeID;
			return true;
		}
		
		const CVertex* Node = vertex(CurrentNodeID);

		const_iterator	i,e;
		begin		(CurrentNodeID,i,e);

		for (; i != e; i++)
		{
			u32 NeighborID = i->vertex_id();
			if (!IsAccessible(NeighborID)) continue;


			const CVertex* Neighbor = vertex(NeighborID);
			float NewCost = TempCostSoFar[CurrentNodeID] + CalcCost(Node, Neighbor,i);
			auto TempCostSoFarIterator = TempCostSoFar.find(NeighborID);
			if ((TempCostSoFarIterator != TempCostSoFar.end() &&TempCostSoFarIterator->second > NewCost)|| (TempCostSoFarIterator == TempCostSoFar.end()))
			{
;
				if(TempCostSoFarIterator!=TempCostSoFar.end())
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
	return false;
}
