#pragma once

#include "../xrEngine/AI/game_graph.h"
#include "../xrScripts/script_export_space.h"
#include "game_level_cross_table.h"

#define GRAPH_NAME			"game.graph"

struct SGameGraphPatchVertex
{
	Fvector LocalPoint = Fvector().set(0.f, 0.f, 0.f);
	Fvector GlobalPoint = Fvector().set(0.f, 0.f, 0.f);
	u8 LevelID = 0;
	u32 NodeID = u32(-1);
	u8 Types[GameGraph::LOCATION_TYPE_COUNT] = {};
};

struct SGameGraphPatchLink
{
	u16 From = u16(-1);
	u16 To = u16(-1);
	float Distance = 0.f;
};

class CGameGraph:public IGameGraph
{
private:
	friend class CRenumbererConverter;
public:
	CGameGraph(const IReader &stream);
	virtual ~CGameGraph();
	virtual void set_current_level(u32 const level_id);
	bool AppendLevelVertices(
		const xr_vector<SGameGraphPatchVertex>& Vertices,
		const xr_vector<SGameGraphPatchLink>& Links,
		u8 CurrentLevelID,
		xr_string& Error);
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
