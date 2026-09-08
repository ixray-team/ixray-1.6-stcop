#include "StdAfx.h"
#include "GraphEditor.h"
#include "ImUtils.h"

#include "../Level.h"
#include "../ai_space.h"
#include "../game_graph.h"
#include "../level_graph.h"

struct SNewGraphPoint
{
	Fvector Pos = Fvector().set(0, 0, 0);
	u32 LevelVertexID = u32(-1);
	u8 Types[GameGraph::LOCATION_TYPE_COUNT] = {0, 0, 0, 0};
};

struct SRebuiltEdge
{
	u32 From = u32(-1);
	u32 To = u32(-1);
	float Distance = 0.f;
	bool LosOk = true;
};

struct SGraphEditorState
{
	xr_vector<SNewGraphPoint> Points;
	xr_vector<SRebuiltEdge> Edges;

	float MaxLinkDist = 30.f;
	int MaxNeighbours = 6;
	bool UseLos = true;
	float PointLift = 0.3f;

	bool ShowShipped = true;
	bool ShowShippedEdges = true;
	bool ShowLabels = true;
	float Zoom = 1.f;
	ImVec2 Pan = ImVec2(0, 0);
	bool AutoFit = true;
	int Selected = -1;
	int LocationPreset[GameGraph::LOCATION_TYPE_COUNT] = {0, 0, 0, 0};

	string256 Status = {};
	Fvector LastHit = Fvector().set(0, 0, 0);
	bool HasHit = false;

	shared_str CachedLevel;
};

static SGraphEditorState GraphEditor;

static void GraphEd_SetStatus(const char* Format, ...)
{
	va_list Args;
	va_start(Args, Format);
	vsnprintf(GraphEditor.Status, sizeof(GraphEditor.Status), Format, Args);
	va_end(Args);
}

static void GraphEd_PatchFileName(string_path& Output)
{
	string256 Name{};
	xr_sprintf(Name, sizeof(Name), "graph_patch_%s.ltx", Level().name().c_str());
	FS.update_path(Output, "$app_data_root$", Name);
}

static u32 GraphEd_CollectShipped(xr_vector<u32>& OutIDs, xr_vector<Fvector>& OutPositions)
{
	OutIDs.clear();
	OutPositions.clear();

	if (!ai().get_game_graph())
	{
		return 0;
	}

	const IGameGraph& Graph = ai().game_graph();
	const u32 Total = Graph.header().vertex_count();
	const u32 Current = Graph.current_level_vertex();
	const GameGraph::_LEVEL_ID CurrentLevel = Graph.vertex(Current)->level_id();
	for (u32 i = 0; i < Total; ++i)
	{
		const IGameGraph::CVertex* Vertex = Graph.vertex(i);
		if (!Vertex)
		{
			continue;
		}
		if (CurrentLevel != Vertex->level_id())
		{
			continue;
		}
		OutIDs.push_back(i);
		OutPositions.push_back(Vertex->level_point());
	}
	return (u32)OutIDs.size();
}

static bool GraphEd_LineOfSight(const Fvector& A, const Fvector& B)
{
	Fvector Direction = Fvector().sub(B, A);
	const float Distance = Direction.magnitude();
	if (Distance < EPS_L)
	{
		return true;
	}
	Direction.div(Distance);

	Fvector From = A;
	From.y += 1.2f;

	collide::rq_result RQ;
	CObject* Ignore = Level().CurrentControlEntity();
	if (Level().ObjectSpace.RayPick(From, Direction, Distance, collide::rqtStatic, RQ, Ignore))
	{
		if (RQ.range < Distance - 0.6f)
		{
			return false;
		}
	}
	return true;
}

static void GraphEd_Rebuild()
{
	GraphEditor.Edges.clear();

	if (!ai().get_game_graph())
	{
		GraphEd_SetStatus("no game graph loaded");
		return;
	}

	xr_vector<u32> ShippedIDs;
	xr_vector<Fvector> ShippedPos;
	GraphEd_CollectShipped(ShippedIDs, ShippedPos);
	const u32 ShippedCount = (u32)ShippedPos.size();
	const u32 NewCount = (u32)GraphEditor.Points.size();

	if (NewCount == 0)
	{
		GraphEd_SetStatus("nothing to rebuild: no new points");
		return;
	}

	xr_vector<Fvector> All;
	All.reserve(ShippedCount + NewCount);
	for (const Fvector& Point : ShippedPos)
	{
		All.push_back(Point);
	}
	for (const auto& NewPoint : GraphEditor.Points)
	{
		All.push_back(NewPoint.Pos);
	}

	const float MaxDistance = std::max(1.f, GraphEditor.MaxLinkDist);
	const int MaxNeighbours = std::max(1, GraphEditor.MaxNeighbours);

	u32 Linked = 0, Blocked = 0;
	for (u32 NewIndex = 0; NewIndex < NewCount; ++NewIndex)
	{
		const u32 CombinedIndex = ShippedCount + NewIndex;
		const Fvector& PointA = All[CombinedIndex];

		xr_vector<xr_pair<float, u32>> Candidates;
		Candidates.reserve(All.size());
		for (u32 CandidateIndex = 0; CandidateIndex < (u32)All.size(); ++CandidateIndex)
		{
			if (CandidateIndex == CombinedIndex)
			{
				continue;
			}
			const float Distance = PointA.distance_to(All[CandidateIndex]);
			if (Distance <= MaxDistance)
			{
				Candidates.emplace_back(Distance, CandidateIndex);
			}
		}
		std::sort(Candidates.begin(), Candidates.end(), [](const xr_pair<float, u32>& Left, const xr_pair<float, u32>& Right)
				  { return Left.first < Right.first; });

		int Added = 0;
		for (const auto& [Distance, CandidateIndex] : Candidates)
		{
			if (Added >= MaxNeighbours)
			{
				break;
			}
			bool Visible = true;
			if (GraphEditor.UseLos)
			{
				Visible = GraphEd_LineOfSight(PointA, All[CandidateIndex]);
			}
			SRebuiltEdge Edge;
			Edge.From = CombinedIndex;
			Edge.To = CandidateIndex;
			Edge.Distance = Distance;
			Edge.LosOk = Visible;
			if (Visible)
			{
				GraphEditor.Edges.push_back(Edge);
				++Added;
				++Linked;
			}
			else
			{
				GraphEditor.Edges.push_back(Edge);
				++Blocked;
			}
		}
	}

	GraphEd_SetStatus("rebuilt: %u new points, %u links ok, %u blocked (LOS)", NewCount, Linked, Blocked);
}

static bool GraphEd_AddAtCameraLook()
{
	if (!ai().get_level_graph())
	{
		GraphEd_SetStatus("no level graph loaded");
		return false;
	}

	const Fvector& Start = Device.vCameraPosition;
	const Fvector& Direction = Device.vCameraDirection;

	collide::rq_result RQ;
	CObject* Ignore = Level().CurrentControlEntity();
	constexpr float Range = 500.f;
	if (!Level().ObjectSpace.RayPick(Start, Direction, Range, collide::rqtStatic, RQ, Ignore))
	{
		GraphEd_SetStatus("no static hit in look direction (range %.0f)", Range);
		return false;
	}

	Fvector Hit = Fvector(Start).mad(Direction, RQ.range);
	Hit.y += GraphEditor.PointLift;

	SNewGraphPoint Point;
	Point.Pos = Hit;
	for (int i = 0; i < GameGraph::LOCATION_TYPE_COUNT; ++i)
	{
		Point.Types[i] = (u8)clampr(GraphEditor.LocationPreset[i], 0, 255);
	}

	const ILevelGraph& LevelGraph = ai().level_graph();
	if (LevelGraph.valid_vertex_position(Hit))
	{
		u32 Guess = u32(-1);
		if (Level().CurrentControlEntity())
		{
			const Fvector& ActorPos = Level().CurrentControlEntity()->Position();
			if (LevelGraph.valid_vertex_position(ActorPos))
			{
				Guess = LevelGraph.vertex_id(ActorPos);
			}
		}
		Point.LevelVertexID = (Guess == u32(-1)) ? LevelGraph.vertex_id(Hit) : LevelGraph.vertex(Guess, Hit);
		if (!LevelGraph.valid_vertex_id(Point.LevelVertexID))
		{
			Point.LevelVertexID = u32(-1);
		}
	}
	else
	{
		Point.LevelVertexID = u32(-1);
	}

	GraphEditor.LastHit = Hit;
	GraphEditor.HasHit = true;
	GraphEditor.Points.push_back(Point);
	GraphEditor.Selected = (int)GraphEditor.Points.size() - 1;

	if (Point.LevelVertexID == u32(-1))
	{
		GraphEd_SetStatus("added #%u at (%.1f,%.1f,%.1f), NO level vertex nearby!", (u32)GraphEditor.Points.size() - 1, Hit.x, Hit.y, Hit.z);
	}
	else
	{
		GraphEd_SetStatus("added #%u at (%.1f,%.1f,%.1f), level_vertex=%u", (u32)GraphEditor.Points.size() - 1, Hit.x, Hit.y, Hit.z, Point.LevelVertexID);
	}
	return true;
}

static void GraphEd_Save()
{
	string_path FileName;
	GraphEd_PatchFileName(FileName);

	CInifile File(FileName, false, false, true);
	File.w_u32("patch", "count", (u32)GraphEditor.Points.size());
	File.w_float("patch", "max_link_dist", GraphEditor.MaxLinkDist);
	File.w_u32("patch", "max_neighbours", (u32)GraphEditor.MaxNeighbours);
	File.w_bool("patch", "use_los", GraphEditor.UseLos);

	for (u32 i = 0; i < GraphEditor.Points.size(); ++i)
	{
		string64 Section{};
		xr_sprintf(Section, sizeof(Section), "point_%u", i);
		const auto& Point = GraphEditor.Points[i];
		File.w_fvector3(Section, "pos", Point.Pos);
		File.w_u32(Section, "level_vertex", Point.LevelVertexID);
		for (int k = 0; k < GameGraph::LOCATION_TYPE_COUNT; ++k)
		{
			string64 Key{};
			xr_sprintf(Key, sizeof(Key), "loc_%d", k);
			File.w_u8(Section, Key, Point.Types[k]);
		}
	}
	File.save_as(FileName);
	GraphEd_SetStatus("saved %u points -> %s", (u32)GraphEditor.Points.size(), FileName);
}

static void GraphEd_Load()
{
	string_path FileName;
	GraphEd_PatchFileName(FileName);
	if (!FS.exist(FileName))
	{
		GraphEd_SetStatus("no patch file: %s", FileName);
		return;
	}

	CInifile File(FileName, true, true, false);
	const u32 Count = File.line_exist("patch", "count") ? File.r_u32("patch", "count") : 0;
	if (File.line_exist("patch", "max_link_dist"))
	{
		GraphEditor.MaxLinkDist = File.r_float("patch", "max_link_dist");
	}
	if (File.line_exist("patch", "max_neighbours"))
	{
		GraphEditor.MaxNeighbours = (int)File.r_u32("patch", "max_neighbours");
	}
	if (File.line_exist("patch", "use_los"))
	{
		GraphEditor.UseLos = File.r_bool("patch", "use_los");
	}

	GraphEditor.Points.clear();
	GraphEditor.Edges.clear();
	for (u32 i = 0; i < Count; ++i)
	{
		string64 Section{};
		xr_sprintf(Section, sizeof(Section), "point_%u", i);
		if (!File.section_exist(Section))
		{
			continue;
		}
		SNewGraphPoint Point;
		Point.Pos = File.r_fvector3(Section, "pos");
		Point.LevelVertexID = File.line_exist(Section, "level_vertex") ? File.r_u32(Section, "level_vertex") : u32(-1);
		for (int k = 0; k < GameGraph::LOCATION_TYPE_COUNT; ++k)
		{
			string64 Key{};
			xr_sprintf(Key, sizeof(Key), "loc_%d", k);
			Point.Types[k] = File.line_exist(Section, Key) ? File.r_u8(Section, Key) : 0;
		}
		GraphEditor.Points.push_back(Point);
	}
	GraphEditor.Selected = GraphEditor.Points.empty() ? -1 : 0;
	GraphEd_SetStatus("loaded %u points from %s", (u32)GraphEditor.Points.size(), FileName);
}

static float GraphEd_PathWeight(ILevelGraph& LevelGraph, u32 FromNode, const Fvector& From, u32 ToNode, const Fvector& To)
{
	const float Pure = From.distance_to_xz(To);
	if (Pure >= 6000.f)
	{
		return Pure;
	}
	if (LevelGraph.valid_vertex_id(LevelGraph.check_position_in_direction(FromNode, From, To)))
	{
		return Pure;
	}

	xr_vector<u32> Path;
	if (LevelGraph.Search(FromNode, ToNode, Path, 6000) && !Path.empty())
	{
		float Cumulative = 0.f, LastDirect = 0.f;
		Fvector Position = From;
		u32 Node = Path.front();
		for (size_t i = 1; i < Path.size(); ++i)
		{
			float Direct;
			if (LevelGraph.valid_vertex_id(LevelGraph.check_position_in_direction(Node, Position, LevelGraph.vertex_position(Path[i]))))
			{
				Direct = Position.distance_to(LevelGraph.vertex_position(Path[i]));
			}
			else
			{
				Direct = 6000.f;
			}
			if (Direct == 6000.f)
			{
				if (LastDirect == 0.f)
				{
					Cumulative += LevelGraph.distance(Node, Path[i]);
					Node = Path[i];
				}
				else
				{
					Cumulative += LastDirect;
					LastDirect = 0.f;
					Node = Path[i - 1];
				}
				Position = LevelGraph.vertex_position(Node);
			}
			else
			{
				LastDirect = Direct;
			}
			if (Cumulative + LastDirect >= 6000.f)
			{
				return 6000.f;
			}
		}
		if (LevelGraph.valid_vertex_id(LevelGraph.check_position_in_direction(Node, Position, To)))
		{
			return Cumulative + Position.distance_to(To);
		}
		return Cumulative + LastDirect + To.distance_to(LevelGraph.vertex_position(Path.back()));
	}
	return From.distance_to(To);
}

static void GraphEd_WriteBinaries()
{
	if (!ai().get_game_graph() || !ai().get_level_graph())
	{
		GraphEd_SetStatus("no game/level graph loaded");
		return;
	}
	if (GraphEditor.Points.empty())
	{
		GraphEd_SetStatus("nothing to write: no new points");
		return;
	}

	CGameGraph* Graph = static_cast<CGameGraph*>(ai().get_game_graph());
	const IGameGraph& Game = ai().game_graph();
	ILevelGraph& LevelGraph = ai().level_graph();

	const u32 OldTotal = Game.header().vertex_count();
	const GameGraph::_LEVEL_ID CurrentLevel = Game.vertex(Game.current_level_vertex())->level_id();

	xr_vector<u32> ShippedIDs;
	xr_vector<Fvector> ShippedPos;
	GraphEd_CollectShipped(ShippedIDs, ShippedPos);
	const u32 ShippedCount = (u32)ShippedPos.size();

	Fvector Offset = Fvector().set(0.f, 0.f, 0.f);
	if (!ShippedIDs.empty())
	{
		Offset.sub(Game.vertex(ShippedIDs[0])->game_point(), ShippedPos[0]);
	}

	xr_set<u32> UsedNodes;
	for (u32 i = 0; i < ShippedCount; ++i)
	{
		UsedNodes.insert(Game.vertex(ShippedIDs[i])->level_vertex_id());
	}

	xr_vector<SGameGraphPatchVertex> Vertices;
	xr_vector<u32> IndexMap(GraphEditor.Points.size(), u32(-1));
	u32 Skipped = 0;
	for (u32 k = 0; k < (u32)GraphEditor.Points.size(); ++k)
	{
		const SNewGraphPoint& NewPoint = GraphEditor.Points[k];
		const u32 Node = NewPoint.LevelVertexID;
		const bool Valid = Node != u32(-1) && LevelGraph.valid_vertex_id(Node) &&
						   LevelGraph.inside(Node, NewPoint.Pos) && !UsedNodes.contains(Node);
		if (!Valid)
		{
			++Skipped;
			continue;
		}
		UsedNodes.insert(Node);
		SGameGraphPatchVertex PatchVertex;
		PatchVertex.LocalPoint = NewPoint.Pos;
		PatchVertex.GlobalPoint.add(NewPoint.Pos, Offset);
		PatchVertex.LevelID = CurrentLevel;
		PatchVertex.NodeID = Node;
		for (int t = 0; t < GameGraph::LOCATION_TYPE_COUNT; ++t)
		{
			PatchVertex.Types[t] = NewPoint.Types[t];
		}
		IndexMap[k] = (u32)Vertices.size();
		Vertices.push_back(PatchVertex);
	}

	if (Vertices.empty())
	{
		GraphEd_SetStatus("nothing valid to write (skipped %u). Fix level binding first.", Skipped);
		return;
	}

	auto Endpoint = [&](u32 CanvasIndex, u32& OutGameID, u32& OutNode, Fvector& OutPos) -> bool
	{
		if (CanvasIndex < ShippedCount)
		{
			OutGameID = ShippedIDs[CanvasIndex];
			OutNode = Game.vertex(OutGameID)->level_vertex_id();
			OutPos = Game.vertex(OutGameID)->level_point();
			return true;
		}
		const u32 UIIndex = CanvasIndex - ShippedCount;
		if (UIIndex >= IndexMap.size() || IndexMap[UIIndex] == u32(-1))
		{
			return false;
		}
		OutGameID = OldTotal + IndexMap[UIIndex];
		OutNode = Vertices[IndexMap[UIIndex]].NodeID;
		OutPos = Vertices[IndexMap[UIIndex]].LocalPoint;
		return true;
	};

	xr_vector<SGameGraphPatchLink> Links;
	for (const SRebuiltEdge& Edge : GraphEditor.Edges)
	{
		if (!Edge.LosOk)
		{
			continue;
		}
		u32 GameA, GameB, NodeA, NodeB;
		Fvector PosA, PosB;
		if (!Endpoint(Edge.From, GameA, NodeA, PosA) || !Endpoint(Edge.To, GameB, NodeB, PosB))
		{
			continue;
		}
		SGameGraphPatchLink Link;
		Link.From = (u16)GameA;
		Link.To = (u16)GameB;
		Link.Distance = GraphEd_PathWeight(LevelGraph, NodeA, PosA, NodeB, PosB);
		Links.push_back(Link);
	}

	xr_string Error;
	if (!Graph->AppendLevelVertices(Vertices, Links, CurrentLevel, Error))
	{
		GraphEd_SetStatus("%s", Error.c_str());
		return;
	}

	GraphEditor.Points.clear();
	GraphEditor.Edges.clear();
	GraphEditor.Selected = -1;
	GraphEditor.AutoFit = true;
	GraphEd_SetStatus("chunk 4 written to .new file: +%u vertices (skipped %u). Rename to .spawn and reload the level.", (u32)Vertices.size(), Skipped);
}

static ImVec2 GraphEd_WorldToCanvas(const Fvector& World, const ImVec2& Origin, const Fvector2& Center, float Scale, const ImVec2& Pan)
{
	return ImVec2(Origin.x + (World.x - Center.x) * Scale + Pan.x, Origin.y - (World.z - Center.y) * Scale + Pan.y);
}

static void GraphEd_DrawCanvas(const xr_vector<Fvector>& ShippedPos, const xr_vector<u32>& ShippedIDs)
{
	const float CanvasHeight = 340.f;
	ImGui::SeparatorText("Graph map (top view, XZ)");
	ImGui::BeginChild("##GraphEd_Canvas", ImVec2(0, CanvasHeight), true, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse);

	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	const ImVec2 Origin = ImGui::GetCursorScreenPos();
	const ImVec2 Size = ImGui::GetContentRegionAvail();
	if (Size.x <= 0 || Size.y <= 0)
	{
		ImGui::EndChild();
		return;
	}

	ImGuiIO& IO = ImGui::GetIO();
	const bool Hovered = ImGui::IsWindowHovered();
	if (Hovered)
	{
		if (IO.MouseWheel != 0.f)
		{
			GraphEditor.Zoom = clampr(GraphEditor.Zoom * (1.f + IO.MouseWheel * 0.1f), 0.05f, 50.f);
		}
		if (ImGui::IsMouseDragging(ImGuiMouseButton_Right) || ImGui::IsMouseDragging(ImGuiMouseButton_Middle))
		{
			GraphEditor.Pan.x += IO.MouseDelta.x;
			GraphEditor.Pan.y += IO.MouseDelta.y;
			GraphEditor.AutoFit = false;
		}
	}

	Fbox Bounds;
	Bounds.invalidate();
	if (GraphEditor.ShowShipped)
	{
		for (const Fvector& Point : ShippedPos)
		{
			Bounds.modify(Point);
		}
	}
	for (const auto& NewPoint : GraphEditor.Points)
	{
		Bounds.modify(NewPoint.Pos);
	}
	if (GraphEditor.HasHit)
	{
		Bounds.modify(GraphEditor.LastHit);
	}
	Bounds.modify(Device.vCameraPosition);

	Fvector2 Center{0, 0};
	float Scale = GraphEditor.Zoom;
	if (Bounds.is_valid())
	{
		Fvector BoxCenter, BoxSize;
		Bounds.getcenter(BoxCenter);
		Bounds.getsize(BoxSize);
		Center.set(BoxCenter.x, BoxCenter.z);
		if (GraphEditor.AutoFit)
		{
			const float Span = std::max(BoxSize.x, BoxSize.z);
			if (Span > EPS_L)
			{
				Scale = std::min(Size.x, Size.y) / (Span * 1.15f);
			}
			else
			{
				Scale = 1.f;
			}
			GraphEditor.Zoom = Scale;
			GraphEditor.Pan = ImVec2(0, 0);
			if (!GraphEditor.Points.empty() || !ShippedPos.empty())
			{
				GraphEditor.AutoFit = false;
			}
		}
	}
	const ImVec2 CanvasCenter = ImVec2(Origin.x + Size.x * 0.5f, Origin.y + Size.y * 0.5f);

	DrawList->AddRectFilled(Origin, ImVec2(Origin.x + Size.x, Origin.y + Size.y), IM_COL32(12, 14, 18, 255));
	const float GridStep = 10.f * Scale;
	if (GridStep > 8.f && GridStep < 400.f)
	{
		for (float X = fmodf(CanvasCenter.x + GraphEditor.Pan.x, GridStep); X < Size.x; X += GridStep)
		{
			DrawList->AddLine(ImVec2(Origin.x + X, Origin.y), ImVec2(Origin.x + X, Origin.y + Size.y), IM_COL32(255, 255, 255, 14));
		}
		for (float Y = fmodf(CanvasCenter.y + GraphEditor.Pan.y, GridStep); Y < Size.y; Y += GridStep)
		{
			DrawList->AddLine(ImVec2(Origin.x, Origin.y + Y), ImVec2(Origin.x + Size.x, Origin.y + Y), IM_COL32(255, 255, 255, 14));
		}
	}

	auto ToCanvas = [&](const Fvector& World) -> ImVec2
	{
		return GraphEd_WorldToCanvas(World, CanvasCenter, Center, Scale, GraphEditor.Pan);
	};

	const u32 ShippedCount = (u32)ShippedPos.size();

	if (GraphEditor.ShowShipped && GraphEditor.ShowShippedEdges && ai().get_game_graph())
	{
		const IGameGraph& Graph = ai().game_graph();
		u32 Drawn = 0;
		constexpr u32 EdgeCap = 4000;
		xr_hash_map<u32, u32> IDToIndex;
		IDToIndex.reserve(ShippedCount * 2);
		for (u32 i = 0; i < ShippedCount; ++i)
		{
			IDToIndex[ShippedIDs[i]] = i;
		}

		for (u32 i = 0; i < ShippedCount && Drawn < EdgeCap; ++i)
		{
			IGameGraph::const_iterator b, e;
			Graph.begin(ShippedIDs[i], b, e);
			for (; b != e && Drawn < EdgeCap; ++b, ++Drawn)
			{
				const u32 Neighbour = Graph.value(ShippedIDs[i], b);
				auto It = IDToIndex.find(Neighbour);
				if (It == IDToIndex.end())
				{
					continue;
				}
				DrawList->AddLine(ToCanvas(ShippedPos[i]), ToCanvas(ShippedPos[It->second]), IM_COL32(120, 130, 150, 70), 1.f);
			}
		}
	}

	if (GraphEditor.ShowShipped)
	{
		for (u32 i = 0; i < ShippedCount; ++i)
		{
			const ImVec2 Point = ToCanvas(ShippedPos[i]);
			DrawList->AddCircleFilled(Point, 2.5f, IM_COL32(140, 150, 170, 200));
		}
	}

	for (const SRebuiltEdge& Edge : GraphEditor.Edges)
	{
		Fvector PosA, PosB;
		auto IndexToPos = [&](u32 Index, Fvector& Output) -> bool
		{
			if (Index < ShippedCount)
			{
				Output = ShippedPos[Index];
				return true;
			}
			const u32 NewIndex = Index - ShippedCount;
			if (NewIndex < GraphEditor.Points.size())
			{
				Output = GraphEditor.Points[NewIndex].Pos;
				return true;
			}
			return false;
		};
		if (!IndexToPos(Edge.From, PosA) || !IndexToPos(Edge.To, PosB))
		{
			continue;
		}
		DrawList->AddLine(ToCanvas(PosA), ToCanvas(PosB), Edge.LosOk ? IM_COL32(90, 220, 120, 220) : IM_COL32(230, 80, 80, 160), Edge.LosOk ? 2.f : 1.f);
	}

	for (u32 i = 0; i < GraphEditor.Points.size(); ++i)
	{
		const ImVec2 Point = ToCanvas(GraphEditor.Points[i].Pos);
		const bool Selected = (int)i == GraphEditor.Selected;
		DrawList->AddCircleFilled(Point, Selected ? 7.f : 5.f, Selected ? IM_COL32(255, 200, 60, 255) : IM_COL32(255, 140, 40, 255));
		DrawList->AddCircle(Point, Selected ? 8.5f : 6.5f, IM_COL32(0, 0, 0, 220));
		if (GraphEditor.ShowLabels)
		{
			char Buffer[16]{};
			xr_sprintf(Buffer, sizeof(Buffer), "#%u", i);
			DrawList->AddText(ImVec2(Point.x + 9, Point.y - 7), IM_COL32(255, 220, 150, 255), Buffer);
		}
	}

	{
		const ImVec2 CameraPos = ToCanvas(Device.vCameraPosition);
		Fvector2 Direction2D(Device.vCameraDirection.x, Device.vCameraDirection.z);
		const float Length = std::max(Direction2D.magnitude(), EPS_L);
		Direction2D.div(Length);
		const ImVec2 Tip = ImVec2(CameraPos.x + Direction2D.x * 18.f, CameraPos.y - Direction2D.y * 18.f);
		const ImVec2 Left = ImVec2(CameraPos.x - Direction2D.y * 7.f - Direction2D.x * 6.f, CameraPos.y + Direction2D.x * 7.f + Direction2D.y * 6.f);
		const ImVec2 Right = ImVec2(CameraPos.x + Direction2D.y * 7.f - Direction2D.x * 6.f, CameraPos.y - Direction2D.x * 7.f + Direction2D.y * 6.f);
		DrawList->AddTriangleFilled(Tip, Left, Right, IM_COL32(80, 220, 255, 255));
	}

	if (Hovered && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		const ImVec2 Mouse = IO.MousePos;
		int Best = -1;
		float BestDistance = 14.f;
		for (u32 i = 0; i < GraphEditor.Points.size(); ++i)
		{
			const ImVec2 Point = ToCanvas(GraphEditor.Points[i].Pos);
			const float Distance = sqrtf((Point.x - Mouse.x) * (Point.x - Mouse.x) + (Point.y - Mouse.y) * (Point.y - Mouse.y));
			if (Distance < BestDistance)
			{
				BestDistance = Distance;
				Best = (int)i;
			}
		}
		if (Best >= 0)
		{
			GraphEditor.Selected = Best;
		}
	}

	if (GraphEditor.ShowLabels)
	{
		DrawList->AddText(ImVec2(Origin.x + 6, Origin.y + 4), IM_COL32(180, 190, 200, 220), "X -> right, Z -> up | wheel: zoom, RMB-drag: pan, LMB: select");
	}

	ImGui::EndChild();
}

void RenderGraphEditorWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_GraphEditor)])
	{
		return;
	}
	if (!g_pGameLevel)
	{
		return;
	}

	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.f, 0.f, 0.f, kGeneralAlphaLevelForImGuiWindows));
	if (!ImGui::Begin("Graph Editor##InGame", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_GraphEditor)]))
	{
		ImGui::End();
		ImGui::PopStyleColor(1);
		return;
	}

	if (!ai().get_game_graph() || !ai().get_level_graph())
	{
		ImGui::TextWrapped("Game graph / level graph is not loaded (load a level first).");
		ImGui::End();
		ImGui::PopStyleColor(1);
		return;
	}

	if (!GraphEditor.CachedLevel.equal(Level().name()))
	{
		GraphEditor.CachedLevel = Level().name();
		GraphEditor.Points.clear();
		GraphEditor.Edges.clear();
		GraphEditor.Selected = -1;
		GraphEditor.AutoFit = true;
		GraphEd_SetStatus("level: %s", Level().name().c_str());
	}

	xr_vector<u32> ShippedIDs;
	xr_vector<Fvector> ShippedPos;
	GraphEd_CollectShipped(ShippedIDs, ShippedPos);

	ImGui::Text("Level: %s | shipped verts: %u | new: %u | links: %u", Level().name().c_str(), (u32)ShippedPos.size(), (u32)GraphEditor.Points.size(), (u32)GraphEditor.Edges.size());
	ImGui::TextWrapped("Status: %s", GraphEditor.Status[0] ? GraphEditor.Status : "idle");
	ImGui::Text("Camera: (%.1f, %.1f, %.1f)", Device.vCameraPosition.x, Device.vCameraPosition.y, Device.vCameraPosition.z);

	ImGui::SeparatorText("Placement (camera look -> static geometry)");
	ImGui::SliderFloat("Lift above hit", &GraphEditor.PointLift, 0.f, 3.f, "%.2f m");
	ImGui::InputInt("loc[0]", &GraphEditor.LocationPreset[0]);
	ImGui::SameLine();
	ImGui::InputInt("loc[1]", &GraphEditor.LocationPreset[1]);
	ImGui::SameLine();
	ImGui::InputInt("loc[2]", &GraphEditor.LocationPreset[2]);
	ImGui::SameLine();
	ImGui::InputInt("loc[3]", &GraphEditor.LocationPreset[3]);

	for (int i = 0; i < GameGraph::LOCATION_TYPE_COUNT; ++i)
	{
		clamp(GraphEditor.LocationPreset[i], 0, 255);
	}

	if (ImGui::Button("Add graph at camera look"))
	{
		GraphEd_AddAtCameraLook();
	}
	ImGui::SameLine();
	if (ImGui::Button("Rebuild links"))
	{
		GraphEd_Rebuild();
	}
	ImGui::SameLine();
	if (ImGui::Button("Fit view"))
	{
		GraphEditor.AutoFit = true;
	}

	ImGui::SeparatorText("Rebuild params (like xrAI fill_neighbours, simplified)");
	ImGui::SliderFloat("Max link dist", &GraphEditor.MaxLinkDist, 5.f, 200.f, "%.1f m");
	ImGui::SliderInt("Max neighbours", &GraphEditor.MaxNeighbours, 1, 12);
	ImGui::Checkbox("LOS check (static)", &GraphEditor.UseLos);

	ImGui::SeparatorText("View");
	ImGui::Checkbox("Show shipped", &GraphEditor.ShowShipped);
	ImGui::SameLine();
	ImGui::Checkbox("Show shipped edges", &GraphEditor.ShowShippedEdges);
	ImGui::SameLine();
	ImGui::Checkbox("Labels", &GraphEditor.ShowLabels);

	GraphEd_DrawCanvas(ShippedPos, ShippedIDs);

	ImGui::SeparatorText("New points");
	if (GraphEditor.Points.empty())
	{
		ImGui::TextDisabled("No new points yet. Look at the ground and press the button above.");
	}
	else
	{
		if (GraphEditor.Selected >= (int)GraphEditor.Points.size())
		{
			GraphEditor.Selected = (int)GraphEditor.Points.size() - 1;
		}

		int SelectedIndex = GraphEditor.Selected;
		if (ImGui::SliderInt("Selected", &SelectedIndex, 0, (int)GraphEditor.Points.size() - 1))
		{
			GraphEditor.Selected = SelectedIndex;
		}

		SNewGraphPoint& Point = GraphEditor.Points[GraphEditor.Selected];
		ImGui::Text("#%d pos (%.2f, %.2f, %.2f) level_vertex %s", GraphEditor.Selected, Point.Pos.x, Point.Pos.y, Point.Pos.z, Point.LevelVertexID == u32(-1) ? "INVALID" : std::to_string(Point.LevelVertexID).c_str());
		ImGui::InputFloat3("Position##graphed", &Point.Pos.x);
		int LevelVertex = (int)Point.LevelVertexID;
		if (ImGui::InputInt("Level vertex##graphed", &LevelVertex))
		{
			Point.LevelVertexID = LevelVertex < 0 ? u32(-1) : (u32)LevelVertex;
		}
		for (int i = 0; i < GameGraph::LOCATION_TYPE_COUNT; ++i)
		{
			char Label[16]{};
			xr_sprintf(Label, sizeof(Label), "t[%d]", i);
			int Type = Point.Types[i];
			if (ImGui::InputInt(Label, &Type))
			{
				Point.Types[i] = (u8)clampr(Type, 0, 255);
			}
			if (i < GameGraph::LOCATION_TYPE_COUNT - 1)
			{
				ImGui::SameLine();
			}
		}
		if (ImGui::Button("Rebind level vertex"))
		{
			const ILevelGraph& LevelGraph = ai().level_graph();
			if (LevelGraph.valid_vertex_position(Point.Pos))
			{
				Point.LevelVertexID = LevelGraph.vertex_id(Point.Pos);
				GraphEd_SetStatus("rebound #%d -> level_vertex %u", GraphEditor.Selected, Point.LevelVertexID);
			}
			else
			{
				GraphEd_SetStatus("position outside level.ai, cannot bind");
			}
		}
		ImGui::SameLine();
		if (ImGui::Button("Teleport actor here"))
		{
			xr_string Command = "set_actor_position ";
			Command += Command.ToString(Point.Pos.x);
			Command += ",";
			Command += Command.ToString(Point.Pos.y + 1.f);
			Command += ",";
			Command += Command.ToString(Point.Pos.z);
			execute_console_command_deferred(Console, Command.c_str());
		}
		ImGui::SameLine();
		if (ImGui::Button("Delete selected"))
		{
			GraphEditor.Points.erase(GraphEditor.Points.begin() + GraphEditor.Selected);
			GraphEditor.Edges.clear();
			GraphEditor.Selected = GraphEditor.Points.empty() ? -1 : 0;
		}
		ImGui::SameLine();
		if (ImGui::Button("Clear all"))
		{
			GraphEditor.Points.clear();
			GraphEditor.Edges.clear();
			GraphEditor.Selected = -1;
		}

		if (ImGui::BeginListBox("##graphed_list", ImVec2(-FLT_MIN, 110)))
		{
			for (u32 i = 0; i < GraphEditor.Points.size(); ++i)
			{
				const auto& ListPoint = GraphEditor.Points[i];
				char Buffer[96]{};
				xr_sprintf(Buffer, sizeof(Buffer), "#%u (%.1f,%.1f,%.1f) lv=%s", i, ListPoint.Pos.x, ListPoint.Pos.y, ListPoint.Pos.z, ListPoint.LevelVertexID == u32(-1) ? "?" : std::to_string(ListPoint.LevelVertexID).c_str());
				if (ImGui::Selectable(Buffer, (int)i == GraphEditor.Selected))
				{
					GraphEditor.Selected = (int)i;
				}
			}
			ImGui::EndListBox();
		}
	}

	ImGui::SeparatorText("Game binaries (bypass LE/xrAI)");
	ImGui::TextWrapped("Appends the new points + rebuilt links into chunk 4 of the active .spawn file, saved as .new next to it. Skips points with bad level binding. Rename .new to .spawn and reload the level to apply.");
	if (ImGui::Button("Write new points to game binaries"))
	{
		GraphEd_WriteBinaries();
	}

	ImGui::SeparatorText("Patch file (human-readable backup)");
	if (ImGui::Button("Save patch"))
	{
		GraphEd_Save();
	}
	ImGui::SameLine();
	if (ImGui::Button("Load patch"))
	{
		GraphEd_Load();
		GraphEditor.AutoFit = true;
	}
	ImGui::SameLine();
	if (ImGui::Button("Rebuild + Save"))
	{
		GraphEd_Rebuild();
		GraphEd_Save();
	}

	if (ImGui::CollapsingHeader("Export preview"))
	{
		ImGui::TextWrapped("Each new point becomes a game vertex after the shipped count; only LOS-ok links are written (red ones are skipped). "
						   "Binary write enforces xrAI rules: valid level.ai node, node inside check, one graph point per node.");
		for (u32 i = 0; i < GraphEditor.Points.size(); ++i)
		{
			const auto& PreviewPoint = GraphEditor.Points[i];
			ImGui::Text("#%u pos=%.2f,%.2f,%.2f lv=%u loc=%u,%u,%u,%u", i, PreviewPoint.Pos.x, PreviewPoint.Pos.y, PreviewPoint.Pos.z, PreviewPoint.LevelVertexID, PreviewPoint.Types[0], PreviewPoint.Types[1], PreviewPoint.Types[2], PreviewPoint.Types[3]);
		}
		u32 OkLinks = 0;
		for (const auto& Edge : GraphEditor.Edges)
		{
			OkLinks += Edge.LosOk ? 1 : 0;
		}
		ImGui::Text("links: %u ok / %u total", OkLinks, (u32)GraphEditor.Edges.size());
	}

	ImGui::End();
	ImGui::PopStyleColor(1);
}
