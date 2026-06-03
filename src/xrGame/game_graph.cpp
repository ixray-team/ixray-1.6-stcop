#include "stdafx.h"
#include "game_graph.h"
#include "ai_space.h"
#include "alife_simulator.h"
#include "alife_simulator_base.h"
#include "alife_spawn_registry.h"
#include "GamePersistent.h"

static bool ReadWholeFile(const char* Path, xr_vector<u8>& Output)
{
	Output.clear();
	FILE* File = fopen(Path, "rb");
	if (!File)
		return false;
	fseek(File, 0, SEEK_END);
	const long Size = ftell(File);
	fseek(File, 0, SEEK_SET);
	if (Size <= 0)
	{
		fclose(File);
		return false;
	}
	Output.resize((size_t)Size);
	const size_t Read = fread(Output.data(), 1, (size_t)Size, File);
	fclose(File);
	return Read == (size_t)Size;
}

static bool WriteWholeFile(const char* Path, const void* Data, size_t Size)
{
	FILE* File = fopen(Path, "wb");
	if (!File)
		return false;
	const size_t Written = fwrite(Data, 1, Size, File);
	fclose(File);
	return Written == Size;
}

CGameGraph::CGameGraph(const IReader& _stream)
{
	VERIFY(!Device.IsEditorMode());
	IReader& stream = const_cast<IReader&>(_stream);
	m_header.load(&stream);

	const u32 AIVersion = header().version();
	R_ASSERT2(CHECK_SPAWN_VERSION(AIVersion), "Graph version mismatch!");
	m_edges = (BYTE*)_stream.pointer();
	m_nodes = (CVertex*)_stream.pointer();
	m_current_level_some_vertex_id = _GRAPH_ID(-1);
	m_enabled.assign(header().vertex_count(), true);

	if (header().version() <= XRAI_SOC_VERSION)
	{
		m_cross_tables = nullptr;
		m_current_level_cross_table = nullptr;
		return;
	}

	u8* temp = (u8*)(m_nodes + header().vertex_count());
	temp += header().edge_count() * sizeof(CGameGraph::CEdge);
	m_cross_tables = (u32*)(((CLevelPoint*)temp) + header().death_point_count());
	m_current_level_cross_table = 0;
}

CGameGraph::~CGameGraph()
{
	VERIFY(Device.IsEditorMode() == false);
	xr_delete(m_current_level_cross_table);
}

void CGameGraph::set_current_level(u32  level_id)
{
	xr_delete(m_current_level_cross_table);
	if (m_cross_tables)
	{
		u32* current_cross_table = m_cross_tables;
		auto	I = header().levels().begin();
		auto	E = header().levels().end();
		for (; I != E; ++I) {
			if (level_id != I->first) {
				current_cross_table = (u32*)((u8*)current_cross_table + *current_cross_table);
				continue;
			}
	
			m_current_level_cross_table = new CGameLevelCrossTable(current_cross_table + 1, *current_cross_table);
			break;
		}
	}
	else
	{
		string_path fName;
		FS.update_path(fName, "$level$", CROSS_TABLE_NAME);
		m_current_level_cross_table = new CGameLevelCrossTable(fName);
	}
	VERIFY(m_current_level_cross_table);

	m_current_level_some_vertex_id = _GRAPH_ID(-1);
	for (_GRAPH_ID i = 0, n = header().vertex_count(); i < n; ++i) {
		if (level_id != vertex(i)->level_id())
			continue;

		m_current_level_some_vertex_id = i;
		break;
	}

	VERIFY(valid_vertex_id(m_current_level_some_vertex_id));
}

bool CGameGraph::AppendLevelVertices(
	const xr_vector<SGameGraphPatchVertex>& Vertices,
	const xr_vector<SGameGraphPatchLink>& Links,
	u8 CurrentLevelID,
	xr_string& Error)
{
	const u32 OldVertexCount = header().vertex_count();
	const u32 NewVertexCount = (u32)Vertices.size();

	if (!NewVertexCount)
	{
		Error = "AppendLevelVertices: no new vertices";
		return false;
	}
	if (OldVertexCount + NewVertexCount > (u32)_GRAPH_ID(-1))
	{
		Error = "AppendLevelVertices: vertex id overflow (u16 limit)";
		return false;
	}

	const u32 TotalVertexCount = OldVertexCount + NewVertexCount;

	for (const SGameGraphPatchLink& Link : Links)
	{
		if (Link.From >= TotalVertexCount || Link.To >= TotalVertexCount || Link.From == Link.To)
		{
			Error = "AppendLevelVertices: link endpoint out of range";
			return false;
		}
		if (!(Link.Distance > 0.f) || Link.Distance >= 6000.f)
		{
			Error = "AppendLevelVertices: bad link distance";
			return false;
		}
	}

	xr_vector<xr_vector<xr_pair<u16, float>>> Adjacency(TotalVertexCount);
	for (u32 i = 0; i < OldVertexCount; ++i)
	{
		const_iterator b, e;
		begin(i, b, e);
		for (; b != e; ++b)
			Adjacency[i].emplace_back(value(i, b), edge_weight(b));
	}
	auto AddDirected = [](xr_vector<xr_pair<u16, float>>& Run, u16 To, float Weight)
	{
		for (const auto& Pair : Run)
			if (Pair.first == To)
				return;
		Run.emplace_back(To, Weight);
	};
	for (const SGameGraphPatchLink& Link : Links)
	{
		AddDirected(Adjacency[Link.From], Link.To, Link.Distance);
		AddDirected(Adjacency[Link.To], Link.From, Link.Distance);
	}
	for (u32 i = 0; i < TotalVertexCount; ++i)
	{
		if (Adjacency[i].size() > 255)
		{
			Error = "AppendLevelVertices: neighbour count overflow (>255)";
			return false;
		}
	}

	const u32 OldEdgeCount = header().edge_count();
	const u32 OldDeathPointCount = header().death_point_count();
	u32 TotalEdgeCount = 0;
	for (const auto& Run : Adjacency)
		TotalEdgeCount += (u32)Run.size();

	u32* LiveTable = m_cross_tables;
	u32 LiveTableSize = 0;
	if (LiveTable)
	{
		u32* Table = LiveTable;
		for (auto I = header().levels().begin(), E = header().levels().end(); I != E; ++I)
		{
			const u32 Size = *Table;
			if ((*I).first == CurrentLevelID)
			{
				LiveTable = Table;
				LiveTableSize = Size;
				break;
			}
			Table = (u32*)((u8*)Table + Size);
		}
		if (!LiveTableSize)
		{
			Error = "AppendLevelVertices: current level has no embedded cross table";
			return false;
		}
	}

	u32 CellCount = 0;
	if (LiveTable)
	{
		using XCHeader = IGameLevelCrossTable::CHeader;
		using XCCell = IGameLevelCrossTable::CCell;
		const XCHeader* TableHeader = (const XCHeader*)((const u8*)LiveTable + sizeof(u32));
		if (LiveTableSize < sizeof(u32) + sizeof(XCHeader) ||
			(LiveTableSize - sizeof(u32) - sizeof(XCHeader)) % sizeof(XCCell) != 0)
		{
			Error = "AppendLevelVertices: embedded cross table is malformed";
			return false;
		}
		CellCount = (LiveTableSize - (u32)sizeof(u32) - (u32)sizeof(XCHeader)) / (u32)sizeof(XCCell);
		if (TableHeader->dwNodeCount != CellCount)
		{
			Error = "AppendLevelVertices: embedded cross table header mismatch";
			return false;
		}
	}

	xr_vector<u32> UsedNodes;
	UsedNodes.reserve(OldVertexCount + NewVertexCount);
	for (u32 i = 0; i < OldVertexCount; ++i)
	{
		if (vertex(i)->level_id() == CurrentLevelID)
			UsedNodes.push_back(vertex(i)->level_vertex_id());
	}
	for (u32 k = 0; k < NewVertexCount; ++k)
	{
		const u32 NodeID = Vertices[k].NodeID;
		if (NodeID == u32(-1) || (LiveTable && NodeID >= CellCount))
		{
			Error = make_string<xr_string>("AppendLevelVertices: new vertex #%u has no valid level node", k);
			return false;
		}
		if (Vertices[k].LevelID != CurrentLevelID)
		{
			Error = make_string<xr_string>("AppendLevelVertices: new vertex #%u has wrong level id", k);
			return false;
		}
		for (u32 Used : UsedNodes)
		{
			if (Used == NodeID)
			{
				Error = make_string<xr_string>("AppendLevelVertices: level node %u of new vertex #%u is already used by another graph point", NodeID, k);
				return false;
			}
		}
		UsedNodes.push_back(NodeID);
	}

	CMemoryWriter Writer;
	CHeader NewHeader = m_header;
	NewHeader.m_vertex_count = (_GRAPH_ID)TotalVertexCount;
	NewHeader.m_edge_count = TotalEdgeCount;
	NewHeader.save(&Writer);

	const u32 VerticesSize = TotalVertexCount * (u32)sizeof(CVertex);
	u32 EdgeOffset = VerticesSize;
	for (u32 i = 0; i < TotalVertexCount; ++i)
	{
		CVertex Vertex;
		if (i < OldVertexCount)
		{
			Vertex = *vertex(i);
		}
		else
		{
			const SGameGraphPatchVertex& Patch = Vertices[i - OldVertexCount];
			Vertex.tLocalPoint = Patch.LocalPoint;
			Vertex.tGlobalPoint = Patch.GlobalPoint;
			Vertex.tLevelID = Patch.LevelID;
			Vertex.tNodeID = Patch.NodeID;
			for (int t = 0; t < GameGraph::LOCATION_TYPE_COUNT; ++t)
				Vertex.tVertexTypes[t] = Patch.Types[t];
			Vertex.dwPointOffset = 0;
			Vertex.tDeathPointCount = 0;
		}
		Vertex.dwEdgeOffset = EdgeOffset;
		Vertex.tNeighbourCount = (u8)Adjacency[i].size();
		EdgeOffset += (u32)Adjacency[i].size() * (u32)sizeof(CEdge);
		Writer.w(&Vertex, sizeof(Vertex));
	}

	for (u32 i = 0; i < TotalVertexCount; ++i)
	{
		for (const auto& [Neighbour, Weight] : Adjacency[i])
		{
			CEdge Edge;
			Edge.m_vertex_id = Neighbour;
			Edge.m_path_distance = Weight;
			Writer.w(&Edge.m_vertex_id, sizeof(Edge.m_vertex_id));
			Writer.w_float(Edge.m_path_distance);
		}
	}

	const u8* DeathPoints = (const u8*)m_nodes + OldVertexCount * sizeof(CVertex) + OldEdgeCount * sizeof(CEdge);
	Writer.w(DeathPoints, OldDeathPointCount * sizeof(CLevelPoint));

	if (m_cross_tables)
	{
		u32* Table = m_cross_tables;
		for (auto I = header().levels().begin(), E = header().levels().end(); I != E; ++I)
		{
			const u32 Size = *Table;
			const size_t TableOffset = Writer.size();
			Writer.w(Table, Size);
			if ((*I).first == CurrentLevelID)
			{
				using XCHeader = IGameLevelCrossTable::CHeader;
				using XCCell = IGameLevelCrossTable::CCell;
				u8* TableData = (u8*)Writer.pointer() + TableOffset;
				XCHeader* TableHeader = (XCHeader*)(TableData + sizeof(u32));
				XCCell* Cells = (XCCell*)(TableData + sizeof(u32) + sizeof(XCHeader));
				for (u32 k = 0; k < NewVertexCount; ++k)
				{
					XCCell& Cell = Cells[Vertices[k].NodeID];
					Cell.tGraphIndex = (_GRAPH_ID)(OldVertexCount + k);
					Cell.fDistance = 0.f;
				}
				TableHeader->dwGraphPointCount += NewVertexCount;
			}
			Table = (u32*)((u8*)Table + Size);
		}
	}

	string_path SpawnPath;
	{
		if (g_pGamePersistent->GameType() == eGameIDSingle)
		{
			if (!ai().get_alife())
			{
				Error = "AppendLevelVertices: no alife simulator";
				return false;
			}
			const shared_str& SpawnName = ai().get_alife()->spawns().get_spawn_name();
			if (!SpawnName.size() || !FS.exist(SpawnPath, "$game_spawn$", *SpawnName, ".spawn"))
			{
				Error = "AppendLevelVertices: cannot resolve the active spawn file";
				return false;
			}
		}
		else if (!FS.exist(SpawnPath, "$level$", "alife", ".spawn"))
		{
			Error = "AppendLevelVertices: cannot resolve $level$/alife.spawn";
			return false;
		}
	}

	xr_vector<u8> FileData;
	if (!ReadWholeFile(SpawnPath, FileData))
	{
		Error = make_string<xr_string>("AppendLevelVertices: cannot open %s", SpawnPath);
		return false;
	}

	struct SChunkRange
	{
		u32 ID = u32(-1);
		size_t Start = 0;
		u32 Size = 0;
	};
	xr_vector<SChunkRange> Chunks;
	for (size_t Position = 0; Position + 8 <= FileData.size();)
	{
		u32 ID, Size;
		memcpy(&ID, FileData.data() + Position, 4);
		memcpy(&Size, FileData.data() + Position + 4, 4);
		if ((ID & CFS_CompressMark) || (u64)Position + 8 + (u64)Size > (u64)FileData.size())
		{
			Error = "AppendLevelVertices: spawn file has unexpected chunk layout, abort";
			return false;
		}
		SChunkRange Chunk;
		Chunk.ID = ID;
		Chunk.Start = Position + 8;
		Chunk.Size = Size;
		Chunks.push_back(Chunk);
		Position += 8 + Size;
	}

	int HeaderChunk = -1, GraphChunk = -1;
	for (u32 i = 0; i < (u32)Chunks.size(); ++i)
	{
		if (Chunks[i].ID == 0 && HeaderChunk < 0)
			HeaderChunk = (int)i;
		if (Chunks[i].ID == 4)
		{
			if (GraphChunk >= 0)
			{
				Error = "AppendLevelVertices: spawn file has duplicate chunk 4, abort";
				return false;
			}
			GraphChunk = (int)i;
		}
	}
	if (HeaderChunk < 0 || Chunks[(u32)HeaderChunk].Size < 4 + 16 + 16 + 4 + 4)
	{
		Error = "AppendLevelVertices: spawn file has no valid header chunk, abort";
		return false;
	}

	{
		IReader Reader((void*)(FileData.data() + Chunks[(u32)HeaderChunk].Start), Chunks[(u32)HeaderChunk].Size);
		Reader.r_u32();
		xrGUID FileGUID, GraphGUID;
		Reader.r(&FileGUID, sizeof(FileGUID));
		Reader.r(&GraphGUID, sizeof(GraphGUID));
		if (!(GraphGUID == header().guid()))
		{
			Error = "AppendLevelVertices: spawn graph_guid differs from the loaded graph, abort";
			return false;
		}
	}

	if (GraphChunk >= 0)
	{
		IReader Reader((void*)(FileData.data() + Chunks[(u32)GraphChunk].Start), Chunks[(u32)GraphChunk].Size);
		CHeader DiskHeader;
		DiskHeader.load(&Reader);
		if (!(DiskHeader.guid() == header().guid()) || DiskHeader.vertex_count() != OldVertexCount)
		{
			Error = "AppendLevelVertices: spawn chunk 4 differs from the loaded graph, abort";
			return false;
		}
	}

	if (Writer.size() > (size_t)(u32)-1)
	{
		Error = "AppendLevelVertices: graph image too large";
		return false;
	}
	xr_vector<u8> Output;
	{
		const u32 NewSize = (u32)Writer.size();
		const u8* Image = (const u8*)Writer.pointer();
		auto AppendU32 = [&](u32 Value)
		{
			const u8* Data = (const u8*)&Value;
			Output.insert(Output.end(), Data, Data + 4);
		};
		if (GraphChunk >= 0)
		{
			const SChunkRange& Chunk = Chunks[(u32)GraphChunk];
			Output.insert(Output.end(), FileData.begin(), FileData.begin() + (Chunk.Start - 8));
			AppendU32(4);
			AppendU32(NewSize);
			Output.insert(Output.end(), Image, Image + Writer.size());
			Output.insert(Output.end(), FileData.begin() + (Chunk.Start + Chunk.Size), FileData.end());
		}
		else
		{
			Output = FileData;
			AppendU32(4);
			AppendU32(NewSize);
			Output.insert(Output.end(), Image, Image + Writer.size());
		}
		Msg("* GraphEditor: spawn chunk 4 %s (%u vertices)", GraphChunk >= 0 ? "replaced" : "added", TotalVertexCount);
	}

	{
		string_path NewPath;
		xr_strconcat(NewPath, SpawnPath, ".new");
		if (!WriteWholeFile(NewPath, Output.data(), Output.size()))
		{
			Error = make_string<xr_string>("AppendLevelVertices: cannot write %s", NewPath);
			return false;
		}
	}

	Msg("* GraphEditor: patched %u vertices, %u new edges", NewVertexCount, TotalEdgeCount - OldEdgeCount);
	return true;
}
