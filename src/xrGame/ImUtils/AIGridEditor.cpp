#include "StdAfx.h"
#include "AIGridEditor.h"
#include "ImUtils.h"
#include "../Level.h"
#include "../ai_space.h"
#include "../level_graph.h"
#include "../cover_manager.h"
#include "../../xrEngine/AI/game_level_cross_table.h"
#include "../../xrEngine/AI/level_graph_format.h"
#include "../../xrEngine/GameMtlLib.h"
#include "../../xrUI/ui_defs.h"
#include "../../utils/xrLevelBuilder/xrAI/AIMapExport.h"
#include <cmath>

namespace AIGrid
{
constexpr u32 Invalid = LevelGraph::NodeUncompressed::LINK_MASK_0;
constexpr u32 LegacyMagic = 0x31474941; // AIG1, no deletion mask.
constexpr u32 Magic = 0x32474941;		// AIG2, stable-ID overlay with deletion mask.
constexpr int DX[4] = {-1, 0, 1, 0};
constexpr int DZ[4] = {0, 1, 0, -1};
using Cell = IGameLevelCrossTable::CCell;

struct Node
{
	ILevelGraph::CVertex Vertex{};
	Fvector Position;
	Fvector Normal;
	Fvector Corners[4];
	bool Modified = false;
	bool Deleted = false;
	u32 Next = Invalid;
};

static u64 Key(int X, int Z)
{
	return (u64(u32(X)) << 32) | u32(Z);
}

struct PickContext
{
	collide::rq_result Result;
	bool IgnoreActorObstacles = true;
};

static bool PickCallback(collide::rq_result& Result, void* Data)
{
	auto& Context = *static_cast<PickContext*>(Data);
	const auto& Triangle = g_pGameLevel->ObjectSpace.GetStaticTris()[Result.element];
	const SGameMtl* Material = GMLib.GetMaterialByIdx(Triangle.material);
	if (Material && (Material->Flags.is(SGameMtl::flPassable) || (Context.IgnoreActorObstacles && Material->Flags.is(SGameMtl::flActorObstacle))))
	{
		return true;
	}
	Context.Result = Result;
	return false;
}

static bool Ray(const Fvector& Origin, const Fvector& Direction, float Range, Fvector& Hit, Fvector* Normal = nullptr, u16* MaterialIndex = nullptr, bool IgnoreActorObstacles = true)
{
	PickContext Context;
	Context.IgnoreActorObstacles = IgnoreActorObstacles;
	auto& Result = Context.Result;
	Result.set(nullptr, Range, -1);
	collide::rq_results Results;
	collide::ray_defs Definition(Origin, Direction, Range, CDB::OPT_FULL_TEST, collide::rqtStatic);
	g_pGameLevel->ObjectSpace.RayQuery(Results, Definition, PickCallback, &Context, nullptr, nullptr);
	if (Result.element < 0)
	{
		return false;
	}
	Hit.mad(Origin, Direction, Result.range);
	if (MaterialIndex)
	{
		*MaterialIndex = g_pGameLevel->ObjectSpace.GetStaticTris()[Result.element].material;
	}
	if (Normal)
	{
		const auto& Triangle = g_pGameLevel->ObjectSpace.GetStaticTris()[Result.element];
		const auto& Vertices = g_pGameLevel->ObjectSpace.GetStaticVerts();
		Normal->mknormal(Vertices[Triangle.verts[0]], Vertices[Triangle.verts[1]], Vertices[Triangle.verts[2]]);
	}
	return true;
}

static Fvector MouseDirection()
{
	float X, Y;
	SDL_GetMouseState(&X, &Y);
	const float Width = float(Device.GetSwapchainWidth());
	const float Height = float(Device.GetSwapchainHeight());
	const float Vertical = tan(deg2rad(Device.fFOV) * 0.5f);
	Fvector Direction = Device.vCameraDirection;
	Direction.mad(Device.vCameraRight, (2.f * X / std::max(1.f, Width) - 1.f) * Vertical / Device.fASPECT);
	Direction.mad(Device.vCameraTop, (1.f - 2.f * Y / std::max(1.f, Height)) * Vertical);
	return Direction.normalize_safe();
}

class Editor final : public pureFrame
{
public:
	xr_vector<Node> Nodes;
	xr_hash_map<u64, u32> Columns;
	xr_hash_map<u64, xr_vector<u32>> Tiles;
	xr_vector<IUIRender::r_vertL<3>> Triangles;
	xr_vector<Cell> Cells;
	xr_vector<Cell> RevertCells;
	xr_vector<u8> ReservedBytes;
	xr_hash_set<u32> ProtectedNodes;
	bool SourceFormatVerified = false;
	hdrNODES Header{};
	const ILevelGraph* Graph = nullptr;
	xrGUID BaseGUID{};
	u32 BaseCount = 0;
	u32 SourceCount = 0;
	u32 RemovedCount = 0;
	u32 Selected = Invalid;
	bool EditHeight = false;
	bool EditTilt = false;
	float Radius = 2.f;
	bool ConnectedOnly = true;
	bool FilterAngle = false;
	float MinAngle = 0.f;
	float MaxAngle = 56.f;
	bool FilterMaterial = false;
	bool HasMaterial = false;
	u16 BrushMaterial = 0;
	xr_string MaterialName;
	float DrawDistance = 70.f;
	bool Brush = false;
	bool Single = false;
	bool Show = true;
	bool Pending = false;
	bool PendingRevert = false;
	bool Registered = false;
	bool Dirty = false;
	Fvector LastStroke = Fvector().set(flt_max, flt_max, flt_max);
	bool LastErasing = false;
	Fvector LastGround = Fvector().set(0.f, 0.f, 0.f);
	bool HasGround = false;
	xr_string Status;
	ui_shader Shader;

	~Editor()
	{
		if (Registered && DevicePtr)
		{
			Device.seqFrame.Remove(this);
		}
	}

	void Reset()
	{
		Nodes.clear();
		Columns.clear();
		Tiles.clear();
		Cells.clear();
		RevertCells.clear();
		ReservedBytes.clear();
		ProtectedNodes.clear();
		SourceFormatVerified = false;
		SourceCount = 0;
		RemovedCount = 0;
		Triangles.clear();
		Graph = nullptr;
		HasGround = false;
		HasMaterial = false;
		MaterialName.clear();
		Selected = Invalid;
		Pending = false;
		PendingRevert = false;
		Dirty = false;
		LastStroke.set(flt_max, flt_max, flt_max);
	}

	int X(const Fvector& P) const { return iFloor((P.x - Header.aabb.min.x) / Header.size + .5f); }
	int Z(const Fvector& P) const { return iFloor((P.z - Header.aabb.min.z) / Header.size + .5f); }

	void Cache(u32 ID, bool Index = true)
	{
		Node& N = Nodes[ID];
		const float Half = Header.size * .48f;
		for (int I = 0; I < 4; ++I)
		{
			N.Corners[I].set(N.Position.x + ((I == 1 || I == 2) ? Half : -Half), N.Position.y + .035f, N.Position.z + ((I >= 2) ? Half : -Half));
			if (fabsf(N.Normal.y) > .01f)
			{
				N.Corners[I].y -= (N.Normal.x * (N.Corners[I].x - N.Position.x) + N.Normal.z * (N.Corners[I].z - N.Position.z)) / N.Normal.y;
			}
		}
		if (Index)
		{
			if (N.Deleted)
			{
				++RemovedCount;
			}
			const u64 ColumnKey = Key(X(N.Position), Z(N.Position));
			auto Column = Columns.find(ColumnKey);
			N.Next = Column == Columns.end() ? Invalid : Column->second;
			Columns[ColumnKey] = ID;
			Tiles[Key(iFloor(N.Position.x / 16.f), iFloor(N.Position.z / 16.f))].push_back(ID);
		}
	}

	void ReadCurrent()
	{
		Reset();
		Graph = ai().get_level_graph();
		Header = Graph->header();
		BaseGUID = Header.guid;
		BaseCount = Header.count;
		Nodes.reserve(Header.count);
		for (u32 I = 0; I < Header.count; ++I)
		{
			Node N;
			N.Vertex = *Graph->vertex(I);
			N.Deleted = !Graph->IsEditorNodeEnabled(I);
			for (u8 D = 0; D < 4; ++D)
			{
				if (N.Vertex.link(D) >= Header.count)
				{
					N.Vertex.link(D, Invalid);
				}
			}
			N.Position = Graph->vertex_position(I);
			pvDecompress(N.Normal, N.Vertex.plane());
			Nodes.push_back(N);
			Cache(I);
		}
		if (ai().get_game_graph())
		{
			const auto& Game = ai().game_graph();
			const auto LevelID = Game.vertex(Game.current_level_vertex())->level_id();
			for (u32 I = 0; I < Game.header().vertex_count(); ++I)
			{
				if (Game.vertex(I)->level_id() == LevelID)
				{
					ProtectedNodes.insert(Game.vertex(I)->level_vertex_id());
				}
			}
		}
		ReadReservedBytes();
		Status = "Loaded current AI grid. Hold LMB on the surface to paint.";
	}

	void ReadReservedBytes()
	{
		LevelGraphFormat::Limits Format;
		if (!LevelGraphFormat::GetLimits(Header.version, Format))
		{
			return;
		}
		string_path Path;
		FS.update_path(Path, "$level$", "level.ai");
		IReader* Reader = FS.r_open(Path);
		if (!Reader)
		{
			return;
		}
		if (Reader->length() >= sizeof(hdrNODES))
		{
			hdrNODES Original;
			Reader->r(&Original, sizeof(Original));
			if (Original.version == Header.version && Original.guid == BaseGUID &&
				u64(Reader->length()) == sizeof(Original) + u64(Original.count) * Format.RecordSize)
			{
				SourceFormatVerified = true;
				SourceCount = Original.count;
				if (Header.version != XRAI_LARGE_VERSION)
				{
					ReservedBytes.assign(Nodes.size(), 0);
					const auto* Bytes = static_cast<const u8*>(Reader->pointer());
					const u32 Offset = Header.version == XRAI_CURRENT_VERSION ? 12 : 11;
					for (u32 I = 0; I < Original.count && I < ReservedBytes.size(); ++I)
					{
						ReservedBytes[I] = Bytes[size_t(I) * Format.RecordSize + Offset] & 0xf0;
					}
				}
			}
		}
		FS.r_close(Reader);
	}

	u32 Nearest(int ColumnX, int ColumnZ, float Height, float Limit, bool IncludeDeleted = false) const
	{
		auto It = Columns.find(Key(ColumnX, ColumnZ));
		if (It == Columns.end())
		{
			return Invalid;
		}
		u32 Best = Invalid;
		for (u32 ID = It->second; ID < Nodes.size(); ID = Nodes[ID].Next)
		{
			if (Nodes[ID].Deleted && !IncludeDeleted)
			{
				continue;
			}
			float Distance = fabsf(Nodes[ID].Position.y - Height);
			if (Distance <= Limit)
			{
				Limit = Distance;
				Best = ID;
			}
		}
		return Best;
	}

	bool Travel(const Fvector& From, const Fvector& To) const
	{
		// SDK uses directional step limits and a motion tester. Check the
		// same step limits plus body-height static rays, excluding vegetation.
		const float Rise = To.y - From.y;
		if (Rise > 1.5f || Rise < -4.f)
		{
			return false;
		}
		for (float Height : {.2f, 1.f, 1.6f})
		{
			Fvector A = From, B = To, Direction, Hit;
			A.y += Height;
			B.y += Height;
			Direction.sub(B, A);
			float Length = Direction.magnitude();
			if (Length > EPS_L && Ray(A, Direction.div(Length), Length - EPS_L, Hit, nullptr, nullptr, false))
			{
				return false;
			}
		}
		return true;
	}

	bool AllowsSurface(const Fvector& Normal, u16 MaterialIndex) const
	{
		const float Angle = rad2deg(acosf(clampr(Normal.y, -1.f, 1.f)));
		return (FilterAngle ? (Angle >= MinAngle && Angle <= MaxAngle) : Normal.y >= .55f) &&
			   (!FilterMaterial || (HasMaterial && MaterialIndex == BrushMaterial));
	}

	void Paint(const Fvector& Hit)
	{
		if (FilterMaterial && !HasMaterial)
		{
			Status = "Capture a surface material before painting with the material filter.";
			return;
		}
		const int CenterX = X(Hit), CenterZ = Z(Hit);
		const int Extent = iCeil(Radius / Header.size);
		xr_vector<u32> Added;
		xr_vector<Node> Candidates;
		xr_hash_map<u64, u32> CandidateColumns;
		// Full deterministic coverage: every cell center inside the disk.
		for (int CX = CenterX - Extent; CX <= CenterX + Extent; ++CX)
		{
			for (int CZ = CenterZ - Extent; CZ <= CenterZ + Extent; ++CZ)
			{
				Fvector P = Fvector().set(Header.aabb.min.x + CX * Header.size, Hit.y + 1.5f, Header.aabb.min.z + CZ * Header.size);
				if (_sqr(P.x - Hit.x) + _sqr(P.z - Hit.z) > Radius * Radius)
				{
					continue;
				}
				Fvector Ground, Normal;
				u16 MaterialIndex = 0;
				if (!Ray(P, Fvector().set(0, -1, 0), 5.5f, Ground, &Normal, &MaterialIndex))
				{
					continue;
				}
				if (!AllowsSurface(Normal, MaterialIndex))
				{
					continue;
				}
				// Reject unsupported edges and abrupt height changes across a cell.
				bool Supported = true;
				for (int I = 0; I < 4; ++I)
				{
					Fvector Sample = P, Surface, SampleNormal;
					u16 SampleMaterial = 0;
					Sample.x += DX[I] * Header.size * .4f;
					Sample.z += DZ[I] * Header.size * .4f;
					if (!Ray(Sample, Fvector().set(0, -1, 0), 5.5f, Surface, &SampleNormal, &SampleMaterial) || fabsf(Surface.y - Ground.y) > Header.size || !AllowsSurface(SampleNormal, SampleMaterial))
					{
						Supported = false;
						break;
					}
				}
				if (!Supported || Nodes.size() >= Invalid - 1)
				{
					continue;
				}
				Node N;
				N.Position = Ground;
				N.Normal = Normal;
				CandidateColumns[Key(CX, CZ)] = u32(Candidates.size());
				Candidates.push_back(N);
			}
		}
		// Optional protection against disconnected islands beyond boundary walls.
		xr_vector<u8> Reachable(ConnectedOnly ? Candidates.size() : 0, 0);
		xr_vector<u32> Queue;
		for (u32 I = 0; ConnectedOnly && I < Candidates.size(); ++I)
		{
			const Fvector& P = Candidates[I].Position;
			const int CX = X(P), CZ = Z(P);
			for (int D = -1; D < 4; ++D)
			{
				const u32 Anchor = Nearest(CX + (D < 0 ? 0 : DX[D]), CZ + (D < 0 ? 0 : DZ[D]), P.y, Header.size);
				if (Anchor < Nodes.size() && Travel(Nodes[Anchor].Position, P))
				{
					Reachable[I] = 1;
					Queue.push_back(I);
					break;
				}
			}
		}
		for (size_t Head = 0; Head < Queue.size(); ++Head)
		{
			const Fvector P = Candidates[Queue[Head]].Position;
			for (u8 D = 0; D < 4; ++D)
			{
				const auto It = CandidateColumns.find(Key(X(P) + DX[D], Z(P) + DZ[D]));
				if (It != CandidateColumns.end() && !Reachable[It->second] && Travel(P, Candidates[It->second].Position))
				{
					Reachable[It->second] = 1;
					Queue.push_back(It->second);
				}
			}
		}
		for (u32 I = 0; I < Candidates.size(); ++I)
		{
			if ((ConnectedOnly && !Reachable[I]) || Nodes.size() >= Invalid - 1)
			{
				continue;
			}
			const auto& Candidate = Candidates[I];
			const u32 Existing = Nearest(X(Candidate.Position), Z(Candidate.Position), Candidate.Position.y, Header.size, true);
			Node N = Existing < Nodes.size() ? Nodes[Existing] : Node{};
			if (N.Deleted)
			{
				--RemovedCount;
			}
			N.Position = Candidate.Position;
			N.Normal = Candidate.Normal;
			N.Modified = true;
			N.Deleted = false;
			N.Vertex.UncompressedNode.plane = pvCompress(N.Normal);
			for (u8 I = 0; I < 4; ++I)
			{
				N.Vertex.link(I, Invalid);
			}
			if (Existing < Nodes.size())
			{
				Nodes[Existing] = N;
				Cache(Existing, false);
				Added.push_back(Existing);
			}
			else
			{
				u32 ID = u32(Nodes.size());
				Nodes.push_back(N);
				Cache(ID);
				Added.push_back(ID);
			}
		}
		if (ConnectedOnly && !Candidates.empty() && Queue.empty())
		{
			Status = "Painting blocked: extend the existing AI grid without crossing walls.";
		}
		for (u32 ID : Added)
		{
			const Fvector P = Nodes[ID].Position;
			for (u8 I = 0; I < 4; ++I)
			{
				u32 Other = Nearest(X(P) + DX[I], Z(P) + DZ[I], P.y, 4.f);
				if (Other == Invalid)
				{
					continue;
				}
				if (Travel(P, Nodes[Other].Position))
				{
					Nodes[ID].Vertex.link(I, Other);
				}
				if (ID >= BaseCount && Other < BaseCount && Nodes[Other].Vertex.link((I + 2) % 4) >= Nodes.size() && Travel(Nodes[Other].Position, P))
				{
					Nodes[Other].Vertex.link((I + 2) % 4, ID);
				}
			}
		}
		Dirty |= !Added.empty();
	}

	void Erase(const Fvector& Hit)
	{
		const int Extent = iCeil(Radius / Header.size);
		xr_hash_set<u32> Removed;
		u32 Protected = 0;
		for (int CX = X(Hit) - Extent; CX <= X(Hit) + Extent; ++CX)
		{
			for (int CZ = Z(Hit) - Extent; CZ <= Z(Hit) + Extent; ++CZ)
			{
				Fvector Origin = Fvector().set(Header.aabb.min.x + CX * Header.size, Hit.y + 1.5f, Header.aabb.min.z + CZ * Header.size);
				if (_sqr(Origin.x - Hit.x) + _sqr(Origin.z - Hit.z) > Radius * Radius)
				{
					continue;
				}
				Fvector Ground;
				if (!Ray(Origin, Fvector().set(0, -1, 0), 5.5f, Ground))
				{
					continue;
				}
				const u32 ID = Nearest(CX, CZ, Ground.y, Header.size * .3f);
				if (ID == Invalid)
				{
					continue;
				}
				if (ProtectedNodes.contains(ID))
				{
					++Protected;
					continue;
				}
				Nodes[ID].Deleted = true;
				++RemovedCount;
				Nodes[ID].Vertex.UncompressedNode.high = {};
				Nodes[ID].Vertex.UncompressedNode.low = {};
				for (u8 D = 0; D < 4; ++D)
				{
					Nodes[ID].Vertex.link(D, Invalid);
				}
				Removed.insert(ID);
				if (Selected == ID)
				{
					Selected = Invalid;
				}
			}
		}
		// Only adjacent X/Z columns can contain incoming directional edges.
		for (u32 ID : Removed)
		{
			const Fvector P = Nodes[ID].Position;
			for (u8 D = 0; D < 4; ++D)
			{
				auto Column = Columns.find(Key(X(P) + DX[D], Z(P) + DZ[D]));
				if (Column == Columns.end())
				{
					continue;
				}
				for (u32 Other = Column->second; Other < Nodes.size(); Other = Nodes[Other].Next)
				{
					for (u8 Link = 0; Link < 4; ++Link)
					{
						if (Nodes[Other].Vertex.link(Link) == ID)
						{
							Nodes[Other].Vertex.link(Link, Invalid);
						}
					}
				}
			}
		}
		Dirty |= !Removed.empty();
		if (Protected)
		{
			Status = "Game graph anchor nodes are retained to preserve level/spawn bindings.";
		}
	}

	void SetSelectedHeight(float Height)
	{
		if (Selected >= Nodes.size() || Nodes[Selected].Deleted || !std::isfinite(Height))
		{
			return;
		}
		auto& N = Nodes[Selected];
		N.Position.y = Height;
		N.Modified = true;
		// X/Z indices and normal stay unchanged; update only the display plane.
		Cache(Selected, false);
		Dirty = true;
		Status = "Selected node height changed. Links preserved; Apply realtime / Overwrite save to commit.";
	}

	void SetSelectedTilt(float AngleX, float AngleZ)
	{
		if (Selected >= Nodes.size() || Nodes[Selected].Deleted || !std::isfinite(AngleX) || !std::isfinite(AngleZ))
		{
			return;
		}
		auto& N = Nodes[Selected];
		// Independent slopes about the world X/Z axes, with an upward normal.
		// Avoid vertical planes, which cannot represent a height-field AI node.
		N.Normal.set(-tanf(deg2rad(clampr(AngleZ, -89.f, 89.f))), 1.f, tanf(deg2rad(clampr(AngleX, -89.f, 89.f))));
		N.Normal.normalize_safe();
		N.Vertex.UncompressedNode.plane = pvCompress(N.Normal);
		N.Modified = true;
		Cache(Selected, false);
		Dirty = true;
		Status = "Selected node tilt changed. Position and links preserved; Apply realtime / Overwrite save to commit.";
	}

	void DeleteSelectedNode()
	{
		if (Selected >= Nodes.size() || Nodes[Selected].Deleted)
		{
			return;
		}
		if (ProtectedNodes.contains(Selected))
		{
			Status = "Cannot delete a game graph anchor: level/spawn bindings must be preserved.";
			return;
		}
		const u32 ID = Selected;
		auto& N = Nodes[ID];
		N.Deleted = true;
		N.Modified = true;
		N.Vertex.UncompressedNode.high = {};
		N.Vertex.UncompressedNode.low = {};
		for (auto& Other : Nodes)
		{
			for (u8 D = 0; D < 4; ++D)
			{
				if (&Other == &N || Other.Vertex.link(D) == ID)
				{
					Other.Vertex.link(D, Invalid);
				}
			}
		}
		++RemovedCount;
		Selected = Invalid;
		Dirty = true;
		Status = "Selected node deleted; incoming links cleared. Apply realtime / Overwrite save to commit.";
	}

	void DeleteInvalidNodes()
	{
		const u32 Count = u32(Nodes.size());
		xr_vector<u32> Parents(Count);
		xr_vector<u8> Ranks(Count, 0);
		xr_vector<u8> Links(Count, 0);
		xr_vector<u8> HasFullNode(Count, 0);
		for (u32 I = 0; I < Count; ++I)
		{
			Parents[I] = I;
		}
		auto Root = [&](u32 ID)
		{
			while (Parents[ID] != ID)
			{
				Parents[ID] = Parents[Parents[ID]];
				ID = Parents[ID];
			}
			return ID;
		};
		// Components ignore link direction: a one-way edge still connects nodes.
		for (u32 I = 0; I < Count; ++I)
		{
			if (Nodes[I].Deleted)
			{
				continue;
			}
			for (u8 D = 0; D < 4; ++D)
			{
				const u32 Other = Nodes[I].Vertex.link(D);
				if (Other >= Count || Other == I || Nodes[Other].Deleted)
				{
					continue;
				}
				++Links[I];
				u32 A = Root(I), B = Root(Other);
				if (A == B)
				{
					continue;
				}
				if (Ranks[A] < Ranks[B])
				{
					std::swap(A, B);
				}
				Parents[B] = A;
				if (Ranks[A] == Ranks[B])
				{
					++Ranks[A];
				}
			}
		}
		for (u32 I = 0; I < Count; ++I)
		{
			if (!Nodes[I].Deleted && Links[I] == 4)
			{
				HasFullNode[Root(I)] = 1;
			}
		}
		u32 Removed = 0, Protected = 0;
		// Classify once, before deleting; do not erode valid boundaries repeatedly.
		for (u32 I = 0; I < Count; ++I)
		{
			if (Nodes[I].Deleted || (Links[I] != 0 && HasFullNode[Root(I)]))
			{
				continue;
			}
			if (ProtectedNodes.contains(I))
			{
				++Protected;
				continue;
			}
			Nodes[I].Deleted = true;
			Nodes[I].Modified = true;
			Nodes[I].Vertex.UncompressedNode.high = {};
			Nodes[I].Vertex.UncompressedNode.low = {};
			++Removed;
			if (Selected == I)
			{
				Selected = Invalid;
			}
		}
		// Clear every incoming edge, including unusual links outside adjacent cells.
		for (auto& N : Nodes)
		{
			for (u8 D = 0; D < 4; ++D)
			{
				const u32 Other = N.Vertex.link(D);
				if (N.Deleted || (Other < Count && Nodes[Other].Deleted))
				{
					N.Vertex.link(D, Invalid);
				}
			}
		}
		RemovedCount += Removed;
		Dirty |= Removed != 0;
		string256 Message;
		xr_sprintf(Message, "Removed %u invalid nodes; preserved %u game graph anchors. Apply realtime / Overwrite save to commit.", Removed, Protected);
		Status = Message;
	}

	void DeleteNodesWithoutPathToSelected()
	{
		const u32 Count = u32(Nodes.size());
		if (Selected >= Count || Nodes[Selected].Deleted)
		{
			Status = "Select a node on the main outdoor grid first (Edit individual node).";
			return;
		}
		const u32 Reference = Selected;
		// Reverse adjacency permits a linear traversal of all paths TO the anchor,
		// including one-way links, rather than paths leading away from it.
		xr_vector<u32> Offsets(size_t(Count) + 1, 0);
		for (const auto& N : Nodes)
		{
			if (N.Deleted)
			{
				continue;
			}
			for (u8 D = 0; D < 4; ++D)
			{
				const u32 Other = N.Vertex.link(D);
				if (Other < Count && !Nodes[Other].Deleted)
				{
					++Offsets[Other + 1];
				}
			}
		}
		for (u32 I = 0; I < Count; ++I)
		{
			Offsets[I + 1] += Offsets[I];
		}
		xr_vector<u32> Incoming(Offsets.back());
		{
			xr_vector<u32> Cursor = Offsets;
			for (u32 I = 0; I < Count; ++I)
			{
				if (Nodes[I].Deleted)
				{
					continue;
				}
				for (u8 D = 0; D < 4; ++D)
				{
					const u32 Other = Nodes[I].Vertex.link(D);
					if (Other < Count && !Nodes[Other].Deleted)
					{
						Incoming[Cursor[Other]++] = I;
					}
				}
			}
		}
		xr_vector<u8> ReachesReference(Count, 0);
		xr_vector<u32> Queue;
		Queue.push_back(Reference);
		ReachesReference[Reference] = 1;
		for (size_t Head = 0; Head < Queue.size(); ++Head)
		{
			const u32 ID = Queue[Head];
			for (u32 I = Offsets[ID]; I < Offsets[ID + 1]; ++I)
			{
				const u32 Other = Incoming[I];
				if (!ReachesReference[Other])
				{
					ReachesReference[Other] = 1;
					Queue.push_back(Other);
				}
			}
		}
		u32 Removed = 0, Protected = 0;
		for (u32 I = 0; I < Count; ++I)
		{
			if (Nodes[I].Deleted || ReachesReference[I])
			{
				continue;
			}
			if (ProtectedNodes.contains(I))
			{
				++Protected;
				continue;
			}
			Nodes[I].Deleted = true;
			Nodes[I].Modified = true;
			Nodes[I].Vertex.UncompressedNode.high = {};
			Nodes[I].Vertex.UncompressedNode.low = {};
			++Removed;
		}
		for (auto& N : Nodes)
		{
			for (u8 D = 0; D < 4; ++D)
			{
				const u32 Other = N.Vertex.link(D);
				if (N.Deleted || (Other < Count && Nodes[Other].Deleted))
				{
					N.Vertex.link(D, Invalid);
				}
			}
		}
		RemovedCount += Removed;
		Dirty |= Removed != 0;
		string256 Message;
		xr_sprintf(Message, "Removed %u nodes without a path to node %u; preserved %u game graph anchors. Apply realtime / Overwrite save to commit.", Removed, Reference, Protected);
		Status = Message;
	}

	xr_vector<u8> DeletionMask() const
	{
		xr_vector<u8> Result;
		Result.reserve(Nodes.size());
		for (const Node& N : Nodes)
		{
			Result.push_back(N.Deleted ? 1 : 0);
		}
		return Result;
	}

	bool Pack(hdrNODES& OutHeader, xr_vector<ILevelGraph::CVertex>& Output)
	{
		if (!SourceFormatVerified)
		{
			Status = "Source level.ai has an unknown binary layout; refusing incompatible save/export.";
			return false;
		}
		LevelGraphFormat::Limits Format;
		if (!LevelGraphFormat::GetLimits(Header.version, Format))
		{
			Status = "Unsupported AI grid format; saving/conversion is disabled.";
			return false;
		}
		if (Nodes.size() >= Format.InvalidLink)
		{
			Status = "Node count exceeds the current AI grid format capacity.";
			return false;
		}
		if (std::none_of(Nodes.begin(), Nodes.end(), [](const Node& N)
						 { return !N.Deleted; }))
		{
			Status = "At least one active AI node must remain.";
			return false;
		}
		OutHeader = Header;
		OutHeader.count = u32(Nodes.size());
		for (const Node& N : Nodes)
		{
			OutHeader.aabb.modify(N.Position);
		}
		// Preserve the original grid origin modulo cell size.
		OutHeader.aabb.min.x = Header.aabb.min.x + floorf((OutHeader.aabb.min.x - Header.aabb.min.x) / Header.size) * Header.size;
		OutHeader.aabb.min.z = Header.aabb.min.z + floorf((OutHeader.aabb.min.z - Header.aabb.min.z) / Header.size) * Header.size;
		OutHeader.size_y = std::max(Header.size_y, OutHeader.aabb.max.y - OutHeader.aabb.min.y + EPS_L);
		const u32 Row = iFloor((OutHeader.aabb.max.z - OutHeader.aabb.min.z) / Header.size + EPS_L + 1.5f);
		const u64 MaxX = u64(iFloor((OutHeader.aabb.max.x - OutHeader.aabb.min.x) / Header.size + .5f));
		const u64 MaxZ = u64(iFloor((OutHeader.aabb.max.z - OutHeader.aabb.min.z) / Header.size + .5f));
		if (MaxX * Row + MaxZ >= MAX_AI_NODES || MaxX * Row + MaxZ > Format.MaxXZ)
		{
			Status = "Grid bounds exceed the engine coordinate limit.";
			return false;
		}
		Output.reserve(Nodes.size());
		for (const Node& N : Nodes)
		{
			const u64 CX = u64(iFloor((N.Position.x - OutHeader.aabb.min.x) / Header.size + .5f));
			const u64 CZ = u64(iFloor((N.Position.z - OutHeader.aabb.min.z) / Header.size + .5f));
			if (CX * Row + CZ >= MAX_AI_NODES || CX * Row + CZ > Format.MaxXZ)
			{
				Status = "Grid bounds exceed the engine coordinate limit.";
				return false;
			}
			auto V = N.Vertex;
			for (u8 D = 0; D < 4; ++D)
			{
				const u32 Link = V.link(D);
				if (N.Deleted || (Link < Nodes.size() && Nodes[Link].Deleted))
				{
					V.link(D, Invalid);
				}
			}
			// Unchanged bounding box: preserve original quantized coordinates exactly.
			if (N.Modified || OutHeader.aabb.min.x != Header.aabb.min.x || OutHeader.aabb.min.z != Header.aabb.min.z || Row != iFloor((Header.aabb.max.z - Header.aabb.min.z) / Header.size + EPS_L + 1.5f) ||
				OutHeader.aabb.min.y != Header.aabb.min.y || OutHeader.size_y != Header.size_y || Output.size() >= BaseCount)
			{
				V.UncompressedNode.p.xz(u32(CX * Row + CZ));
				V.UncompressedNode.p.y(u16(clampr(iFloor(65535.f * (N.Position.y - OutHeader.aabb.min.y) / OutHeader.size_y + EPS_S), 0, 65535)));
			}
			Output.push_back(V);
		}
		return true;
	}

	bool BuildCells()
	{
		if (!ai().get_cross_table())
		{
			Status = "No cross table in this level.";
			return false;
		}
		const auto& Table = ai().cross_table();
		const u32 Existing = Table.header().level_vertex_count();
		if (!Existing)
		{
			Status = "An existing cross-table area is required.";
			return false;
		}
		Cells.resize(Nodes.size());
		xr_vector<bool> Assigned(Nodes.size(), false);
		for (u32 I = 0; I < Existing && I < Nodes.size(); ++I)
		{
			Cells[I] = Table.vertex(I);
			Assigned[I] = true;
		}
		// Multi-source Dijkstra into new nodes; old cells retain exact ownership.
		using Entry = xr_pair<float, u32>;
		xr_vector<Entry> Queue;
		auto Compare = [](const Entry& A, const Entry& B)
		{ return A.first > B.first; };
		if (Existing >= Nodes.size())
		{
			return true;
		}
		xr_vector<xr_vector<u32>> Adjacent(Nodes.size() - Existing);
		auto Relax = [&](u32 I, u32 J)
		{
			if (J < Existing)
			{
				return;
			}
			float Distance = Cells[I].fDistance + Nodes[I].Position.distance_to(Nodes[J].Position);
			if (!Assigned[J] || Distance < Cells[J].fDistance)
			{
				Cells[J] = Cells[I];
				Cells[J].fDistance = Distance;
				Assigned[J] = true;
				Queue.emplace_back(Distance, J);
				std::push_heap(Queue.begin(), Queue.end(), Compare);
			}
		};
		for (u32 I = 0; I < Nodes.size(); ++I)
		{
			if (Nodes[I].Deleted)
			{
				continue;
			}
			for (u8 D = 0; D < 4; ++D)
			{
				u32 J = Nodes[I].Vertex.link(D);
				if (J >= Nodes.size() || Nodes[J].Deleted)
				{
					continue;
				}
				if (I >= Existing && J >= Existing)
				{
					Adjacent[I - Existing].push_back(J);
					Adjacent[J - Existing].push_back(I);
				}
				else if (I < Existing && J >= Existing)
				{
					Relax(I, J);
				}
				else if (I >= Existing && J < Existing)
				{
					Relax(J, I);
				}
			}
		}
		auto Propagate = [&]()
		{
			while (!Queue.empty())
			{
				std::pop_heap(Queue.begin(), Queue.end(), Compare);
				Entry Current = Queue.back();
				Queue.pop_back();
				if (Current.first != Cells[Current.second].fDistance)
				{
					continue;
				}
				for (u32 J : Adjacent[Current.second - Existing])
				{
					Relax(Current.second, J);
				}
			}
		};
		Propagate();
		// A disconnected island still needs cross-table ownership, but must not
		// gain artificial navigation edges across walls or gaps.
		using TileCandidate = xr_pair<float, const xr_vector<u32>*>;
		xr_vector<TileCandidate> TileQueue;
		auto TileCompare = [](const TileCandidate& A, const TileCandidate& B)
		{ return A.first > B.first; };
		for (u32 Seed = Existing; Seed < Nodes.size(); ++Seed)
		{
			if (Nodes[Seed].Deleted)
			{
				Cells[Seed] = Cells[0];
				continue;
			}
			if (Assigned[Seed])
			{
				continue;
			}
			const Fvector Position = Nodes[Seed].Position;
			TileQueue.clear();
			for (const auto& Tile : Tiles)
			{
				const float MinX = float(s32(u32(Tile.first >> 32))) * 16.f;
				const float MinZ = float(s32(u32(Tile.first))) * 16.f;
				const float OffsetX = Position.x - clampr(Position.x, MinX, MinX + 16.f);
				const float OffsetZ = Position.z - clampr(Position.z, MinZ, MinZ + 16.f);
				TileQueue.emplace_back(_sqr(OffsetX) + _sqr(OffsetZ), &Tile.second);
			}
			std::make_heap(TileQueue.begin(), TileQueue.end(), TileCompare);
			u32 Anchor = Invalid;
			float DistanceSquared = flt_max;
			while (!TileQueue.empty())
			{
				std::pop_heap(TileQueue.begin(), TileQueue.end(), TileCompare);
				const auto Candidate = TileQueue.back();
				TileQueue.pop_back();
				if (Candidate.first > DistanceSquared)
				{
					break;
				}
				for (u32 ID : *Candidate.second)
				{
					if (ID >= Existing || Nodes[ID].Deleted)
					{
						continue;
					}
					const float Distance = Nodes[ID].Position.distance_to_sqr(Position);
					if (Distance < DistanceSquared || (Distance == DistanceSquared && ID < Anchor))
					{
						DistanceSquared = Distance;
						Anchor = ID;
					}
				}
			}
			if (Anchor == Invalid)
			{
				Status = "No existing cross-table area is available for the new grid.";
				return false;
			}
			Cells[Seed] = Cells[Anchor];
			Cells[Seed].fDistance += sqrtf(DistanceSquared);
			Assigned[Seed] = true;
			Queue.emplace_back(Cells[Seed].fDistance, Seed);
			std::push_heap(Queue.begin(), Queue.end(), Compare);
			Propagate();
		}
		return true;
	}

	void OnFrame() override
	{
		if (!g_pGameLevel || ai().get_level_graph() != Graph || (Graph && !(BaseGUID == Graph->header().guid())))
		{
			Reset();
			return;
		}
		if (!Pending)
		{
			return;
		}
		Pending = false;
		const bool Reverting = PendingRevert;
		PendingRevert = false;
		hdrNODES NewHeader;
		xr_vector<ILevelGraph::CVertex> Packed;
		if (!Pack(NewHeader, Packed))
		{
			return;
		}
		if (Reverting && RevertCells.size() == Nodes.size())
		{
			Cells = RevertCells;
		}
		else if (!BuildCells())
		{
			return;
		}
		RevertCells.clear();
		auto* Runtime = dynamic_cast<CLevelGraph*>(&ai().level_graph());
		if (!Runtime)
		{
			Status = "Runtime level graph is unavailable.";
			return;
		}
		Runtime->ApplyEditorNodes(NewHeader, Packed, DeletionMask());
		const_cast<IGameLevelCrossTable&>(ai().cross_table()).ApplyEditorCells(Cells);
		const_cast<CCoverManager&>(ai().cover_manager()).RefreshStaticCoverForEditor();
		Status = Reverting ? "Reverted live AI to the last saved grid." : "Applied to live AI. Existing node IDs and game graph bindings preserved.";
	}

	void Save();
	bool Export();
	void RecoverExport();
	void RevertToSaved();
	void Draw(const Fvector* Hit, const Fvector* Normal);
};

static xr_unique_ptr<Editor>& EditorInstance()
{
	static xr_unique_ptr<Editor> Instance;
	return Instance;
}

static Editor& GetEditor()
{
	auto& Instance = EditorInstance();
	if (!Instance)
	{
		Instance = xr_make_unique<Editor>();
	}
	return *Instance;
}

static void PatchPath(string_path& Path)
{
	FS.update_path(Path, "$level$", "level.ai.editor");
}

static bool WriteRegistered(CMemoryWriter& Data, const char* Path)
{
	{
		auto Output = FS.wg_open(Path);
		if (!Output || !Output->valid())
		{
			return false;
		}
		Output->w(Data.pointer(), Data.size());
		Output->flush();
		if (!Output->valid() || Output->tell() != Data.size())
		{
			return false;
		}
	}
	// w_close defers registration to the asynchronous watcher when active.
	// Refresh explicitly before read-back verification in this same frame.
	FS.file_update(Path);
	return true;
}

static bool CopyRegistered(const char* Source, const char* Destination)
{
	auto Input = FS.rg_open(Source);
	if (!Input)
	{
		return false;
	}
	CMemoryWriter Copy;
	Copy.w(Input->pointer(), Input->length());
	return WriteRegistered(Copy, Destination);
}

void Editor::Save()
{
	hdrNODES NewHeader;
	xr_vector<ILevelGraph::CVertex> Packed;
	if (!Pack(NewHeader, Packed) || !BuildCells())
	{
		return;
	}
	CMemoryWriter Writer;
	Writer.w_u32(Magic);
	Writer.w(&BaseGUID, sizeof(BaseGUID));
	Writer.w(&ai().cross_table().header().game_guid(), sizeof(xrGUID));
	Writer.w(&NewHeader, sizeof(NewHeader));
	Writer.w(Packed.data(), Packed.size() * sizeof(Packed[0]));
	Writer.w(Cells.data(), Cells.size() * sizeof(Cell));
	const auto Deleted = DeletionMask();
	Writer.w(Deleted.data(), Deleted.size());
	string_path Path;
	PatchPath(Path);
	xr_string Temporary = xr_string(Path) + ".tmp", Backup = xr_string(Path) + ".bak";
	if (!WriteRegistered(Writer, Temporary.c_str()))
	{
		Status = "Cannot write AI grid patch.";
		return;
	}
	IReader* Check = FS.r_open(Temporary.c_str());
	bool Valid = Check && Check->length() == Writer.size() && !memcmp(Check->pointer(), Writer.pointer(), Writer.size());
	if (Check)
	{
		FS.r_close(Check);
	}
	if (!Valid)
	{
		Status = "Patch verification failed; previous grid preserved.";
		return;
	}
	if (FS.exist(Path))
	{
		if (!CopyRegistered(Path, Backup.c_str()))
		{
			Status = "Cannot write backup; previous grid preserved.";
			return;
		}
		IReader* Previous = FS.r_open(Path);
		IReader* SavedBackup = FS.r_open(Backup.c_str());
		Valid = Previous && SavedBackup && Previous->length() == SavedBackup->length() &&
				!memcmp(Previous->pointer(), SavedBackup->pointer(), Previous->length());
		if (Previous)
		{
			FS.r_close(Previous);
		}
		if (SavedBackup)
		{
			FS.r_close(SavedBackup);
		}
		if (!Valid)
		{
			Status = "Backup verification failed; previous grid preserved.";
			return;
		}
	}
	// Publish with the checked writer rather than relying on watcher-driven
	// rename registration. Keep the verified temporary file until completion.
	const bool Written = WriteRegistered(Writer, Path);
	Check = FS.r_open(Path);
	Valid = Written && Check && Check->length() == Writer.size() && !memcmp(Check->pointer(), Writer.pointer(), Writer.size());
	if (Check)
	{
		FS.r_close(Check);
	}
	if (!Valid)
	{
		if (FS.exist(Backup.c_str()))
		{
			const bool Copied = CopyRegistered(Backup.c_str(), Path);
			IReader* Restored = FS.r_open(Path);
			IReader* SavedBackup = FS.r_open(Backup.c_str());
			const bool RestoredOK = Copied && Restored && SavedBackup && Restored->length() == SavedBackup->length() &&
									!memcmp(Restored->pointer(), SavedBackup->pointer(), Restored->length());
			if (Restored)
			{
				FS.r_close(Restored);
			}
			if (SavedBackup)
			{
				FS.r_close(SavedBackup);
			}
			Status = RestoredOK ? "Save failed; previous patch restored. Verified new data remains in .tmp." : "Save failed; restore failed. Previous .bak and new .tmp retained.";
		}
		else
		{
			Status = "Save failed; verified new data remains in .tmp.";
		}
		return;
	}
	FS.file_delete(Temporary.c_str());
	const bool Exported = Export();
	if (!Exported)
	{
		Status = xr_string("Restart patch saved, but export failed: ") + Status;
		return;
	}
	Dirty = false;
	Status = xr_string("Saved grid files. Restart file: ") + Path + ". Automatically restored on level load.";
	Msg("* AI grid editor: saved %u nodes (AI format %u) to %s", NewHeader.count, NewHeader.version, Path);
}

bool Editor::Export()
{
	hdrNODES ExportHeader;
	xr_vector<ILevelGraph::CVertex> Packed;
	if (!Pack(ExportHeader, Packed))
	{
		return false;
	}
	LevelGraphFormat::Limits Format;
	if (!LevelGraphFormat::GetLimits(ExportHeader.version, Format))
	{
		return false;
	}
	xr_vector<u32> Order;
	Order.reserve(Packed.size());
	xr_vector<u32> Remap(Packed.size(), u32(-1));
	for (u32 I = 0; I < Packed.size(); ++I)
	{
		if (!Nodes[I].Deleted)
		{
			Order.push_back(I);
		}
	}
	std::stable_sort(Order.begin(), Order.end(), [&](u32 A, u32 B)
					 { return Packed[A].position().xz() < Packed[B].position().xz(); });
	for (u32 I = 0; I < Order.size(); ++I)
	{
		Remap[Order[I]] = I;
	}
	// Keep the loaded format. Sorting changes IDs, so dependent compiled assets
	// still need rebuilding; a new GUID identifies this exported graph.
	ExportHeader.guid = generate_guid();
	ExportHeader.count = u32(Order.size());
	CMemoryWriter Native, Mapping, Source;
	Native.w(&ExportHeader, sizeof(ExportHeader));
	Mapping.w_u32(u32(Remap.size()));
	Mapping.w(Remap.data(), Remap.size() * sizeof(u32));
	// This bundled xrAI emits version 10 from aimap v1 and version 11 from v2.
	// It cannot compile a version 8/13 map without converting the format.
	bool SDKAvailable = ExportHeader.version == XRAI_MINIMAL_VERSION || ExportHeader.version == XRAI_CURRENT_VERSION;
	const bool SDKChunkOpened = SDKAvailable;
	const bool Legacy = ExportHeader.version == XRAI_MINIMAL_VERSION;
	Fbox SourceBox = ExportHeader.aabb;
	SourceBox.max.y = SourceBox.min.y + ExportHeader.size_y;
	SAIParams Parameters;
	Parameters.fPatchSize = Header.size;
	if (SDKAvailable)
	{
		Source.open_chunk(E_AIMAP_CHUNK_VERSION);
		Source.w_u16(Legacy ? 1 : E_AIMAP_VERSION);
		Source.close_chunk();
		Source.open_chunk(E_AIMAP_CHUNK_BOX);
		Source.w(&SourceBox, sizeof(SourceBox));
		Source.close_chunk();
		Source.open_chunk(E_AIMAP_CHUNK_PARAMS);
		Source.w(&Parameters, sizeof(Parameters));
		Source.close_chunk();
		Source.open_chunk(E_AIMAP_CHUNK_NODES);
		Source.w_u32(u32(Order.size()));
	}
	for (u32 ID : Order)
	{
		const auto& V = Packed[ID];
		u32 Links[4];
		for (u8 D = 0; D < 4; ++D)
		{
			u32 Link = V.link(D);
			Links[D] = Link < Remap.size() && Remap[Link] != u32(-1) ? Remap[Link] : Format.InvalidLink;
		}
		const u8 Reserved = ID < ReservedBytes.size() ? ReservedBytes[ID] : 0;
		LevelGraphFormat::WriteNode(Native, ExportHeader.version, V, Links, Reserved);
		if (!SDKAvailable)
		{
			continue;
		}
		const Fvector P = Nodes[ID].Position;
		const int CX = iFloor(P.x / Header.size + .5f), CZ = iFloor(P.z / Header.size + .5f);
		if (CX < -32767 || CX > 32767 || CZ < -32767 || CZ > 32767 ||
			fabsf(P.x - CX * Header.size) > Header.size * .001f || fabsf(P.z - CZ * Header.size) > Header.size * .001f)
		{
			SDKAvailable = false;
			continue;
		}
		for (u8 D = 0; D < 4; ++D)
		{
			const u32 Link = V.link(D);
			const u32 SourceLink = Link < Remap.size() && Remap[Link] != u32(-1) ? Remap[Link] : (Legacy ? 0x00ffffff : u32(-1));
			if (Legacy)
			{
				Source.w(&SourceLink, 3);
			}
			else
			{
				Source.w_u32(SourceLink);
			}
		}
		SNodePositionOld Position{s16(CX), u16(V.position().y()), s16(CZ)};
		Source.w_u16(V.plane());
		Source.w(&Position, sizeof(Position));
	}
	if (SDKChunkOpened)
	{
		Source.close_chunk();
	}
	string_path NativePath, SourcePath, MapPath;
	FS.update_path(NativePath, "$level$", "level.ai.compiled");
	FS.update_path(SourcePath, "$level$", "build.aimap.editor");
	FS.update_path(MapPath, "$level$", "level.ai.compiled.remap");
	if (!WriteRegistered(Native, NativePath) || !WriteRegistered(Mapping, MapPath))
	{
		Status = "Cannot write native AI grid export.";
		return false;
	}
	if (SDKAvailable && !WriteRegistered(Source, SourcePath))
	{
		Status = "Native grid exported, but SDK source could not be saved.";
		return false;
	}
	if (!SDKAvailable && FS.exist(SourcePath))
	{
		FS.file_delete(SourcePath);
	}
	string512 Message;
	xr_sprintf(Message, "Exported native AI grid version %u + ID remap. %s Rebuild dependent game graph/cross table/spawn before installing.", ExportHeader.version, SDKAvailable ? "Matching SDK source exported." : "No matching SDK source: bundled xrAI only supports v10/v11 and the global s16 grid.");
	Status = Message;
	Msg("* AI grid editor: %s", Message);
	return true;
}

void Editor::RevertToSaved()
{
	string_path Path;
	PatchPath(Path);
	const bool Patch = FS.exist(Path) != nullptr;
	if (!Patch)
	{
		string_path ExportPath, RemapPath;
		FS.update_path(ExportPath, "$level$", "level.ai.compiled");
		FS.update_path(RemapPath, "$level$", "level.ai.compiled.remap");
		if (FS.exist(ExportPath) && FS.exist(RemapPath))
		{
			RecoverExport();
			return;
		}
		FS.update_path(Path, "$level$", "level.ai");
	}
	auto Input = FS.rg_open(Path);
	if (!Input)
	{
		Status = "Cannot open the last saved grid.";
		return;
	}
	hdrNODES Saved{};
	xr_vector<ILevelGraph::CVertex> Packed;
	xr_vector<u8> Deleted;
	xr_vector<Cell> SavedCells;
	bool HasMask = false;
	const u32 Prefix = Patch ? sizeof(u32) + 2 * sizeof(xrGUID) + sizeof(Saved) : sizeof(Saved);
	if (Input->length() < Prefix)
	{
		Status = "Saved grid is truncated; current work retained.";
		return;
	}
	if (Patch)
	{
		const u32 FileMagic = Input->r_u32();
		HasMask = FileMagic == Magic;
		xrGUID LevelGUID{}, GameGUID{};
		Input->r(&LevelGUID, sizeof(LevelGUID));
		Input->r(&GameGUID, sizeof(GameGUID));
		if ((!HasMask && FileMagic != LegacyMagic) || !(LevelGUID == BaseGUID) || !(GameGUID == ai().cross_table().header().game_guid()))
		{
			Status = "Saved patch belongs to a different level/game graph.";
			return;
		}
	}
	Input->r(&Saved, sizeof(Saved));
	LevelGraphFormat::Limits Format;
	if (!LevelGraphFormat::GetLimits(Saved.version, Format) || Saved.version != Header.version || Saved.size != Header.size ||
		!(Saved.guid == BaseGUID) || Saved.count < SourceCount || Saved.count >= Format.InvalidLink ||
		!std::isfinite(Saved.size_y) || Saved.size_y <= 0 ||
		u64(Input->length()) != Prefix + u64(Saved.count) * (Patch ? sizeof(ILevelGraph::CVertex) + sizeof(Cell) + (HasMask ? 1 : 0) : Format.RecordSize))
	{
		Status = "Saved grid format does not match the loaded level.";
		return;
	}
	for (u32 D = 0; D < 3; ++D)
	{
		if (!std::isfinite(Saved.aabb.min[D]) || !std::isfinite(Saved.aabb.max[D]) || Saved.aabb.min[D] > Saved.aabb.max[D])
		{
			Status = "Invalid saved bounds.";
			return;
		}
	}
	const double RowValue = (double(Saved.aabb.max.z) - Saved.aabb.min.z) / Saved.size + EPS_L + 1.5;
	if (RowValue < 1 || RowValue >= MAX_AI_NODES)
	{
		Status = "Invalid saved grid row length.";
		return;
	}
	const u32 Row = iFloor((Saved.aabb.max.z - Saved.aabb.min.z) / Saved.size + EPS_L + 1.5f);
	Packed.resize(Saved.count);
	Deleted.assign(Saved.count, 0);
	if (Patch)
	{
		Input->r(Packed.data(), Packed.size() * sizeof(Packed[0]));
		SavedCells.resize(Saved.count);
		Input->r(SavedCells.data(), SavedCells.size() * sizeof(Cell));
		for (const Cell& C : SavedCells)
		{
			if (C.tGraphIndex >= ai().cross_table().header().game_vertex_count() || !std::isfinite(C.fDistance) || C.fDistance < 0)
			{
				Status = "Invalid saved cross table.";
				return;
			}
		}
		if (HasMask)
		{
			Input->r(Deleted.data(), Deleted.size());
		}
	}
	else
	{
		for (auto& V : Packed)
		{
			LevelGraphFormat::ReadNode(*Input, Saved.version, V);
		}
	}
	xr_vector<Node> Restored;
	Restored.reserve(std::max(Saved.count, Graph->header().vertex_count()));
	for (u32 I = 0; I < Saved.count; ++I)
	{
		if (Deleted[I] > 1 || (Deleted[I] && ProtectedNodes.contains(I)) || Packed[I].position().xz() >= MAX_AI_NODES)
		{
			Status = "Invalid deletion mask or coordinates in saved grid.";
			return;
		}
		Node N;
		N.Vertex = Packed[I];
		N.Deleted = Deleted[I] != 0;
		N.Position.set(float(N.Vertex.position().xz() / Row) * Saved.size + Saved.aabb.min.x, float(N.Vertex.position().y()) / 65535.f * Saved.size_y + Saved.aabb.min.y, float(N.Vertex.position().xz() % Row) * Saved.size + Saved.aabb.min.z);
		pvDecompress(N.Normal, N.Vertex.plane());
		for (u8 D = 0; D < 4; ++D)
		{
			const u32 Link = N.Vertex.link(D);
			if (Link >= Saved.count)
			{
				N.Vertex.link(D, Invalid);
			}
			else if (N.Deleted || Deleted[Link])
			{
				N.Vertex.link(D, Invalid);
			}
		}
		Restored.push_back(N);
	}
	// Retain extra live IDs as deleted slots; un-applied new slots can be dropped.
	for (u32 I = Saved.count; I < Graph->header().vertex_count(); ++I)
	{
		Node N;
		N.Vertex = *Graph->vertex(I);
		N.Position = Graph->vertex_position(I);
		N.Deleted = true;
		N.Modified = true;
		pvDecompress(N.Normal, N.Vertex.plane());
		for (u8 D = 0; D < 4; ++D)
		{
			N.Vertex.link(D, Invalid);
		}
		Restored.push_back(N);
	}
	if (Patch)
	{
		for (u32 I = Saved.count; I < Restored.size(); ++I)
		{
			SavedCells.push_back(ai().cross_table().vertex(I));
		}
	}
	Header = Saved;
	Nodes.swap(Restored);
	RevertCells.swap(SavedCells);
	Columns.clear();
	Tiles.clear();
	RemovedCount = 0;
	for (u32 I = 0; I < Nodes.size(); ++I)
	{
		Cache(I);
	}
	ReadReservedBytes();
	Selected = Invalid;
	Pending = true;
	PendingRevert = true;
	Dirty = false;
	HasGround = false;
	LastStroke.set(flt_max, flt_max, flt_max);
	Status = "Saved grid restored; live rollback queued for the next frame.";
}

void Editor::RecoverExport()
{
	string_path NativePath, MapPath;
	FS.update_path(NativePath, "$level$", "level.ai.compiled");
	FS.update_path(MapPath, "$level$", "level.ai.compiled.remap");
	auto Native = FS.rg_open(NativePath);
	auto Mapping = FS.rg_open(MapPath);
	if (!Native || !Mapping || Native->length() < sizeof(hdrNODES) || Mapping->length() < sizeof(u32))
	{
		Status = "Native export and matching .remap are required for recovery.";
		return;
	}
	hdrNODES Saved;
	Native->r(&Saved, sizeof(Saved));
	const u32 WorkingCount = Mapping->r_u32();
	LevelGraphFormat::Limits Format;
	if (!LevelGraphFormat::GetLimits(Saved.version, Format) || Saved.version != Header.version || Saved.size != Header.size ||
		WorkingCount < SourceCount || WorkingCount < Saved.count || WorkingCount >= Format.InvalidLink || Saved.count >= Format.InvalidLink ||
		u64(Native->length()) != sizeof(Saved) + u64(Saved.count) * Format.RecordSize ||
		u64(Mapping->length()) != sizeof(u32) + u64(WorkingCount) * sizeof(u32) ||
		!std::isfinite(Saved.size_y) || Saved.size_y <= 0)
	{
		Status = "Export version, node count or remap does not match this level.";
		return;
	}
	for (u32 D = 0; D < 3; ++D)
	{
		if (!std::isfinite(Saved.aabb.min[D]) || !std::isfinite(Saved.aabb.max[D]) || Saved.aabb.min[D] > Saved.aabb.max[D])
		{
			Status = "Invalid export bounds.";
			return;
		}
	}
	xr_vector<u32> Remap(WorkingCount), Inverse(Saved.count, Invalid);
	Mapping->r(Remap.data(), Remap.size() * sizeof(u32));
	for (u32 I = 0; I < WorkingCount; ++I)
	{
		if (Remap[I] == u32(-1))
		{
			if (ProtectedNodes.contains(I))
			{
				Status = "Export removed a game graph anchor.";
				return;
			}
			continue;
		}
		if (Remap[I] >= Saved.count || Inverse[Remap[I]] != Invalid)
		{
			Status = "Invalid export ID remap.";
			return;
		}
		Inverse[Remap[I]] = I;
	}
	if (std::find(Inverse.begin(), Inverse.end(), Invalid) != Inverse.end())
	{
		Status = "Incomplete export remap.";
		return;
	}
	const double RowValue = (double(Saved.aabb.max.z) - Saved.aabb.min.z) / Saved.size + EPS_L + 1.5;
	if (RowValue < 1 || RowValue >= MAX_AI_NODES)
	{
		Status = "Invalid export row length.";
		return;
	}
	const u32 Row = iFloor((Saved.aabb.max.z - Saved.aabb.min.z) / Saved.size + EPS_L + 1.5f);
	xr_vector<Node> Recovered(std::max(WorkingCount, Graph->header().vertex_count()));
	for (u32 I = 0; I < Recovered.size(); ++I)
	{
		Node& N = Recovered[I];
		N.Position = I < Nodes.size() ? Nodes[I].Position : Nodes[0].Position;
		N.Normal.set(0.f, 1.f, 0.f);
		N.Vertex.UncompressedNode.plane = pvCompress(N.Normal);
		N.Deleted = true;
		N.Modified = true;
		for (u8 D = 0; D < 4; ++D)
		{
			N.Vertex.link(D, Invalid);
		}
	}
	for (u32 SortedID = 0; SortedID < Saved.count; ++SortedID)
	{
		const u32 WorkingID = Inverse[SortedID];
		Node& N = Recovered[WorkingID];
		N.Deleted = false;
		if (!LevelGraphFormat::ReadNode(*Native, Saved.version, N.Vertex) || N.Vertex.position().xz() >= MAX_AI_NODES || N.Vertex.position().xz() > Format.MaxXZ)
		{
			Status = "Invalid native export record.";
			return;
		}
		for (u8 D = 0; D < 4; ++D)
		{
			const u32 Link = N.Vertex.link(D);
			if (Link >= Saved.count && Link != Format.InvalidLink)
			{
				Status = "Invalid export link.";
				return;
			}
			N.Vertex.link(D, Link < Saved.count ? Inverse[Link] : Invalid);
		}
		N.Position.set(float(N.Vertex.position().xz() / Row) * Saved.size + Saved.aabb.min.x, float(N.Vertex.position().y()) / 65535.f * Saved.size_y + Saved.aabb.min.y, float(N.Vertex.position().xz() % Row) * Saved.size + Saved.aabb.min.z);
		pvDecompress(N.Normal, N.Vertex.plane());
		if (WorkingID < SourceCount && (fabsf(N.Position.x - Nodes[WorkingID].Position.x) > Header.size * .05f ||
										fabsf(N.Position.z - Nodes[WorkingID].Position.z) > Header.size * .05f))
		{
			Status = "Export IDs address different level cells; recovery refused.";
			return;
		}
	}
	// Restore stable working IDs instead of installing sorted compiled IDs.
	Saved.guid = BaseGUID;
	Saved.count = u32(Recovered.size());
	Header = Saved;
	Nodes.swap(Recovered);
	RevertCells.clear();
	Columns.clear();
	Tiles.clear();
	RemovedCount = 0;
	for (u32 I = 0; I < Nodes.size(); ++I)
	{
		Cache(I);
	}
	ReadReservedBytes();
	Selected = Invalid;
	Pending = true;
	PendingRevert = true;
	Dirty = false;
	HasGround = false;
	LastStroke.set(flt_max, flt_max, flt_max);
	Status = "Saved native grid restored; live rollback queued for the next frame.";
}

void Editor::Draw(const Fvector* Hit, const Fvector* Normal)
{
	if (!Show && !Brush)
	{
		return;
	}
	if (!Shader->inited())
	{
		Shader->create("portal");
	}
	Triangles.clear();
	CFrustum View;
	View.CreateFromMatrix(Device.mFullTransform, FRUSTUM_P_LRTB | FRUSTUM_P_FAR);
	auto Triangle = [&](const Fvector& A, const Fvector& B, const Fvector& C, u32 Color)
	{
		Triangles.push_back({{{A, Color}, {B, Color}, {C, Color}}});
	};
	if (Show)
	{
		const int MinX = iFloor((Device.vCameraPosition.x - DrawDistance) / 16.f), MaxX = iFloor((Device.vCameraPosition.x + DrawDistance) / 16.f);
		const int MinZ = iFloor((Device.vCameraPosition.z - DrawDistance) / 16.f), MaxZ = iFloor((Device.vCameraPosition.z + DrawDistance) / 16.f);
		for (int TX = MinX; TX <= MaxX; ++TX)
		{
			for (int TZ = MinZ; TZ <= MaxZ; ++TZ)
			{
				auto Tile = Tiles.find(Key(TX, TZ));
				if (Tile == Tiles.end())
				{
					continue;
				}
				for (u32 ID : Tile->second)
				{
					const Node& N = Nodes[ID];
					if (N.Deleted)
					{
						continue;
					}
					if (Device.vCameraPosition.distance_to_sqr(N.Position) > DrawDistance * DrawDistance ||
						!View.testSphere_dirty(N.Position, Header.size))
					{
						continue;
					}
					u32 Links = 0;
					for (u8 D = 0; D < 4; ++D)
					{
						Links += N.Vertex.link(D) < Nodes.size();
					}
					u32 Color = ID == Selected ? color_rgba(255, 150, 20, 220) : color_rgba(Links * 50, 170, 255 - Links * 40, 100);
					Triangle(N.Corners[0], N.Corners[1], N.Corners[2], Color);
					Triangle(N.Corners[0], N.Corners[2], N.Corners[3], Color);
				}
			}
		}
	}
	const u32 GridTriangleCount = u32(Triangles.size());
	if (Brush && Hit && Normal)
	{
		Fvector Center = *Hit;
		Center.y += .06f;
		for (int I = 0; I < 64; ++I)
		{
			Fvector A = Center, B = Center;
			A.x += Radius * cosf(I * PI_MUL_2 / 64);
			A.z += Radius * sinf(I * PI_MUL_2 / 64);
			B.x += Radius * cosf((I + 1) * PI_MUL_2 / 64);
			B.z += Radius * sinf((I + 1) * PI_MUL_2 / 64);
			if (fabsf(Normal->y) > .01f)
			{
				A.y -= (Normal->x * (A.x - Center.x) + Normal->z * (A.z - Center.z)) / Normal->y;
				B.y -= (Normal->x * (B.x - Center.x) + Normal->z * (B.z - Center.z)) / Normal->y;
			}
			const bool Erasing = (SDL_GetMouseState(nullptr, nullptr) & SDL_BUTTON_RMASK) != 0;
			Triangle(Center, A, B, Erasing ? color_rgba(255, 45, 35, 85) : color_rgba(40, 255, 100, 85));
			Fvector OuterA = A, OuterB = B;
			const float Scale = (Radius + .035f) / Radius;
			OuterA.sub(Center).mul(Scale).add(Center);
			OuterB.sub(Center).mul(Scale).add(Center);
			const u32 Outline = Erasing ? color_rgba(255, 80, 40, 230) : color_rgba(80, 255, 120, 230);
			Triangle(A, OuterA, OuterB, Outline);
			Triangle(A, OuterB, B, Outline);
		}
	}
	if (Triangles.empty())
	{
		return;
	}
	UIRender->CacheSetXformWorld(Fidentity);
	Device.m_pRender->SetCacheXform(Device.mView, Device.mProject);
	UIRender->SetShader(*Shader);
	const Irect PreviousScissor = UIRender->GetScissor();
	UIRender->SetScissor(nullptr);
	UIRender->SetAlphaRef(0);
	UIRender->CacheSetCullMode(ERHI_CULLMODE::NONE);
	UIRender->zb_enable(true);
	const u32 Batch = UIRender->VBuffMaxSize() / sizeof(Triangles[0]);
	if (Batch)
	{
		auto Submit = [&](u32 Begin, u32 End, bool Depth)
		{
			UIRender->zb_enable(Depth);
			for (u32 Start = Begin; Start < End; Start += Batch)
			{
				u32 Count = std::min(Batch, End - Start);
				auto** Buffer = reinterpret_cast<IUIRender::r_vertL<3>**>(UIRender->StartPrimitive(Count * 3, IUIRender::ptTriList, IUIRender::pttL));
				std::copy_n(Triangles.data() + Start, Count, *Buffer);
				*Buffer += Count;
				UIRender->FlushPrimitive();
			}
		};
		Submit(0, GridTriangleCount, true);
		Submit(GridTriangleCount, u32(Triangles.size()), false);
	}
	UIRender->zb_enable(true);
	Irect RestoreScissor = PreviousScissor;
	UIRender->SetScissor(&RestoreScissor);
	UIRender->CacheSetCullMode(ERHI_CULLMODE::BACK);
}
} // namespace AIGrid

void ShutdownAIGridEditor()
{
	// Release the shader and frame callback while the render device and factory
	// are still alive. The static owner stays empty during xrGame DLL unload.
	AIGrid::EditorInstance().reset();
}

void RenderAIGridEditorWindow()
{
	bool& Open = Engine.External.EditorStates[u8(EditorUI::Tools_AIGridEditor)];
	if (!Open || !g_pGameLevel || !ai().get_level_graph())
	{
		return;
	}
	auto& Editor = AIGrid::GetEditor();
	if (!Editor.Registered)
	{
		Device.seqFrame.Add(&Editor, REG_PRIORITY_HIGH);
		Editor.Registered = true;
	}
	if (Editor.Graph != ai().get_level_graph() || !(Editor.BaseGUID == ai().level_graph().header().guid()))
	{
		Editor.ReadCurrent();
	}
	if (ImGui::Begin("AI Grid Editor", &Open))
	{
		if (ImGui::Checkbox("Enable 3D brush", &Editor.Brush) && Editor.Brush)
		{
			Editor.Single = false;
		}
		ImGui::SliderFloat("Radius (m)", &Editor.Radius, .5f, 500.f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
		bool FiltersChanged = ImGui::Checkbox("Only paint connected to existing grid", &Editor.ConnectedOnly);
		FiltersChanged |= ImGui::Checkbox("Filter surface angle", &Editor.FilterAngle);
		ImGui::BeginDisabled(!Editor.FilterAngle);
		FiltersChanged |= ImGui::SliderFloat("Min angle (degrees)", &Editor.MinAngle, 0.f, 180.f, "%.1f", ImGuiSliderFlags_AlwaysClamp);
		Editor.MaxAngle = std::max(Editor.MinAngle, Editor.MaxAngle);
		FiltersChanged |= ImGui::SliderFloat("Max angle (degrees)", &Editor.MaxAngle, Editor.MinAngle, 180.f, "%.1f", ImGuiSliderFlags_AlwaysClamp);
		ImGui::EndDisabled();
		FiltersChanged |= ImGui::Checkbox("Only captured material", &Editor.FilterMaterial);
		ImGui::SameLine();
		if (ImGui::Button("Capture material"))
		{
			Fvector Surface;
			u16 MaterialIndex = 0;
			if (AIGrid::Ray(Device.vCameraPosition, Device.vCameraDirection, 500.f, Surface, nullptr, &MaterialIndex))
			{
				const SGameMtl* Material = GMLib.GetMaterialByIdx(MaterialIndex);
				if (Material)
				{
					Editor.BrushMaterial = MaterialIndex;
					Editor.MaterialName = Material->m_Name.c_str();
					Editor.HasMaterial = true;
					Editor.Status = "Captured material at the centre of the view.";
					FiltersChanged = true;
				}
			}
			else
			{
				Editor.Status = "No static surface under the centre of the view; captured material unchanged.";
			}
		}
		ImGui::TextWrapped("Material: %s", Editor.HasMaterial ? Editor.MaterialName.c_str() : "not captured");
		ImGui::TextWrapped("Angle from (0, 1, 0): 0 = upward floor, 90 = wall. Disable connectivity protection to paint isolated areas inside buildings.");
		if (FiltersChanged)
		{
			Editor.LastStroke.set(flt_max, flt_max, flt_max);
		}
		if (ImGui::Checkbox("Edit individual node", &Editor.Single) && Editor.Single)
		{
			Editor.Brush = false;
		}
		ImGui::Checkbox("Show AI grid", &Editor.Show);
		ImGui::SliderFloat("Draw distance (m)", &Editor.DrawDistance, 10.f, 150.f);
		ImGui::Text("Nodes: %u | Deleted: %u | Cell: %.2f m%s", u32(Editor.Nodes.size()) - Editor.RemovedCount, Editor.RemovedCount, Editor.Header.size, Editor.Dirty ? " | modified" : "");
		ImGui::Text("Level AI version: %u (preserved when saving)", Editor.Header.version);
		string_path SavePath;
		AIGrid::PatchPath(SavePath);
		ImGui::TextWrapped("Save path: %s", SavePath);
		ImGui::TextWrapped("Brush: hold LMB to paint, RMB to erase. Click a node to select it. Arrows toggle outgoing links in world X/Z.");
		if (Editor.Selected < Editor.Nodes.size())
		{
			const u32 ID = Editor.Selected;
			ImGui::Text("Selected node: %u", ID);
			ImGui::Checkbox("Edit node height Y", &Editor.EditHeight);
			if (Editor.EditHeight)
			{
				float Height = Editor.Nodes[ID].Position.y;
				if (ImGui::DragFloat("Position Y (m)", &Height, .05f, 0.f, 0.f, "%.3f"))
				{
					Editor.SetSelectedHeight(Height);
				}
			}
			ImGui::Checkbox("Edit node tilt X/Z", &Editor.EditTilt);
			if (Editor.EditTilt)
			{
				const Fvector& Normal = Editor.Nodes[ID].Normal;
				float AngleX = rad2deg(atan2f(Normal.z, Normal.y));
				float AngleZ = rad2deg(atan2f(-Normal.x, Normal.y));
				bool Changed = ImGui::DragFloat("Tilt around X (degrees)", &AngleX, .1f, -89.f, 89.f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
				Changed |= ImGui::DragFloat("Tilt around Z (degrees)", &AngleZ, .1f, -89.f, 89.f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
				if (Changed)
				{
					Editor.SetSelectedTilt(AngleX, AngleZ);
				}
				if (ImGui::Button("Reset node tilt"))
				{
					Editor.SetSelectedTilt(0.f, 0.f);
				}
			}
			auto Arrow = [&](u8 D, ImGuiDir Direction)
			{
				ImGui::PushID(int(D));
				bool Linked = Editor.Nodes[ID].Vertex.link(D) < Editor.Nodes.size();
				ImGui::PushStyleColor(ImGuiCol_Button, Linked ? ImVec4(.1f, .6f, .2f, 1.f) : ImVec4(.6f, .15f, .15f, 1.f));
				if (ImGui::ArrowButton("direction", Direction))
				{
					if (Linked)
					{
						Editor.Nodes[ID].Vertex.link(D, AIGrid::Invalid);
						Editor.Dirty = true;
					}
					else
					{
						const Fvector P = Editor.Nodes[ID].Position;
						u32 Other = Editor.Nearest(Editor.X(P) + AIGrid::DX[D], Editor.Z(P) + AIGrid::DZ[D], P.y, 4.f);
						if (Other < Editor.Nodes.size())
						{
							Editor.Nodes[ID].Vertex.link(D, Other);
							Editor.Dirty = true;
						}
						else
						{
							Editor.Status = "No neighbouring node in this direction.";
						}
					}
				}
				ImGui::PopStyleColor();
				ImGui::PopID();
			};
			ImGui::Indent(35.f);
			Arrow(1, ImGuiDir_Up);
			ImGui::SameLine();
			ImGui::TextUnformatted("+Z");
			ImGui::Unindent(35.f);
			Arrow(0, ImGuiDir_Left);
			ImGui::SameLine();
			ImGui::TextUnformatted("+");
			ImGui::SameLine();
			Arrow(2, ImGuiDir_Right);
			ImGui::Indent(35.f);
			Arrow(3, ImGuiDir_Down);
			ImGui::SameLine();
			ImGui::TextUnformatted("-Z");
			ImGui::Unindent(35.f);
			const bool Protected = Editor.ProtectedNodes.contains(ID);
			ImGui::BeginDisabled(Protected);
			if (ImGui::Button("Delete selected node"))
			{
				Editor.DeleteSelectedNode();
			}
			ImGui::EndDisabled();
			if (Protected)
			{
				ImGui::TextUnformatted("Game graph anchor: deletion is protected.");
			}
		}
		if (ImGui::Button("Delete invalid nodes"))
		{
			Editor.DeleteInvalidNodes();
		}
		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("Delete nodes with no outgoing links and components without a node having all four links. Game graph anchors are preserved.");
		}
		const bool HasReference = Editor.Selected < Editor.Nodes.size() && !Editor.Nodes[Editor.Selected].Deleted;
		ImGui::BeginDisabled(!HasReference);
		if (ImGui::Button("Delete nodes without path to selected"))
		{
			Editor.DeleteNodesWithoutPathToSelected();
		}
		ImGui::EndDisabled();
		ImGui::TextWrapped("Select a node on the main outdoor grid using Edit individual node. Cleanup keeps nodes that can reach it; game graph anchors are protected.");
		if (ImGui::Button("Apply realtime"))
		{
			Editor.Pending = true;
		}
		ImGui::SameLine();
		if (ImGui::Button("Revert to saved"))
		{
			Editor.RevertToSaved();
		}
		ImGui::SameLine();
		if (ImGui::Button("Overwrite save"))
		{
			Editor.Save();
		}
		ImGui::TextWrapped("%s", Editor.Status.c_str());
	}
	ImGui::End();
	if (!Open)
	{
		return;
	}
	Fvector Hit, Normal;
	bool HasHit = AIGrid::Ray(Device.vCameraPosition, AIGrid::MouseDirection(), 500.f, Hit, &Normal);
	bool CanEdit = CImGuiManager::Instance().IsCapturingInputs() && !ImGui::IsWindowHovered(ImGuiHoveredFlags_AnyWindow) && !ImGui::IsAnyItemActive();
	if (HasHit && CanEdit)
	{
		const bool Erasing = ImGui::IsMouseDown(ImGuiMouseButton_Right);
		if (Editor.LastErasing != Erasing)
		{
			Editor.LastStroke.set(flt_max, flt_max, flt_max);
			Editor.LastErasing = Erasing;
		}
		if (Editor.Single && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
		{
			Editor.Selected = Editor.Nearest(Editor.X(Hit), Editor.Z(Hit), Hit.y, Editor.Header.size);
		}
		if (Editor.Brush && (ImGui::IsMouseDown(ImGuiMouseButton_Left) || ImGui::IsMouseDown(ImGuiMouseButton_Right)) && Hit.distance_to_sqr(Editor.LastStroke) > _sqr(Editor.Header.size * .2f))
		{
			const float Distance = Hit.distance_to(Editor.LastStroke);
			const int Steps = Editor.LastStroke.x != flt_max && Distance < 50.f ? std::min(16, std::max(1, iCeil(Distance / std::max(Editor.Header.size, Editor.Radius * .75f)))) : 1;
			for (int I = 1; I <= Steps; ++I)
			{
				Fvector Point = Hit;
				if (Steps > 1)
				{
					Point.lerp(Editor.LastStroke, Hit, float(I) / Steps);
				}
				if (ImGui::IsMouseDown(ImGuiMouseButton_Right))
				{
					Editor.Erase(Point);
				}
				else
				{
					Editor.Paint(Point);
				}
			}
			Editor.LastStroke = Hit;
		}
	}
	if (!CanEdit || (!ImGui::IsMouseDown(ImGuiMouseButton_Left) && !ImGui::IsMouseDown(ImGuiMouseButton_Right)))
	{
		Editor.LastStroke.set(flt_max, flt_max, flt_max);
	}
}

void RenderAIGridEditorWorld()
{
	if (!Engine.External.EditorStates[u8(EditorUI::Tools_AIGridEditor)] || !g_pGameLevel ||
		!ai().get_level_graph() || !CImGuiManager::Instance().IsGUIRendering())
	{
		return;
	}
	auto& Editor = AIGrid::GetEditor();
	if (Editor.Graph != ai().get_level_graph() || !(Editor.BaseGUID == ai().level_graph().header().guid()))
	{
		return;
	}
	Fvector Hit, Normal;
	const Fvector Direction = AIGrid::MouseDirection();
	if (AIGrid::Ray(Device.vCameraPosition, Direction, 500.f, Hit, &Normal))
	{
		Editor.LastGround = Hit;
		Editor.HasGround = true;
	}
	else
	{
		// Keep a visible preview when the cursor misses terrain. Painting still
		// requires a real surface hit in the input handler.
		const float Height = Editor.HasGround ? Editor.LastGround.y : Device.vCameraPosition.y - 2.f;
		const float Distance = fabsf(Direction.y) > EPS_L ? (Height - Device.vCameraPosition.y) / Direction.y : 20.f;
		Hit.mad(Device.vCameraPosition, Direction, clampr(Distance, 3.f, 100.f));
		Normal.set(0.f, 1.f, 0.f);
	}
	Editor.Draw(&Hit, &Normal);
}

bool HasAIGridEditorPatch()
{
	string_path Path;
	AIGrid::PatchPath(Path);
	return FS.exist(Path) != nullptr;
}

void LoadAIGridEditorPatch()
{
	string_path Path;
	AIGrid::PatchPath(Path);
	IReader* Reader = FS.r_open(Path);
	if (!Reader)
	{
		return;
	}
	const u32 Prefix = sizeof(u32) + 2 * sizeof(xrGUID) + sizeof(hdrNODES);
	const u32 FileMagic = Reader->length() >= Prefix ? Reader->r_u32() : 0;
	const bool HasDeletionMask = FileMagic == AIGrid::Magic;
	bool Valid = HasDeletionMask || FileMagic == AIGrid::LegacyMagic;
	xrGUID LevelGUID{}, GameGUID{};
	hdrNODES Header{};
	if (Valid)
	{
		Reader->r(&LevelGUID, sizeof(LevelGUID));
		Reader->r(&GameGUID, sizeof(GameGUID));
		Reader->r(&Header, sizeof(Header));
		Valid = LevelGUID == ai().level_graph().header().guid() && GameGUID == ai().cross_table().header().game_guid() &&
				Header.guid == LevelGUID && Header.count >= ai().level_graph().header().vertex_count() && Header.count < AIGrid::Invalid &&
				Header.version == ai().level_graph().header().version() &&
				Header.size == ai().level_graph().header().cell_size() && std::isfinite(Header.size_y) && Header.size_y > 0 &&
				u64(Reader->length()) == Prefix + u64(Header.count) * (sizeof(ILevelGraph::CVertex) + sizeof(AIGrid::Cell) + (HasDeletionMask ? 1 : 0));
	}
	xr_vector<ILevelGraph::CVertex> Nodes;
	xr_vector<AIGrid::Cell> Cells;
	xr_vector<u8> Deleted;
	LevelGraphFormat::Limits NativeFormat{};
	if (Valid)
	{
		Valid = LevelGraphFormat::GetLimits(Header.version, NativeFormat) && Header.count < NativeFormat.InvalidLink;
	}
	if (Valid)
	{
		for (u32 D = 0; D < 3; ++D)
		{
			Valid &= std::isfinite(Header.aabb.min[D]) && std::isfinite(Header.aabb.max[D]) && Header.aabb.min[D] <= Header.aabb.max[D];
		}
	}
	if (Valid)
	{
		const double RowValue = (double(Header.aabb.max.z) - Header.aabb.min.z) / Header.size + EPS_L + 1.5;
		const double ColumnValue = (double(Header.aabb.max.x) - Header.aabb.min.x) / Header.size + EPS_L + 1.5;
		Valid = RowValue >= 1 && ColumnValue >= 1 && RowValue < MAX_AI_NODES && ColumnValue < MAX_AI_NODES;
	}
	if (Valid)
	{
		Nodes.resize(Header.count);
		Cells.resize(Header.count);
		Reader->r(Nodes.data(), Nodes.size() * sizeof(Nodes[0]));
		Reader->r(Cells.data(), Cells.size() * sizeof(Cells[0]));
		Deleted.assign(Header.count, 0);
		if (HasDeletionMask)
		{
			Reader->r(Deleted.data(), Deleted.size());
		}
		for (u32 I = 0; I < Header.count && Valid; ++I)
		{
			Valid = Deleted[I] <= 1 && Cells[I].tGraphIndex < ai().cross_table().header().game_vertex_count() &&
					std::isfinite(Cells[I].fDistance) && Cells[I].fDistance >= 0 && Nodes[I].position().xz() < MAX_AI_NODES &&
					Nodes[I].position().xz() <= NativeFormat.MaxXZ;
			for (u8 D = 0; D < 4; ++D)
			{
				u32 Link = Nodes[I].link(D);
				Valid &= Link < Header.count || Link >= AIGrid::Invalid;
				if (Link < Header.count)
				{
					Valid &= !Deleted[I] && !Deleted[Link];
				}
			}
		}
		Valid &= std::find(Deleted.begin(), Deleted.end(), u8(0)) != Deleted.end();
		const auto& Game = ai().game_graph();
		const auto LevelID = Game.vertex(Game.current_level_vertex())->level_id();
		for (u32 I = 0; I < Game.header().vertex_count() && Valid; ++I)
		{
			const auto* V = Game.vertex(I);
			if (V->level_id() == LevelID)
			{
				Valid = V->level_vertex_id() < Deleted.size() && !Deleted[V->level_vertex_id()];
			}
		}
		// Existing IDs must still address the same X/Z cells, including when
		// painting extends the bounds and changes the packed row length.
		const auto& Base = ai().level_graph();
		const u32 Row = iFloor((Header.aabb.max.z - Header.aabb.min.z) / Header.size + EPS_L + 1.5f);
		const float ShiftX = (Base.header().box().min.x - Header.aabb.min.x) / Header.size;
		const float ShiftZ = (Base.header().box().min.z - Header.aabb.min.z) / Header.size;
		const int OffsetX = iFloor(ShiftX + .5f), OffsetZ = iFloor(ShiftZ + .5f);
		Valid &= fabsf(ShiftX - OffsetX) < .01f && fabsf(ShiftZ - OffsetZ) < .01f;
		for (u32 I = 0; I < Base.header().vertex_count() && Valid; ++I)
		{
			int X, Z;
			Base.unpack_xz(Base.vertex(I), X, Z);
			Valid = s64(Nodes[I].position().xz() / Row) == s64(X) + OffsetX &&
					s64(Nodes[I].position().xz() % Row) == s64(Z) + OffsetZ;
		}
	}
	FS.r_close(Reader);
	if (!Valid)
	{
		Msg("! AI grid editor: ignored invalid or mismatched patch %s", Path);
		return;
	}
	auto* Graph = dynamic_cast<CLevelGraph*>(&ai().level_graph());
	if (Graph)
	{
		Graph->ApplyEditorNodes(Header, Nodes, Deleted);
		const_cast<IGameLevelCrossTable&>(ai().cross_table()).ApplyEditorCells(Cells);
		Msg("* AI grid editor: restored %u nodes from %s", Header.count, Path);
	}
}
