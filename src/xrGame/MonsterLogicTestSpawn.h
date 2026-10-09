#pragma once

#include "../xrEngine/AI/game_level_cross_table.h"

class CCC_GSpawnMonsters : public IConsole_Command
{
	struct SGroup
	{
		const char* Section;
		u32 Count;
		float Side;
		float Forward;
	};
	struct SSpawn
	{
		const char* Section;
		Fvector Position;
		u32 Vertex;
	};
public:
	CCC_GSpawnMonsters(const char* Name) : IConsole_Command(Name)
	{
		bEmptyArgsHandled = true;
	}
	void Execute(const char* Args) override
	{
		if (!g_pGameLevel || !IsGameTypeSingle() || !ai().get_alife() || !ai().get_cross_table() ||
			!Level().Server || !Level().Server->game)
		{
			Msg("! g_spawn_monsters requires a loaded single-player level with ALife");
			return;
		}
		CActor* Actor = Level().CurrentEntity() ? Level().CurrentEntity()->cast_actor() : nullptr;
		if (!Actor || !Actor->g_Alive())
		{
			Msg("! g_spawn_monsters requires a living actor");
			return;
		}
		xr_string Profile = Args ? Args : "";
		_Trim(Profile);
		if (Profile.empty())
		{
			Profile = "50";
		}
		xr_vector<SGroup> Groups;
		u32 Scale = 1;
		if (Profile == "50" || Profile == "100" || Profile == "200")
		{
			Scale = Profile == "200" ? 4 : Profile == "100" ? 2 : 1;
			Groups = {{"dog_strong", 20, -16.f, 26.f}, {"burer_normal", 3, 0.f, 30.f},
				{"controller_tubeman", 2, 8.f, 30.f}, {"boar_normal", 10, -14.f, 48.f},
				{"bayun_normal", 10, 14.f, 48.f}, {"biryuk_normal", 5, 18.f, 26.f}};
		}
		else if (Profile == "radio")
		{
			Groups = {{"dog_strong", 6, -8.f, 25.f}, {"wolf_grey_normal", 4, 8.f, 30.f},
				{"boar_normal", 4, 0.f, 44.f}};
		}
		else if (Profile == "psy")
		{
			Groups = {{"burer_normal", 2, -6.f, 28.f}, {"psy_dog_normal", 2, 6.f, 28.f},
				{"zhaba_normal", 3, -20.f, 32.f}, {"geruda_normal", 3, -10.f, 40.f},
				{"tarakan_normal", 3, 10.f, 40.f}, {"rotan_normal", 3, 20.f, 32.f},
				{"bayun_normal", 4, 0.f, 54.f}};
		}
		else if (Profile == "targets")
		{
			Groups = {{"dog_strong", 12, 0.f, 26.f}, {"bayun_normal", 3, -12.f, 46.f},
				{"boar_normal", 3, 12.f, 46.f}};
		}
		else if (Profile == "hit")
		{
			Groups = {{"dog_normal", 1, -12.f, 35.f}, {"boar_normal", 1, 0.f, 55.f},
				{"biryuk_normal", 1, 12.f, 75.f}};
		}
		else if (Profile == "peaceful")
		{
			Groups = {{"m_scavenger_crow", 20, 0.f, 30.f}, {"dog_strong", 4, -14.f, 40.f},
				{"boar_normal", 2, 14.f, 40.f}};
		}
		else
		{
			InvalidSyntax();
			return;
		}
		u32 Requested = 0;
		for (const SGroup& Group : Groups)
		{
			if (!pSettings->section_exist(Group.Section) || !pSettings->line_exist(Group.Section, "class") ||
				!pSettings->line_exist(Group.Section, "MaxHealthValue") || !pSettings->line_exist(Group.Section, "visual") ||
				!isValidSection(Group.Section))
			{
				Msg("! Test aborted before spawning: missing/invalid monster section [%s]", Group.Section);
				return;
			}
			Requested += Group.Count * Scale;
		}
		Fvector Forward = Device.vCameraDirection;
		Forward.y = 0.f;
		if (!_valid(Forward) || Forward.square_magnitude() < EPS_L)
		{
			Msg("! Look horizontally toward an open area before running g_spawn_monsters");
			return;
		}
		Forward.normalize();
		Fvector Right;
		Right.set(Forward.z, 0.f, -Forward.x);
		const Fvector Origin = Actor->Position();
		ILevelGraph& Graph = ai().level_graph();
		xr_vector<Fvector> SearchOffsets;
		SearchOffsets.reserve(625);
		for (int X = -12; X <= 12; ++X)
		{
			for (int Z = -12; Z <= 12; ++Z)
			{
				if (X * X + Z * Z <= 144)
				{
					Fvector Offset;
					Offset.set(float(X) * 2.f, 0.f, float(Z) * 2.f);
					SearchOffsets.push_back(Offset);
				}
			}
		}
		std::sort(SearchOffsets.begin(), SearchOffsets.end(), [](const Fvector& A, const Fvector& B)
		{
			return A.square_magnitude() < B.square_magnitude();
		});
		u32 Shifted = 0;
		float MaxShift = 0.f;
		xr_vector<SSpawn> Pending;
		Pending.reserve(Requested);
		for (const SGroup& Group : Groups)
		{
			const u32 Count = Group.Count * Scale;
			const u32 Columns = std::min(Count, 10u);
			for (u32 Index = 0; Index < Count; ++Index)
			{
				Fvector Desired = Origin;
				Desired.mad(Right, Group.Side + (float(Index % Columns) - float(Columns - 1) * .5f) * 3.f);
				Desired.mad(Forward, Group.Forward + float(Index / Columns) * 3.f);
				bool Found = false;
				u32 ValidNodes = 0;
				for (const Fvector& Offset : SearchOffsets)
				{
					Fvector Probe;
					Probe.add(Desired, Offset);
					if (!Graph.valid_vertex_position(Probe))
					{
						continue;
					}
					const u32 Vertex = Graph.vertex_id(Probe);
					if (!Graph.valid_vertex_id(Vertex))
					{
						continue;
					}
					++ValidNodes;
					Fvector Position = Graph.vertex_position(Vertex);
					Position.y = Graph.vertex_plane_y(Vertex, Position.x, Position.z);
					if (!_valid(Position))
					{
						continue;
					}
					if (Position.distance_to_sqr(Origin) < 64.f || std::any_of(Pending.begin(), Pending.end(), [&](const SSpawn& Existing)
					{
						return Position.distance_to_sqr(Existing.Position) < 4.f;
					}))
					{
						continue;
					}
					const float DeltaX = Position.x - Desired.x, DeltaZ = Position.z - Desired.z;
					const float Shift = _sqrt(DeltaX * DeltaX + DeltaZ * DeltaZ);
					if (Shift > 2.f)
					{
						++Shifted;
						MaxShift = std::max(MaxShift, Shift);
					}
					Position.y += .2f;
					Pending.push_back({Group.Section, Position, Vertex});
					Found = true;
					break;
				}
				if (!Found)
				{
					Msg("! Test aborted before spawning: [%s] %u has no free AI node within 24m (nodes %u)",
						Group.Section, Index + 1, ValidNodes);
					Msg("! Move onto walkable ground and face another area; no monsters were spawned");
					return;
				}
			}
		}
		u32 Spawned = 0;
		for (const SSpawn& Entry : Pending)
		{
			const auto GameVertex = ai().cross_table().vertex(Entry.Vertex).game_vertex_id();
			auto* Object = Level().Server->game->alife().spawn_item(Entry.Section, Entry.Position, Entry.Vertex, GameVertex, ALife::INVALID_OBJECT_ID);
			if (!Object || !Object->cast_alife_object())
			{
				Msg("! Spawn failed at [%s]; spawned %u/%u", Entry.Section, Spawned, Requested);
				return;
			}
			Object->cast_alife_object()->use_ai_locations(true);
			++Spawned;
		}
		Console->Execute("monster_logic_stats on");
		Console->Execute("monster_logic_draw on");
		Msg("* g_spawn_monsters %s: spawned %u monsters; existing monsters were not removed", Profile.c_str(), Spawned);
		if (Shifted)
		{
			Msg("* Adapted to AI grid: %u spawn positions shifted, maximum %.1fm", Shifted, MaxShift);
		}
		for (const SGroup& Group : Groups)
		{
			Msg("* [%s] x%u", Group.Section, Group.Count * Scale);
		}
		Msg("* Wait 15-20 seconds, then capture MAIN and DETAILS. Compare monster_logic on/off in the same scene.");
	}
	void Info(TInfo& I) override { xr_strcpy(I, "[50(default)/100/200/radio/psy/targets/hit/peaceful]"); }
	void Save(IWriter*) override {}
	void fill_tips(vecTips& Tips, u32) override
	{
		for (const char* Profile : {"50", "100", "200", "radio", "psy", "targets", "hit", "peaceful"})
		{
			Tips.push_back(Profile);
		}
	}
};
