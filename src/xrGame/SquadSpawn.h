#pragma once

// Squad spawn helpers for g_spawn_squad and the spawn manager.
// Each platform keeps squads in its own world model:
// - Call of Pripyat: squad sections live in system.ltx and spawn through xr_effects.create_squad.
// - Clear Sky: squad sections live in misc\squad_descr.ltx and belong to sim_board factions and smarts.
// - Shadow of Chernobyl: there are no squads. A squad is a group of simulation NPCs or monsters
//   spawned near the camera target. Smart terrains (gulags) pick them up by their own rules.
namespace SquadSpawn
{
	// Ini file with squad sections, or nullptr if the platform has no squad descriptions.
	CInifile* GetDescriptions();

	void CollectSections(xr_vector<CInifile::Sect*>& Sections);
	bool IsSquadSection(const char* Section);

	// Count is the number of squads. In Shadow of Chernobyl it is the number of members (0 means the default size).
	void Spawn(const char* Section, int Count);

	constexpr int SocDefaultGroupSize = 4;
}
