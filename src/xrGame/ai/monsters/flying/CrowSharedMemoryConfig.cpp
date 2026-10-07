#include "StdAfx.h"
#include "CrowSharedMemory.h"

bool CCrowSharedMemory::ReloadConfig()
{
	string_path Path;
	FS.update_path(Path, "$game_config$", "creatures\\crow_shared_memory.ltx");
	if (!FS.exist(Path))
	{
		Msg("! Crow shared memory: configuration file not found: %s", Path);
		return false;
	}
	CInifile Ini(Path, TRUE, TRUE, FALSE);
	if (!Ini.section_exist("crow_shared_memory"))
	{
		Msg("! Crow shared memory: missing [crow_shared_memory] section");
		return false;
	}
	SConfig Next;
	auto Read = [&](const char* Key, u32 Default, u32 Min, u32 Max)
	{
		u32 Value = READ_IF_EXISTS(&Ini, r_u32, "crow_shared_memory", Key, Default);
		clamp(Value, Min, Max);
		return Value;
	};
	Next.MaxCachedCorpses = Read("corpse_limit", Next.MaxCachedCorpses, 0u, 65535u);
	Next.MaxCachedObservations = Read("observation_limit", Next.MaxCachedObservations, 0u, 65535u);
	Next.PerchCount = Read("perches_per_corpse", Next.PerchCount, 0u, 128u);
	Next.CorpseRefreshMs = Read("corpse_refresh_ms", Next.CorpseRefreshMs, 0u, 3600000u);
	Next.ObservationRefreshMs = Read("observation_refresh_ms", Next.ObservationRefreshMs, 16u, 60000u);
	Next.PerchAttempts = Read("perch_search_attempts", Next.PerchAttempts, 1u, 1024u);
	Next.PerchRetryMs = Read("perch_retry_ms", Next.PerchRetryMs, 250u, 60000u);
	Next.CorpseIdleMs = Read("corpse_idle_ms", Next.CorpseIdleMs, 250u, 3600000u);
	Next.ObservationIdleMs = Read("observation_idle_ms", Next.ObservationIdleMs, 250u, 3600000u);
	Next.MaintenanceMs = Read("maintenance_ms", Next.MaintenanceMs, 16u, 10000u);
	Next.PerchProbesPerFrame = Read("perch_probes_per_frame", Next.PerchProbesPerFrame, 1u, 64u);
	Next.ObservationQueriesPerFrame = Read("observation_queries_per_frame", Next.ObservationQueriesPerFrame, 1u, 64u);
	Next.CellSize = READ_IF_EXISTS(&Ini, r_float, "crow_shared_memory", "observation_cell_size", Next.CellSize);
	Next.CorpseMoveDistance = READ_IF_EXISTS(&Ini, r_float, "crow_shared_memory", "corpse_move_distance", Next.CorpseMoveDistance);
	if (!_valid(Next.CellSize) || !_valid(Next.CorpseMoveDistance))
	{
		Msg("! Crow shared memory: non-finite configuration value");
		return false;
	}
	clamp(Next.CellSize, 2.f, 128.f);
	clamp(Next.CorpseMoveDistance, .005f, 1.f);
	Config = Next;
	ConfigLoaded = true;
	// Discard derived data immediately: old cell keys and candidate counts cannot be reused.
	xr_map<u32, SCorpse>().swap(Corpses);
	xr_map<SCellKey, SObservation>().swap(Observations);
	ProbeFrame = ObservationFrame = u32(-1);
	MaintenanceTime = Device.dwTimeGlobal;
	Msg("* Crow shared memory reloaded: %u corpses, %u cells, %u perches/corpse", Config.MaxCachedCorpses, Config.MaxCachedObservations, Config.PerchCount);
	return true;
}
