#include "StdAfx.h"
#include "GamedataReloader.h"

#include "../xrCore/xrAddons.h"
#include "../xrCore/Save/SaveObject.h"
#include "../xrEngine/device.h"
#include "../xrEngine/x_ray.h"
#include "../xrEngine/IGame_Persistent.h"
#include "../xrEngine/AI/alife_space.h"

#include <filesystem>

CGamedataReloader& CGamedataReloader::Instance()
{
	static CGamedataReloader Reloader;
	return Reloader;
}

bool CGamedataReloader::ResolvePath(const MountEntry& Entry, string_path& FullPath)
{
	if (!FS.path_exist(Entry.Alias.c_str()))
	{
		Msg("! Gamedata mount: unknown path alias '%s'", Entry.Alias.c_str());
		return false;
	}

	FS.update_path(FullPath, Entry.Alias.c_str(), Entry.Path.c_str());

	std::error_code Error;
	if (!std::filesystem::exists(FullPath, Error))
	{
		Msg("! Gamedata mount: '%s' not found", FullPath);
		return false;
	}

	return true;
}

bool CGamedataReloader::Mount(const char* PathAlias, const char* Path)
{
	if (PathAlias == nullptr || Path == nullptr)
	{
		return false;
	}

	MountEntry Entry = {PathAlias, Path};
	string_path FullPath;
	if (!ResolvePath(Entry, FullPath))
	{
		return false;
	}

	if (std::find(Desired.begin(), Desired.end(), Entry) == Desired.end())
	{
		Desired.push_back(Entry);
		Schedule();
	}

	return true;
}

bool CGamedataReloader::Unmount(const char* PathAlias, const char* Path)
{
	if (PathAlias == nullptr || Path == nullptr)
	{
		return false;
	}

	auto It = std::find(Desired.begin(), Desired.end(), MountEntry {PathAlias, Path});
	if (It == Desired.end())
	{
		return false;
	}

	Desired.erase(It);
	Schedule();
	return true;
}

void CGamedataReloader::Reload(bool Textures, bool Sounds)
{
	PendingTextures |= Textures;
	PendingSounds |= Sounds;
	Schedule();
}

void CGamedataReloader::Reset()
{
	OnDesiredLoaded({});
}

void CGamedataReloader::OnDesiredLoaded(xr_vector<MountEntry>&& Loaded)
{
	Desired = std::move(Loaded);
	if (Desired != Applied)
	{
		// Textures and sounds of the current gamedata may be loaded already
		Reload(true, true);
	}
}

void CGamedataReloader::Save(IWriter& Stream)
{
	Stream.open_chunk(GAMEDATA_MOUNTS_CHUNK_DATA);
	Stream.w_u32((u32)Desired.size());
	for (const MountEntry& Entry : Desired)
	{
		Stream.w_stringZ(Entry.Alias);
		Stream.w_stringZ(Entry.Path);
	}
	Stream.close_chunk();
}

void CGamedataReloader::Load(IReader& Stream)
{
	xr_vector<MountEntry> Loaded;
	if (IReader* Chunk = Stream.open_chunk(GAMEDATA_MOUNTS_CHUNK_DATA))
	{
		const u32 Count = Chunk->r_u32();
		Loaded.resize(Count);
		for (MountEntry& Entry : Loaded)
		{
			Chunk->r_stringZ(Entry.Alias);
			Chunk->r_stringZ(Entry.Path);
		}
		Chunk->close();
	}

	OnDesiredLoaded(std::move(Loaded));
}

void CGamedataReloader::Serialize(ISaveObject& Object)
{
	xr_vector<shared_str> Aliases;
	xr_vector<shared_str> Paths;
	if (Object.IsSave())
	{
		for (const MountEntry& Entry : Desired)
		{
			Aliases.push_back(Entry.Alias);
			Paths.push_back(Entry.Path);
		}
	}

	BEGIN_CHUNK(Object, "GamedataMounts")
	{
		Object << Aliases << Paths;
	}

	if (!Object.IsSave())
	{
		xr_vector<MountEntry> Loaded;
		for (size_t Idx = 0; Idx < Aliases.size() && Idx < Paths.size(); Idx++)
		{
			Loaded.push_back({Aliases[Idx], Paths[Idx]});
		}

		OnDesiredLoaded(std::move(Loaded));
	}
}

void CGamedataReloader::ApplyMounts()
{
	if (GAddonsManager == nullptr)
	{
		return;
	}

	size_t Common = 0;
	while (Common < Applied.size() && Common < Desired.size() && Applied[Common] == Desired[Common])
	{
		Common++;
	}

	// Runtime mounts are a stack: drop everything above the common part, then mount the rest
	while (Applied.size() > Common)
	{
		GAddonsManager->UnmountRuntime();
		Applied.pop_back();
	}

	for (auto It = Desired.begin() + Common; It != Desired.end();)
	{
		string_path FullPath;
		if (ResolvePath(*It, FullPath) && GAddonsManager->MountRuntime(FullPath))
		{
			Applied.push_back(*It);
			++It;
		}
		else
		{
			It = Desired.erase(It);
		}
	}
}

void CGamedataReloader::Schedule()
{
	if (IsScheduled)
	{
		return;
	}

	// Scripts run on the game thread, which is done before the next on_idle reads the list
	IsScheduled = true;
	Stage = EStage::Begin;
	g_loading_events.push_back(LOADING_EVENT(this, &CGamedataReloader::OnLoadingEvent));
}

// One stage per call: on_idle draws the load screen between the calls
bool CGamedataReloader::OnLoadingEvent()
{
	PROF_EVENT("CGamedataReloader::OnLoadingEvent");

	switch (Stage)
	{
	case EStage::Begin:
	{
		StartTime = Device.TimerAsync();
		StageCount = 1 + (PendingSounds ? 1 : 0) + (PendingTextures ? 1 : 0);
		pApp->LoadBegin();
		g_pGamePersistent->SetLoadStageTitle("st_loading_gamedata");
		pApp->LoadStage(StageCount);
		Stage = EStage::Mount;
		return false;
	}
	case EStage::Mount:
	{
		// Scripts may still edit the list until here
		IsScheduled = false;
		ApplyMounts();
		Stage = EStage::Sounds;
		return false;
	}
	case EStage::Sounds:
	{
		if (PendingSounds)
		{
			g_pGamePersistent->SetLoadStageTitle("st_loading_sounds");
			pApp->LoadStage(StageCount);
			Sound->refresh_env_library();
			Sound->refresh_sources();
			PendingSounds = false;
		}
		Stage = EStage::Textures;
		return false;
	}
	case EStage::Textures:
	{
		if (PendingTextures)
		{
			g_pGamePersistent->SetLoadStageTitle("st_loading_textures");
			pApp->LoadStage(StageCount);
			// vid_restart does not re-read textures on R4 unless deferred loading is on
			Device.m_pRender->ReloadTextures();
			PendingTextures = false;
		}
		Stage = EStage::End;
		return false;
	}
	case EStage::End:
	default:
	{
		g_pGamePersistent->SetLoadStageTitle(nullptr);
		pApp->LoadEnd();
		Stage = EStage::Begin;
		Msg("* Gamedata reload done [%u ms]", Device.TimerAsync() - StartTime);
		return true;
	}
	}
}

XREFLECT(LuaExport, Name = "game.mount_gamedata")
bool MountGamedataScript(const char* PathAlias, const char* Path)
{
	return CGamedataReloader::Instance().Mount(PathAlias, Path);
}

XREFLECT(LuaExport, Name = "game.unmount_gamedata")
bool UnmountGamedataScript(const char* PathAlias, const char* Path)
{
	return CGamedataReloader::Instance().Unmount(PathAlias, Path);
}

XREFLECT(LuaExport, Name = "game.reload_gamedata")
void ReloadGamedataScript(bool Textures, bool Sounds)
{
	CGamedataReloader::Instance().Reload(Textures, Sounds);
}

#include "GamedataReloader.reflect.inl"
