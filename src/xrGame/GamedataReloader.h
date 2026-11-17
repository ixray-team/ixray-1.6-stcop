#pragma once

class ISaveObject;

// Swaps gamedata while the game runs (issue #406).
// Scripts edit the list of runtime mounts and request reloads. The work runs on the primary thread
// through g_loading_events behind the load screen. The list is stored in saves and restored on load.
class CGamedataReloader
{
public:
	static CGamedataReloader& Instance();

	// Adds a directory or a .db archive (PathAlias + Path) on top of $game_data$.
	// Returns false when the alias or the target does not exist.
	bool Mount(const char* PathAlias, const char* Path);
	// Removes a mount added by Mount. Mounts added after it are mounted again.
	bool Unmount(const char* PathAlias, const char* Path);

	// Queues a reload of textures (full device reset) and/or sounds.
	void Reload(bool Textures, bool Sounds);

	// New game: back to the base gamedata
	void Reset();

	void Save(IWriter& Stream);
	void Load(IReader& Stream);
	void Serialize(ISaveObject& Object);

private:
	struct MountEntry
	{
		shared_str Alias;
		shared_str Path;

		bool operator==(const MountEntry& Other) const
		{
			return Alias == Other.Alias && Path == Other.Path;
		}
	};

	enum class EStage
	{
		Begin,
		Mount,
		Sounds,
		Textures,
		End
	};

	CGamedataReloader() = default;

	static bool ResolvePath(const MountEntry& Entry, string_path& FullPath);
	void OnDesiredLoaded(xr_vector<MountEntry>&& Loaded);
	void ApplyMounts();
	void Schedule();
	bool OnLoadingEvent();

	xr_vector<MountEntry> Desired;
	xr_vector<MountEntry> Applied;

	bool PendingTextures = false;
	bool PendingSounds = false;
	bool IsScheduled = false;
	EStage Stage = EStage::Begin;
	int StageCount = 0;
	u32 StartTime = 0;
};
