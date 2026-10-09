#pragma once

// Waveform previews for the Content Browser.
// Peaks are baked from gamedata .ogg files in background tasks and cached as .sch files
// in rawdata under the same relative path (gamedata\sounds\a\b.ogg -> rawdata\sounds\a\b.sch).
class CSoundPreviewCache
{
public:
	static constexpr u32 PeakCount = 256;
	static constexpr u32 MaxLanes = 2;

	struct PeakRange
	{
		float Min = 0.0f;
		float Max = 0.0f;
	};

	struct Preview
	{
		float Duration = 0.0f;
		u32 SampleRate = 0;
		u16 Channels = 0;
		u16 Lanes = 0;

		// [Lane * PeakCount + Peak]
		xr_vector<PeakRange> Peaks;
	};

public:
	CSoundPreviewCache() = default;
	~CSoundPreviewCache();

	CSoundPreviewCache(const CSoundPreviewCache&) = delete;
	CSoundPreviewCache& operator=(const CSoundPreviewCache&) = delete;

	void Initialize();
	void Clear();

	// Returns nullptr while the preview is baking or when the file can't be decoded
	const Preview* Get(const xr_path& SoundFile);

	// Drops the preview if the source file was changed on disk
	void Refresh(const xr_path& SoundFile);

	xr_path GetCachePath(const xr_path& SoundFile) const;
	bool GetGameSoundName(const xr_path& SoundFile, xr_string& OutName) const;

	static bool IsPreviewable(const xr_path& File);

private:
	enum class EState : u8
	{
		Pending,
		Ready,
		Failed
	};

	struct Entry
	{
		std::atomic<EState> State = EState::Pending;
		u64 SourceSize = 0;
		s64 SourceTime = 0;
		Preview Data;
	};

	static bool GetSourceStamp(const xr_path& SoundFile, u64& OutSize, s64& OutTime);
	static bool LoadCache(const xr_path& CacheFile, u64 SourceSize, s64 SourceTime, Preview& Out);
	static void SaveCache(const xr_path& CacheFile, u64 SourceSize, s64 SourceTime, const Preview& Data);
	static bool Bake(const xr_path& SoundFile, Preview& Out);

	static xr_string NormalizePath(const xr_path& Path);

private:
	xrCriticalSection Lock;
	xr_hash_map<xr_string, xr_unique_ptr<Entry>> Entries;
	xr_task_group Tasks;

	xr_string GameDataRoot;
	xr_string RawDataRoot;
	xr_string GameSoundsRoot;
};
