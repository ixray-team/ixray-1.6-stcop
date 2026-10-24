#pragma once

// Particle previews for the Content Browser.
// The effect is created on the primary thread, simulated to the middle of its lifetime in a
// background task and rendered to a small target on the primary thread a few per frame.
// Baked pixels are cached in $app_data_root$\particle_previews, keyed by a CRC of the definition.
class CParticlePreviewCache
{
public:
	static constexpr u32 PreviewSize = 128;

public:
	CParticlePreviewCache() = default;
	~CParticlePreviewCache();

	CParticlePreviewCache(const CParticlePreviewCache&) = delete;
	CParticlePreviewCache& operator=(const CParticlePreviewCache&) = delete;

	// Returns an empty texture while the preview is baking or when it can't be baked.
	// Def is a PS::CPGDef for groups, otherwise a PS::CPEDef
	ref_texture Get(const xr_string& Name, void* Def, bool IsGroup);

	// Advances the bake queue: creates effects and renders simulated ones. Primary thread only
	void Update();

	void Clear();

private:
	enum class EState : u8
	{
		Loading,
		Loaded,
		Missed,
		Queued,
		Simulating,
		Simulated,
		Ready,
		Failed
	};

	struct Entry
	{
		std::atomic<EState> State = EState::Loading;
		void* Def = nullptr;
		bool IsGroup = false;
		u32 Hash = 0;
		xr_path CacheFile;
		dxRender_Visual* Visual = nullptr;
		U32Vec Pixels;
		ref_texture Texture;
	};

	static u32 GetDefHash(void* Def, bool IsGroup);
	static float GetPreviewTime(const Entry& Item);
	static void Simulate(Entry& Item);
	static bool LoadCache(Entry& Item);
	static void SaveCache(const xr_path& CacheFile, u32 Hash, const U32Vec& Pixels);

	bool CreateVisual(Entry& Item);
	void DestroyVisual(Entry& Item);
	bool Render(Entry& Item);
	bool CreateTexture(Entry& Item);

private:
	xr_hash_map<xr_string, xr_unique_ptr<Entry>> Entries;
	xr_vector<Entry*> Queue;
	xr_task_group Tasks;

	ref_rt TargetColor;
	ref_rt TargetDepth;
};
