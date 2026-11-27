#pragma once

// Individually loadable/unloadable level resources.
// A level descriptor (SLevelDesc) carries a mask of these; the load pipeline
// loads exactly the requested set and treats everything else as legally absent.
enum class ELevelResource : u32
{
	LevelConfig     = 1u << 0,  // level.ltx
	RenderScene     = 1u << 1,  // level stream + level.geom/geomx, shaders, sectors, HOM (renderer scene graph)
	Collision       = 1u << 2,  // level.cform (static collision model)
	AiMap           = 1u << 3,  // level.ai (level graph)
	GameGraph       = 1u << 4,  // game.graph / spawn graph chunk
	CrossTable      = 1u << 5,  // level.gct
	PatrolPaths     = 1u << 6,  // waypoints / patrol paths
	Spawn           = 1u << 7,  // level.spawn / alife.spawn
	StaticParticles = 1u << 8,  // level.ps_static
	StaticSounds    = 1u << 9,  // level.snd_static
	SoundEnv        = 1u << 10, // level.snd_env + level.som
	Ambience        = 1u << 11, // [sounds_random] from system.ltx
	EnvModifiers    = 1u << 12, // level.env_mod
	PhysicsWorld    = 1u << 13, // CPHWorld
	Hud             = 1u << 14, // CHUDManager + game UI
	LevelScripts    = 1u << 15, // [level_scripts] script process

	All             = 0xFFFFu,
	None            = 0u,
};

enum class EResourceState : u8
{
	NotLoaded,  // not requested or not attempted yet
	Loading,    // load in progress
	Ready,      // loaded and usable
	Missing,    // requested, but data absent - empty fallback active
	Failed,     // requested, present, but failed to parse - empty fallback active
};

// Description of how a level should be loaded. Defaults reproduce the legacy
// behavior exactly (all resources requested).
struct SLevelDesc
{
	Flags32 Resources;      // ELevelResource bits
	bool    Sandbox = false; // diagnostic flag for viewer-driven sessions
	// Parallel: the level is driven by an external pump (viewer) instead of the
	// global g_loading_events queue, does not claim g_pGameLevel, does not touch
	// the loading screen, input capture, HUD or the global sound environment.
	bool    Parallel = false;
	bool    RemoteTransport = false;
	shared_str SpawnFile = "all";

	SLevelDesc()
	{
		Resources.assign(u32(ELevelResource::All));
	}

	bool Has(ELevelResource res) const { return Resources.test(u32(res)); }

	// Fixes illegal combinations (e.g. CrossTable without GameGraph) and logs
	// every adjustment. Returns the adjusted mask.
	ENGINE_API Flags32 Validate() const;
};

ENGINE_API xr_string_view LevelResourceName(ELevelResource res);
ENGINE_API xr_string      LevelResourceMaskToString(const Flags32& mask);
// Parses a comma/space separated list of resource names (case-insensitive).
// Accepts "all"/"none". Returns false on unknown token.
ENGINE_API bool           LevelResourceMaskFromString(xr_string_view text, Flags32& outMask);
ENGINE_API xr_string_view LevelResourceStateName(EResourceState state);

// True when the level currently owning the global slot is a parallel (viewer
// sandbox) session. Light accessor for modules that cannot include
// IGame_Level.h (e.g. xrPhysics).
ENGINE_API bool CurrentLevelIsParallel();
