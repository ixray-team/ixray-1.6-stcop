#include "stdafx.h"
#include "LevelResources.h"
#include "IGame_Level.h"

namespace
{
	struct SResourceNameEntry
	{
		ELevelResource Resource;
		const char*    Name;
	};

	constexpr SResourceNameEntry ResourceNames[] =
	{
		{ ELevelResource::LevelConfig,     "level_config"     },
		{ ELevelResource::RenderScene,     "render_scene"     },
		{ ELevelResource::Collision,       "collision"        },
		{ ELevelResource::AiMap,           "ai_map"           },
		{ ELevelResource::GameGraph,       "game_graph"       },
		{ ELevelResource::CrossTable,      "cross_table"      },
		{ ELevelResource::PatrolPaths,     "patrol_paths"     },
		{ ELevelResource::Spawn,           "spawn"            },
		{ ELevelResource::StaticParticles, "static_particles" },
		{ ELevelResource::StaticSounds,    "static_sounds"    },
		{ ELevelResource::SoundEnv,        "sound_env"        },
		{ ELevelResource::Ambience,        "ambience"         },
		{ ELevelResource::EnvModifiers,    "env_modifiers"    },
		{ ELevelResource::PhysicsWorld,    "physics_world"    },
		{ ELevelResource::Hud,             "hud"              },
		{ ELevelResource::LevelScripts,    "level_scripts"    },
	};
}

xr_string_view LevelResourceName(ELevelResource res)
{
	for (const SResourceNameEntry& entry : ResourceNames)
	{
		if (entry.Resource == res)
			return entry.Name;
	}
	return "unknown";
}

xr_string LevelResourceMaskToString(const Flags32& mask)
{
	if (mask.flags == u32(ELevelResource::All))
		return "all";
	if (mask.flags == 0)
		return "none";

	xr_string result;
	for (const SResourceNameEntry& entry : ResourceNames)
	{
		if (mask.test(u32(entry.Resource)))
		{
			if (!result.empty())
				result += ",";
			result += entry.Name;
		}
	}
	return result;
}

bool LevelResourceMaskFromString(xr_string_view text, Flags32& outMask)
{
	outMask.zero();

	xr_string work(text.data(), text.size());
	xr_vector<xr_string> tokens = work.Split(',');

	for (xr_string& token : tokens)
	{
		// trim whitespace
		size_t begin = token.find_first_not_of(" \t");
		size_t end = token.find_last_not_of(" \t");
		if (begin == xr_string::npos)
			continue;
		token = token.substr(begin, end - begin + 1);

		if (0 == _stricmp(token.c_str(), "all"))
		{
			outMask.assign(u32(ELevelResource::All));
			continue;
		}
		if (0 == _stricmp(token.c_str(), "none"))
		{
			outMask.zero();
			continue;
		}

		bool found = false;
		for (const SResourceNameEntry& entry : ResourceNames)
		{
			if (0 == _stricmp(token.c_str(), entry.Name))
			{
				outMask.set(u32(entry.Resource), true);
				found = true;
				break;
			}
		}
		if (!found)
		{
			Msg("! LevelResourceMaskFromString: unknown resource '%s'", token.c_str());
			return false;
		}
	}
	return true;
}

xr_string_view LevelResourceStateName(EResourceState state)
{
	switch (state)
	{
	case EResourceState::NotLoaded: return "not_loaded";
	case EResourceState::Loading:    return "loading";
	case EResourceState::Ready:      return "ready";
	case EResourceState::Missing:    return "missing";
	case EResourceState::Failed:     return "failed";
	default:                         return "unknown";
	}
}

Flags32 SLevelDesc::Validate() const
{
	Flags32 result = Resources;

	const auto require = [&](ELevelResource dependent, ELevelResource required)
	{
		if (result.test(u32(dependent)) && !result.test(u32(required)))
		{
			Msg("! SLevelDesc: resource '%s' requires '%s'; disabling '%s'",
				LevelResourceName(dependent).data(), LevelResourceName(required).data(), LevelResourceName(dependent).data());
			result.set(u32(dependent), false);
		}
	};

	// Cross table indexes into the game graph.
	require(ELevelResource::CrossTable, ELevelResource::GameGraph);
	require(ELevelResource::PhysicsWorld, ELevelResource::Collision);

	return result;
}

bool CurrentLevelIsParallel()
{
	return g_pGameLevel != nullptr && g_pGameLevel->IsParallel();
}
