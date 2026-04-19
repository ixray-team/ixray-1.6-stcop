#include "StdAfx.h"

#include "../../xrCore/LocatorAPI_defs.h"
#include "../../xrCore/_std_extensions.h"
#include "../../xrCore/Xr_ini.h"
#include "IxAiStackConfigLoad.h"
#include "IxAiBtTreeRegistry.h"
#include "IxAiStackTuning.h"

namespace
{
bool TryLoadFromDefaultPathImpl()
{
    string_path path{};
    FS.update_path(path, _game_config_, "misc\\ix_ai_stack.ltx");

    if (FS.exist(path) == nullptr)
    {
        return false;
    }

    CInifile ini(path);

    IxAiBtTreeRegistryResetToCodeDefaults();

    constexpr LPCSTR kTuning = "ix_ai_tuning";

    if (!ini.section_exist(kTuning))
    {
        Msg("! [IX AI]: ix_ai_stack.ltx missing section [ix_ai_tuning]");
        return false;
    }

    IxAiRuntimeTuning& tuning = g_ixAiRuntimeTuning;

    if (ini.line_exist(kTuning, "silenced_shot_power_cutoff"))
    {
        tuning.silencedShotPowerCutoff = ini.r_float(kTuning, "silenced_shot_power_cutoff");
    }

    if (ini.line_exist(kTuning, "suspicion_to_suspicious"))
    {
        tuning.suspicionToSuspicious = ini.r_float(kTuning, "suspicion_to_suspicious");
    }

    if (ini.line_exist(kTuning, "suspicion_to_search"))
    {
        tuning.suspicionToSearch = ini.r_float(kTuning, "suspicion_to_search");
    }

    if (ini.line_exist(kTuning, "suspicion_to_combat"))
    {
        tuning.suspicionToCombat = ini.r_float(kTuning, "suspicion_to_combat");
    }

    if (ini.line_exist(kTuning, "global_suspicion_decay_scale"))
    {
        tuning.globalSuspicionDecayScale = ini.r_float(kTuning, "global_suspicion_decay_scale");
    }

    if (ini.line_exist(kTuning, "memory_decay_per_second"))
    {
        tuning.memoryDecayPerSecond = ini.r_float(kTuning, "memory_decay_per_second");
    }

    if (ini.line_exist(kTuning, "memory_suspicion_leak_scale"))
    {
        tuning.memorySuspicionLeakScale = ini.r_float(kTuning, "memory_suspicion_leak_scale");
    }

    if (ini.line_exist(kTuning, "memory_sample_weight_scale"))
    {
        tuning.memorySampleWeightScale = ini.r_float(kTuning, "memory_sample_weight_scale");
    }

    if (ini.line_exist(kTuning, "memory_strength_epsilon"))
    {
        tuning.memoryStrengthEpsilon = ini.r_float(kTuning, "memory_strength_epsilon");
    }

    if (ini.line_exist(kTuning, "tactics_guard_hold_distance"))
    {
        tuning.tacticsGuardHoldDistance = ini.r_float(kTuning, "tactics_guard_hold_distance");
    }

    if (ini.line_exist(kTuning, "tactics_flank_side_scale"))
    {
        tuning.tacticsFlankSideScale = ini.r_float(kTuning, "tactics_flank_side_scale");
    }

    if (ini.line_exist(kTuning, "tactics_feed_movement_hint"))
    {
        tuning.tacticsFeedMovementHint = ini.r_bool(kTuning, "tactics_feed_movement_hint") ? true : false;
    }

    if (ini.line_exist(kTuning, "tactic_hint_danger_cooldown_ms"))
    {
        tuning.tacticHintDangerCooldownMs = ini.r_u32(kTuning, "tactic_hint_danger_cooldown_ms");
    }

    if (ini.line_exist(kTuning, "cover_feed_danger_hint"))
    {
        tuning.coverFeedDangerHint = ini.r_bool(kTuning, "cover_feed_danger_hint") ? true : false;
    }

    if (ini.line_exist(kTuning, "cover_hint_danger_cooldown_ms"))
    {
        tuning.coverHintDangerCooldownMs = ini.r_u32(kTuning, "cover_hint_danger_cooldown_ms");
    }

    if (ini.line_exist(kTuning, "cover_hint_interval_frames"))
    {
        tuning.coverHintIntervalFrames = ini.r_u32(kTuning, "cover_hint_interval_frames");
    }

    constexpr LPCSTR kBridge = "ix_ai_bridge";

    if (ini.section_exist(kBridge))
    {
        if (ini.line_exist(kBridge, "enabled"))
        {
            tuning.bridgeEnabled = ini.r_bool(kBridge, "enabled") ? true : false;
        }

        if (ini.line_exist(kBridge, "ix_memory_authoritative"))
        {
            tuning.memoryAuthoritative = ini.r_bool(kBridge, "ix_memory_authoritative") ? true : false;
        }

        if (ini.line_exist(kBridge, "suspicious_cooldown_sec"))
        {
            tuning.bridgeSuspiciousCooldownSeconds = ini.r_float(kBridge, "suspicious_cooldown_sec");
        }

        if (ini.line_exist(kBridge, "search_cooldown_sec"))
        {
            tuning.bridgeSearchCooldownSeconds = ini.r_float(kBridge, "search_cooldown_sec");
        }

        if (ini.line_exist(kBridge, "combat_cooldown_sec"))
        {
            tuning.bridgeCombatCooldownSeconds = ini.r_float(kBridge, "combat_cooldown_sec");
        }
    }

    constexpr LPCSTR kPerception = "ix_ai_perception";

    if (ini.section_exist(kPerception))
    {
        if (ini.line_exist(kPerception, "visual_probe_interval_frames"))
        {
            tuning.visualProbeIntervalFrames = ini.r_u32(kPerception, "visual_probe_interval_frames");
        }

        if (ini.line_exist(kPerception, "visual_probe_intensity"))
        {
            tuning.visualProbeIntensity = ini.r_float(kPerception, "visual_probe_intensity");
        }

        if (ini.line_exist(kPerception, "visual_probe_radius"))
        {
            tuning.visualProbeRadius = ini.r_float(kPerception, "visual_probe_radius");
        }

        if (ini.line_exist(kPerception, "max_visual_probes_per_frame"))
        {
            tuning.maxVisualProbesPerFrame = ini.r_u32(kPerception, "max_visual_probes_per_frame");
        }

        if (ini.line_exist(kPerception, "vision_half_fov_degrees_fallback"))
        {
            tuning.visionHalfFovDegreesFallback = ini.r_float(kPerception, "vision_half_fov_degrees_fallback");
        }

        if (ini.line_exist(kPerception, "vision_ray_range_extra"))
        {
            tuning.visionRayRangeExtra = ini.r_float(kPerception, "vision_ray_range_extra");
        }

        if (ini.line_exist(kPerception, "vision_occlusion_depth_epsilon"))
        {
            tuning.visionOcclusionDepthEpsilon = ini.r_float(kPerception, "vision_occlusion_depth_epsilon");
        }

        if (ini.line_exist(kPerception, "vision_target_chest_height"))
        {
            tuning.visionTargetChestHeight = ini.r_float(kPerception, "vision_target_chest_height");
        }

        if (ini.line_exist(kPerception, "vision_max_distance_scale"))
        {
            tuning.visionMaxDistanceScale = ini.r_float(kPerception, "vision_max_distance_scale");
        }

        if (ini.line_exist(kPerception, "corpse_probe_interval_frames"))
        {
            tuning.corpseProbeIntervalFrames = ini.r_u32(kPerception, "corpse_probe_interval_frames");
        }

        if (ini.line_exist(kPerception, "corpse_probe_radius"))
        {
            tuning.corpseProbeRadius = ini.r_float(kPerception, "corpse_probe_radius");
        }

        if (ini.line_exist(kPerception, "corpse_event_intensity"))
        {
            tuning.corpseEventIntensity = ini.r_float(kPerception, "corpse_event_intensity");
        }
    }

    constexpr LPCSTR kSquadChannel = "ix_ai_squad_channel";

    if (ini.section_exist(kSquadChannel))
    {
        if (ini.line_exist(kSquadChannel, "enabled"))
        {
            tuning.squadChannelEnabled = ini.r_bool(kSquadChannel, "enabled") ? true : false;
        }

        if (ini.line_exist(kSquadChannel, "max_distance"))
        {
            tuning.squadChannelMaxDistance = ini.r_float(kSquadChannel, "max_distance");
        }

        if (ini.line_exist(kSquadChannel, "ally_wound_intensity"))
        {
            tuning.squadAllyWoundIntensity = ini.r_float(kSquadChannel, "ally_wound_intensity");
        }

        if (ini.line_exist(kSquadChannel, "ally_wound_radius"))
        {
            tuning.squadAllyWoundRadius = ini.r_float(kSquadChannel, "ally_wound_radius");
        }

        if (ini.line_exist(kSquadChannel, "combat_engaged_intensity"))
        {
            tuning.squadCombatEngagedIntensity = ini.r_float(kSquadChannel, "combat_engaged_intensity");
        }

        if (ini.line_exist(kSquadChannel, "combat_engaged_radius"))
        {
            tuning.squadCombatEngagedRadius = ini.r_float(kSquadChannel, "combat_engaged_radius");
        }

        if (ini.line_exist(kSquadChannel, "suspicion_scale"))
        {
            tuning.squadChannelSuspicionScale = ini.r_float(kSquadChannel, "suspicion_scale");
        }

        if (ini.line_exist(kSquadChannel, "focus_intensity_min"))
        {
            tuning.squadChannelFocusIntensityMin = ini.r_float(kSquadChannel, "focus_intensity_min");
        }

        if (ini.line_exist(kSquadChannel, "stealth_fanout_enabled"))
        {
            tuning.squadFanoutStealthHitHandlingEnabled = ini.r_bool(kSquadChannel, "stealth_fanout_enabled") ? true : false;
        }

        if (ini.line_exist(kSquadChannel, "stealth_clear_attacker_id"))
        {
            tuning.squadFanoutClearAttackerIdOnStealthHit = ini.r_bool(kSquadChannel, "stealth_clear_attacker_id") ? true : false;
        }

        if (ini.line_exist(kSquadChannel, "stealth_victim_position_weight"))
        {
            tuning.squadFanoutStealthVictimPositionWeight = ini.r_float(kSquadChannel, "stealth_victim_position_weight");
        }

        if (ini.line_exist(kSquadChannel, "stealth_suppress_direct_focus"))
        {
            tuning.squadFanoutSuppressDirectFocusOnStealthHit = ini.r_bool(kSquadChannel, "stealth_suppress_direct_focus") ? true : false;
        }

        if (ini.line_exist(kSquadChannel, "stealth_suspicion_scale"))
        {
            tuning.squadFanoutStealthSuspicionScale = ini.r_float(kSquadChannel, "stealth_suspicion_scale");
        }
    }

    constexpr LPCSTR kLocality = "ix_ai_locality";

    if (ini.section_exist(kLocality))
    {
        if (ini.line_exist(kLocality, "actor_attenuation_enabled"))
        {
            tuning.localityActorAttenuationEnabled = ini.r_bool(kLocality, "actor_attenuation_enabled") ? true : false;
        }

        if (ini.line_exist(kLocality, "actor_soft_begin_distance"))
        {
            tuning.localityActorSoftBeginDistance = ini.r_float(kLocality, "actor_soft_begin_distance");
        }

        if (ini.line_exist(kLocality, "actor_hard_cutoff_distance"))
        {
            tuning.localityActorHardCutoffDistance = ini.r_float(kLocality, "actor_hard_cutoff_distance");
        }

        if (ini.line_exist(kLocality, "actor_min_intensity_scale"))
        {
            tuning.localityActorMinIntensityScale = ini.r_float(kLocality, "actor_min_intensity_scale");
        }

        if (ini.line_exist(kLocality, "actor_anchor_radius"))
        {
            tuning.localityActorAnchorRadius = ini.r_float(kLocality, "actor_anchor_radius");
        }
    }

    constexpr LPCSTR kPreset = "ix_ai_preset";

    if (ini.section_exist(kPreset))
    {
        if (ini.line_exist(kPreset, "active"))
        {
            const shared_str presetName = ini.r_string_wb(kPreset, "active");

            if (xr_strcmp(presetName.c_str(), "aggressive") == 0)
            {
                constexpr LPCSTR kAggressive = "preset_aggressive";

                if (ini.section_exist(kAggressive))
                {
                    if (ini.line_exist(kAggressive, "suspicion_to_combat"))
                    {
                        tuning.suspicionToCombat = ini.r_float(kAggressive, "suspicion_to_combat");
                    }

                    if (ini.line_exist(kAggressive, "bridge_enabled"))
                    {
                        tuning.bridgeEnabled = ini.r_bool(kAggressive, "bridge_enabled") ? true : false;
                    }

                    if (ini.line_exist(kAggressive, "ix_memory_authoritative"))
                    {
                        tuning.memoryAuthoritative = ini.r_bool(kAggressive, "ix_memory_authoritative") ? true : false;
                    }
                }
            }
            else if (xr_strcmp(presetName.c_str(), "stealth") == 0)
            {
                constexpr LPCSTR kStealth = "preset_stealth";

                if (ini.section_exist(kStealth))
                {
                    if (ini.line_exist(kStealth, "suspicion_to_search"))
                    {
                        tuning.suspicionToSearch = ini.r_float(kStealth, "suspicion_to_search");
                    }

                    if (ini.line_exist(kStealth, "bridge_enabled"))
                    {
                        tuning.bridgeEnabled = ini.r_bool(kStealth, "bridge_enabled") ? true : false;
                    }

                    if (ini.line_exist(kStealth, "ix_memory_authoritative"))
                    {
                        tuning.memoryAuthoritative = ini.r_bool(kStealth, "ix_memory_authoritative") ? true : false;
                    }
                }
            }
        }
    }

    IxAiBtTreeRegistryTryLoadFromIni(ini);

    Msg("* [IX AI]: Loaded misc\\ix_ai_stack.ltx");
    return true;
}
} // namespace

bool IxAiStackConfigTryLoadFromDefaultPath()
{
    xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
    return TryLoadFromDefaultPathImpl();
}

bool IxAiStackRuntimeTuningReloadFromDefaultsAndOptionalFile()
{
    xrCriticalSectionGuard guard(g_ixAiRuntimeTuningCs);
    g_ixAiRuntimeTuning = IxAiRuntimeTuning{};
    return TryLoadFromDefaultPathImpl();
}
