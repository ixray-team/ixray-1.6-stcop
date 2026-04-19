#include "StdAfx.h"

#include "../../3rd-party/imgui/imgui.h"
#include "../../xrCore/EngineExternal.h"
#include "../../xrEngine/device.h"
#include "../../xrEngine/EngineAPI.h"
#include "../../xrEngine/IGame_Level.h"
#include "../Level.h"
#include "IxAiAgent.h"
#include "IxAiConstants.h"
#include "IxAiManager.h"
#include "IxAiPerceptionSystem.h"
#include "IxAiStackApi.h"
#include "IxAiStackImGui.h"
#include "IxAiStackTelemetry.h"
#include "IxAiStackTuning.h"

#if defined(DEBUG) && !defined(MASTER_GOLD)
#    include "PHDebug.h"
#endif

namespace
{
    constexpr f32 kIxAiWindowBgAlpha = 0.65f;

    bool s_drawPerceptionSpheres = false;
    bool s_drawTacticalHints = false;

    const char* AlertLevelName(IxAiAlertLevel level)
    {
        switch (level)
        {
        case IxAiAlertLevel::Vigilant:
            return "Vigilant";
        case IxAiAlertLevel::Suspicious:
            return "Suspicious";
        case IxAiAlertLevel::Search:
            return "Search";
        case IxAiAlertLevel::Combat:
            return "Combat";
        default:
            return "None";
        }
    }

    const char* BehaviourKindName(IxAiBehaviourKind kind)
    {
        switch (kind)
        {
        case IxAiBehaviourKind::GuardBasic:
            return "GuardBasic";
        case IxAiBehaviourKind::FlankerLite:
            return "FlankerLite";
        default:
            return "?";
        }
    }

    const char* PerceptionTypeName(IxAiPerceptionEventType type)
    {
        switch (type)
        {
        case IxAiPerceptionEventType::SoundGunshot:
            return "SoundGunshot";
        case IxAiPerceptionEventType::SoundSilenced:
            return "SoundSilenced";
        case IxAiPerceptionEventType::SoundFootstep:
            return "SoundFootstep";
        case IxAiPerceptionEventType::SoundCry:
            return "SoundCry";
        case IxAiPerceptionEventType::SoundWeaponHandling:
            return "SoundWeaponHandling";
        case IxAiPerceptionEventType::SoundWeaponSurface:
            return "SoundWeaponSurface";
        case IxAiPerceptionEventType::SoundPhysics:
            return "SoundPhysics";
        case IxAiPerceptionEventType::SoundExplosion:
            return "SoundExplosion";
        case IxAiPerceptionEventType::SoundItemFumble:
            return "SoundItemFumble";
        case IxAiPerceptionEventType::SoundBoltImpact:
            return "SoundBoltImpact";
        case IxAiPerceptionEventType::VisualPlayer:
            return "VisualPlayer";
        case IxAiPerceptionEventType::VisualCorpse:
            return "VisualCorpse";
        case IxAiPerceptionEventType::VisualBlood:
            return "VisualBlood";
        case IxAiPerceptionEventType::LightSource:
            return "LightSource";
        case IxAiPerceptionEventType::SquadAllyWounded:
            return "SquadAllyWounded";
        case IxAiPerceptionEventType::SquadCombatEngaged:
            return "SquadCombatEngaged";
        case IxAiPerceptionEventType::Other:
            return "Other";
        default:
            return "None";
        }
    }

    void DrawOverviewTab(bool isStackLive)
    {
        if (isStackLive)
        {
            ImGui::TextColored(ImVec4(0.4f, 0.95f, 0.45f, 1.f), "Stack state: RUNNING");
        }
        else
        {
            ImGui::TextColored(ImVec4(0.95f, 0.55f, 0.35f, 1.f), "Stack state: OFFLINE");
            ImGui::TextWrapped(
                "Runtime tuning below still applies. Agents and overlays need a loaded single-player level, "
                "EnableIxAiStack = true in gamedata/configs/engine_external.ltx ([gameplay]), then reload the save or level.");
        }

        ImGui::SeparatorText("Activation checklist");

        const bool dedicatedOk = !g_dedicated_server;
        const bool copOk = EngineExternal().CallOfPripyatMode();
        const bool flagOk = EngineExternal()[EEngineExternalGame::EnableIxAiStack];
        const bool spOk = IsGameTypeSingle();
        const bool levelOk = (g_pGameLevel != nullptr);

        ImGui::BulletText("Dedicated server clear: %s", dedicatedOk ? "ok" : "BLOCKED");
        ImGui::BulletText("Target game mode: %s", copOk ? "ok" : "BLOCKED");
        ImGui::BulletText("EnableIxAiStack (engine_external): %s", flagOk ? "ok" : "BLOCKED (set true, reload level)");
        ImGui::BulletText("Singleplayer: %s", spOk ? "ok" : "BLOCKED");
        ImGui::BulletText("Game level object: %s", levelOk ? "ok" : "BLOCKED (not in game)");

        if (g_pGameLevel != nullptr)
        {
            ImGui::BulletText("Level ready (bReady): %s", g_pGameLevel->bReady ? "yes" : "no (still loading)");
        }

        ImGui::SeparatorText("Telemetry");

        if (isStackLive)
        {
            IxAiManager* manager = IxAiStackApi::Manager();
            VERIFY(manager != nullptr);
            ImGui::Text("Last manager update: %.3f ms", manager->GetLastUpdateDurationMs());
            ImGui::Text("Visual probes (last frame): %u", manager->GetLastVisualProbeCount());
            ImGui::Text("Corpse probes (last frame): %u", manager->GetLastCorpseProbeCount());
        }
        else
        {
            ImGui::TextDisabled("No manager update (stack not initialized).");
        }

        ImGui::Text(
            "Last frame counters - bridge: %u, sound ingests: %u, tactic danger: %u, cover danger: %u",
            IxAiStackTelemetry_GetBridgePushCountLastFrame(),
            IxAiStackTelemetry_GetSoundIngestCountLastFrame(),
            IxAiStackTelemetry_GetTacticHintPushCountLastFrame(),
            IxAiStackTelemetry_GetCoverHintPushCountLastFrame());

        ImGui::Text("Frame delta: %.3f ms (Device.fTimeDelta)", Device.fTimeDelta * 1000.f);
    }

    void DrawAgentsTab(IxAiManager* manager, bool isStackLive)
    {
        if (!isStackLive || manager == nullptr)
        {
            ImGui::TextDisabled("Agent registry is empty until the stack is running on a loaded level.");
            return;
        }

        ImGui::Text("Agent count: %u", manager->GetAgentCount());

        static ImGuiTableFlags flags = ImGuiTableFlags_Resizable | ImGuiTableFlags_Reorderable | ImGuiTableFlags_RowBg | ImGuiTableFlags_BordersOuter |
            ImGuiTableFlags_BordersV | ImGuiTableFlags_ScrollY;
        const float outerHeight = ImGui::GetTextLineHeightWithSpacing() * 18.f;

        if (ImGui::BeginTable("ix_ai_agents", 8, flags, ImVec2(0.f, outerHeight)))
        {
            ImGui::TableSetupScrollFreeze(0, 1);
            ImGui::TableSetupColumn("id");
            ImGui::TableSetupColumn("alert");
            ImGui::TableSetupColumn("suspicion");
            ImGui::TableSetupColumn("profile");
            ImGui::TableSetupColumn("sensory");
            ImGui::TableSetupColumn("working");
            ImGui::TableSetupColumn("focus");
            ImGui::TableSetupColumn("tactical");
            ImGui::TableHeadersRow();

            for (u32 agentIndex = 0; agentIndex < manager->GetAgentCount(); ++agentIndex)
            {
                const IxAiAgent* agent = manager->GetAgentByIndex(agentIndex);
                ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::Text("%u", (u32)agent->GetObjectId());
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(AlertLevelName(agent->GetAlertLevel()));
                ImGui::TableSetColumnIndex(2);
                ImGui::Text("%.2f", agent->GetSuspicionScore());
                ImGui::TableSetColumnIndex(3);
                ImGui::TextUnformatted(BehaviourKindName(agent->GetProfile()._behaviourKind));
                ImGui::TableSetColumnIndex(4);
                ImGui::Text("%u", agent->GetMemorySlotCount());
                ImGui::TableSetColumnIndex(5);
                ImGui::Text("%u", agent->GetMemoryModel().GetWorkingBeliefCount());
                ImGui::TableSetColumnIndex(6);
                ImGui::Text("%s", agent->HasLastFocus() ? "yes" : "no");
                ImGui::TableSetColumnIndex(7);
                ImGui::Text("%s", agent->HasTacticalHint() ? "yes" : "no");
            }

            ImGui::EndTable();
        }
    }

    void DrawPerceptionTab(IxAiManager* manager, bool isStackLive)
    {
        if (!isStackLive || manager == nullptr)
        {
            ImGui::TextDisabled("Global perception buffer is unavailable until the stack is running.");
        }
        else
        {
            const u32 eventCount = manager->Perception().GetGlobalEventCount();
            ImGui::Text(
                "Buffered events: %u (cap %u, retention %.0fs, xz cell %.0fm)",
                eventCount,
                IxAiConstants::kPerceptionGlobalEventCap,
                IxAiConstants::kPerceptionEventRetentionSeconds,
                IxAiConstants::kPerceptionSpatialCellSize);

            const u32 previewLimit = 64u;

            if (ImGui::BeginTable("ix_ai_perception", 5, ImGuiTableFlags_ScrollY | ImGuiTableFlags_RowBg | ImGuiTableFlags_BordersOuter, ImVec2(0.f, 220.f)))
            {
                ImGui::TableSetupColumn("t");
                ImGui::TableSetupColumn("type");
                ImGui::TableSetupColumn("I");
                ImGui::TableSetupColumn("R");
                ImGui::TableSetupColumn("pos");
                ImGui::TableHeadersRow();

                for (u32 eventIndex = 0; eventIndex < eventCount && eventIndex < previewLimit; ++eventIndex)
                {
                    const IxAiPerceptionEvent& event = manager->Perception().GetGlobalEvent(eventIndex);
                    ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::Text("%.2f", event._timestamp);
                    ImGui::TableSetColumnIndex(1);
                    ImGui::TextUnformatted(PerceptionTypeName(event._type));
                    ImGui::TableSetColumnIndex(2);
                    ImGui::Text("%.2f", event._intensity);
                    ImGui::TableSetColumnIndex(3);
                    ImGui::Text("%.2f", event._radius);
                    ImGui::TableSetColumnIndex(4);
                    ImGui::Text("(%.1f, %.1f, %.1f)", event._position.x, event._position.y, event._position.z);
                }

                ImGui::EndTable();
            }
        }

        ImGui::SeparatorText("Visual probe tuning (applies when stack runs)");
        ImGui::SliderInt("Visual probe interval (frames)", (int*)&g_ixAiRuntimeTuning.visualProbeIntervalFrames, 1, 20);
        ImGui::SliderFloat("Visual probe intensity", &g_ixAiRuntimeTuning.visualProbeIntensity, 0.1f, 3.f);
        ImGui::SliderFloat("Visual probe radius", &g_ixAiRuntimeTuning.visualProbeRadius, 0.5f, 15.f);
        ImGui::SliderInt("Max visual probes / frame", (int*)&g_ixAiRuntimeTuning.maxVisualProbesPerFrame, 1, 128);

        ImGui::SeparatorText("IX vision LOS (FOV + RayPick)");
        ImGui::SliderFloat("FOV fallback half-angle (deg)", &g_ixAiRuntimeTuning.visionHalfFovDegreesFallback, 5.f, 85.f);
        ImGui::SliderFloat("Ray range past target (m)", &g_ixAiRuntimeTuning.visionRayRangeExtra, 0.f, 3.f);
        ImGui::SliderFloat("Occlusion depth epsilon (m)", &g_ixAiRuntimeTuning.visionOcclusionDepthEpsilon, 0.f, 0.5f);
        ImGui::SliderFloat("Aim chest height (m)", &g_ixAiRuntimeTuning.visionTargetChestHeight, 0.f, 1.8f);
        ImGui::SliderFloat("Max distance vs eye_range", &g_ixAiRuntimeTuning.visionMaxDistanceScale, 0.5f, 2.f);

        ImGui::SeparatorText("Corpse probe tuning (VisualCorpse events near actor)");
        ImGui::SliderInt("Corpse probe interval (frames)", (int*)&g_ixAiRuntimeTuning.corpseProbeIntervalFrames, 1, 60);
        ImGui::SliderFloat("Corpse probe radius", &g_ixAiRuntimeTuning.corpseProbeRadius, 5.f, 120.f);
        ImGui::SliderFloat("Corpse event intensity", &g_ixAiRuntimeTuning.corpseEventIntensity, 0.05f, 2.f);

        ImGui::SeparatorText("Squad channel (ally wound / combat fan-out)");
        ImGui::Checkbox("Squad channel enabled", &g_ixAiRuntimeTuning.squadChannelEnabled);
        ImGui::SliderFloat("Squad max distance (0 = off)", &g_ixAiRuntimeTuning.squadChannelMaxDistance, 0.f, 400.f);
        ImGui::SliderFloat("Ally wound intensity", &g_ixAiRuntimeTuning.squadAllyWoundIntensity, 0.1f, 4.f);
        ImGui::SliderFloat("Ally wound radius", &g_ixAiRuntimeTuning.squadAllyWoundRadius, 1.f, 40.f);
        ImGui::SliderFloat("Combat engaged intensity", &g_ixAiRuntimeTuning.squadCombatEngagedIntensity, 0.1f, 4.f);
        ImGui::SliderFloat("Combat engaged radius", &g_ixAiRuntimeTuning.squadCombatEngagedRadius, 1.f, 60.f);
        ImGui::SliderFloat("Squad suspicion scale", &g_ixAiRuntimeTuning.squadChannelSuspicionScale, 0.f, 1.5f);
        ImGui::SliderFloat("Squad focus intensity min", &g_ixAiRuntimeTuning.squadChannelFocusIntensityMin, 0.f, 2.f);

        ImGui::SeparatorText("Squad stealth (melee-like hits: strike / wound_2 / physic_strike)");
        ImGui::Checkbox("Stealth fan-out handling", &g_ixAiRuntimeTuning.squadFanoutStealthHitHandlingEnabled);
        ImGui::Checkbox("Stealth: clear attacker id in event", &g_ixAiRuntimeTuning.squadFanoutClearAttackerIdOnStealthHit);
        ImGui::SliderFloat("Stealth: victim position weight", &g_ixAiRuntimeTuning.squadFanoutStealthVictimPositionWeight, 0.f, 1.f);
        ImGui::Checkbox("Stealth: suppress direct focus snap", &g_ixAiRuntimeTuning.squadFanoutSuppressDirectFocusOnStealthHit);
        ImGui::SliderFloat("Stealth: suspicion scale", &g_ixAiRuntimeTuning.squadFanoutStealthSuspicionScale, 0.05f, 1.5f);
    }

    void DrawStealthTab()
    {
        ImGui::SliderFloat("Silenced shot power cutoff", &g_ixAiRuntimeTuning.silencedShotPowerCutoff, 0.01f, 1.f);
        ImGui::SliderFloat("Suspicion to Suspicious", &g_ixAiRuntimeTuning.suspicionToSuspicious, 0.01f, 2.f);
        ImGui::SliderFloat("Suspicion to Search", &g_ixAiRuntimeTuning.suspicionToSearch, 0.1f, 6.f);
        ImGui::SliderFloat("Suspicion to Combat", &g_ixAiRuntimeTuning.suspicionToCombat, 0.5f, 12.f);
        ImGui::SliderFloat("Global suspicion decay scale", &g_ixAiRuntimeTuning.globalSuspicionDecayScale, 0.1f, 4.f);

        ImGui::SeparatorText("Memory (per agent)");
        ImGui::SliderFloat("Memory decay / sec", &g_ixAiRuntimeTuning.memoryDecayPerSecond, 0.1f, 4.f);
        ImGui::SliderFloat("Memory suspicion leak scale", &g_ixAiRuntimeTuning.memorySuspicionLeakScale, 0.f, 0.5f);
        ImGui::SliderFloat("Memory sample weight scale", &g_ixAiRuntimeTuning.memorySampleWeightScale, 0.05f, 1.f);
        ImGui::SliderFloat("Memory strength epsilon", &g_ixAiRuntimeTuning.memoryStrengthEpsilon, 0.001f, 0.1f);
    }

    void DrawTacticsTab()
    {
        ImGui::SliderFloat("Guard hold distance", &g_ixAiRuntimeTuning.tacticsGuardHoldDistance, 1.f, 12.f);
        ImGui::SliderFloat("Flank side scale", &g_ixAiRuntimeTuning.tacticsFlankSideScale, 0.05f, 1.f);

        ImGui::SeparatorText("Experimental movement bias (danger at tactical hint)");
        ImGui::Checkbox("Feed tactical hint as EnemySound danger", &g_ixAiRuntimeTuning.tacticsFeedMovementHint);
        ImGui::SliderInt("Tactic hint danger cooldown (ms)", (int*)&g_ixAiRuntimeTuning.tacticHintDangerCooldownMs, 200, 5000);

        ImGui::SeparatorText("Experimental cover bias (lateral danger nudge)");
        ImGui::Checkbox("Feed cover-side danger (Search/Combat)", &g_ixAiRuntimeTuning.coverFeedDangerHint);
        ImGui::SliderInt("Cover hint interval (frames)", (int*)&g_ixAiRuntimeTuning.coverHintIntervalFrames, 1, 60);
        ImGui::SliderInt("Cover hint danger cooldown (ms)", (int*)&g_ixAiRuntimeTuning.coverHintDangerCooldownMs, 200, 6000);
    }

    void DrawBridgeTab()
    {
        if (ImGui::Button("Reload misc\\\\ix_ai_stack.ltx (defaults + file)"))
        {
            IxAiStackApi::ReloadRuntimeConfig();
        }

        ImGui::Checkbox("Enable legacy bridge (danger / enemy nudges)", &g_ixAiRuntimeTuning.bridgeEnabled);
        ImGui::Checkbox("IX memory authoritative (Suspicious/Search danger focus from slots)", &g_ixAiRuntimeTuning.memoryAuthoritative);
        ImGui::SliderFloat("Suspicious danger cooldown (s)", &g_ixAiRuntimeTuning.bridgeSuspiciousCooldownSeconds, 0.5f, 12.f);
        ImGui::SliderFloat("Search danger cooldown (s)", &g_ixAiRuntimeTuning.bridgeSearchCooldownSeconds, 0.5f, 12.f);
        ImGui::SliderFloat("Combat push cooldown (s)", &g_ixAiRuntimeTuning.bridgeCombatCooldownSeconds, 0.2f, 8.f);
        ImGui::TextWrapped(
            "Bridge feeds CDangerObject / make_object_visible_somewhen / set_enemy. "
            "Keep off while tuning; enable for gameplay experiments.");
    }

    void DrawDebugDrawTab(IxAiManager* manager, bool isStackLive)
    {
        ImGui::Checkbox("Draw perception spheres", &s_drawPerceptionSpheres);
        ImGui::Checkbox("Draw tactical hints (agent -> hint)", &s_drawTacticalHints);

        if (!isStackLive || manager == nullptr)
        {
            ImGui::TextDisabled("World overlays need a running stack and a loaded level (bReady).");
            return;
        }

#if defined(DEBUG) && !defined(MASTER_GOLD)
        if (g_pGameLevel == nullptr || !g_pGameLevel->bReady)
        {
            ImGui::TextDisabled("Level not ready; overlays are skipped.");
            return;
        }

        if (s_drawPerceptionSpheres)
        {
            const u32 eventCount = manager->Perception().GetGlobalEventCount();

            for (u32 eventIndex = 0; eventIndex < eventCount; ++eventIndex)
            {
                const IxAiPerceptionEvent& event = manager->Perception().GetGlobalEvent(eventIndex);
                DBG_DrawPoint(event._position, 0.15f, color_xrgb(0, 200, 255));
            }
        }

        if (s_drawTacticalHints)
        {
            for (u32 agentIndex = 0; agentIndex < manager->GetAgentCount(); ++agentIndex)
            {
                const IxAiAgent* agent = manager->GetAgentByIndex(agentIndex);

                if (!agent->HasTacticalHint())
                {
                    continue;
                }

                CObject* objectPtr = g_pGameLevel->Objects.net_Find(agent->GetObjectId());

                if (objectPtr == nullptr || objectPtr->getDestroy())
                {
                    continue;
                }

                const Fvector from = objectPtr->Position();
                const Fvector to = agent->GetTacticalHintPosition();
                DBG_DrawLine(from, to, color_xrgb(255, 180, 0));
            }
        }
#else
        ImGui::TextDisabled("Debug draw requires DEBUG && !MASTER_GOLD.");
#endif
    }
} // namespace

void RenderIxAiStackWindow()
{
    if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_IxAiStackManager)])
    {
        return;
    }

    ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.0f, 0.0f, 0.0f, kIxAiWindowBgAlpha));
    if (!ImGui::Begin("IX AI Stack", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_IxAiStackManager)], ImGuiWindowFlags_None))
    {
        ImGui::End();
        ImGui::PopStyleColor();
        return;
    }

    const bool isStackLive = IxAiStackApi::IsActive();
    IxAiManager* manager = isStackLive ? IxAiStackApi::Manager() : nullptr;

    {
        xrCriticalSectionGuard tuningGuard(g_ixAiRuntimeTuningCs);

        if (ImGui::Button("Reset tuning defaults"))
        {
            IxAiRuntimeTuningResetDefaults();
        }

        ImGui::SameLine();
        ImGui::TextDisabled("(applies immediately when stack runs)");

        if (ImGui::BeginTabBar("IxAiTabs", ImGuiTabBarFlags_None))
        {
            if (ImGui::BeginTabItem("Overview"))
            {
                DrawOverviewTab(isStackLive);
                ImGui::EndTabItem();
            }

            if (ImGui::BeginTabItem("Agents"))
            {
                DrawAgentsTab(manager, isStackLive);
                ImGui::EndTabItem();
            }

            if (ImGui::BeginTabItem("Perception"))
            {
                DrawPerceptionTab(manager, isStackLive);
                ImGui::EndTabItem();
            }

            if (ImGui::BeginTabItem("Stealth"))
            {
                DrawStealthTab();
                ImGui::EndTabItem();
            }

            if (ImGui::BeginTabItem("Tactics"))
            {
                DrawTacticsTab();
                ImGui::EndTabItem();
            }

            if (ImGui::BeginTabItem("Bridge"))
            {
                DrawBridgeTab();
                ImGui::EndTabItem();
            }

            if (ImGui::BeginTabItem("Debug draw"))
            {
                DrawDebugDrawTab(manager, isStackLive);
                ImGui::EndTabItem();
            }

            ImGui::EndTabBar();
        }
    }

    ImGui::End();
    ImGui::PopStyleColor();
}

