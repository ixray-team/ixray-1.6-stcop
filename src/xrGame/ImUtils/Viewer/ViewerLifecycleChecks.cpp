#include "StdAfx.h"
#include "ViewerSession.h"
#include "ViewerEditors.h"
#include "../../Level.h"
#include "../../Actor.h"
#include "../../HUDManager.h"
#include "../../GameTask.h"
#include "../../GametaskManager.h"
#include "../../AnomalyZone.h"
#include "../../ui/UIGameCustom.h"
#include "../../ui/UITalkWnd.h"
#include "../../saved_game_wrapper.h"
#include "../../../xrEngine/ImGuiManager.h"
#include "../../../xrEngine/xr_collide_form.h"

extern CUIGameCustom* CurrentGameUI();
extern void start_tutorial(const char* name);
extern void stop_tutorial();

namespace ViewerLifecycleDetail
{
	const char* EditorName(ImUtils::Viewer::EViewerEditorKind kind);
	bool PrimaryReady();
	bool PreparePrimaryHudItem(u32 Wait);
}

namespace ImUtils::Viewer
{
	namespace ViewerEditorChecksDetail
	{
		const char* Control(EViewerEditorKind kind)
		{
			switch (kind)
			{
			case EViewerEditorKind::LevelSandbox: return "Spawn actor";
			case EViewerEditorKind::OgfPreview: return "Reset rotation";
			case EViewerEditorKind::MaterialEditor: return "Restore original material";
			case EViewerEditorKind::ParticleEditor: return "Play";
			case EViewerEditorKind::AiEditor: return "Spawn stalker";
			}
			return "";
		}
	}

	void CViewerSession::TickEditorChecks()
	{
		CLevelSlotGuard slotGuard(this, GetLevel(), ViewerState.Primary);
		const auto advance = [this]
		{
			++Lifecycle.CheckIndex;
			Lifecycle.CheckWait = 0;
		};
		const auto fail = [this](const char* reason)
		{
			Msg("! [editor-checks] failure stage=%u check=%u wait=%u probe=%d/%d/%d/%d control='%s'", u32(Lifecycle.Stage), Lifecycle.CheckIndex, Lifecycle.CheckWait,
				Lifecycle.Ui.Active, Lifecycle.Ui.RectangleSeen, Lifecycle.Ui.Pressed, Lifecycle.Ui.Activated, Lifecycle.Ui.Control);
			FinishLifecycleTest(false, reason);
		};
		if (++Lifecycle.CheckWait > 1200)
		{
			fail("editor check timed out");
			return;
		}

		switch (Lifecycle.CheckIndex)
		{
		case 0:
			UiProbeStart(ViewerEditorChecksDetail::Control(Lifecycle.ExpectedEditor));
			advance();
			break;
		case 1:
		{
			if (!Lifecycle.Ui.Activated)
				return;
			bool passed = Lifecycle.Ui.WindowSeen;
			switch (Lifecycle.ExpectedEditor)
			{
			case EViewerEditorKind::LevelSandbox:
				passed &= ViewerState.HasActor;
				break;
			case EViewerEditorKind::OgfPreview:
			case EViewerEditorKind::MaterialEditor:
			{
				const auto* editor = dynamic_cast<const CViewerOgfEditor*>(Mode());
				passed &= editor != nullptr && editor->PreviewObject(*this) != nullptr && fsimilar(editor->RotationYawAngle(), -PI_DIV_2) && fsimilar(editor->RotationPitchAngle(), 0.f);
				break;
			}
			case EViewerEditorKind::ParticleEditor:
			{
				const auto* editor = dynamic_cast<const CViewerParticleEditor*>(Mode());
				passed &= editor != nullptr && editor->PlayingForTest();
				break;
			}
			case EViewerEditorKind::AiEditor:
				passed &= ViewerState.LastSpawnedId >= 0 && GetLevel()->Objects.net_Find(ALife::_OBJECT_ID(ViewerState.LastSpawnedId)) != nullptr;
				break;
			}
			if (!passed)
				return;
			Msg("* [editor-checks] UI click passed for %s", ViewerLifecycleDetail::EditorName(Lifecycle.ExpectedEditor));
			advance();
			break;
		}
		case 2:
		{
			switch (Lifecycle.ExpectedEditor)
			{
			case EViewerEditorKind::LevelSandbox:
				BeginActorMovementTest(Lifecycle.ViewerFirst && Lifecycle.InitialEditorChecksDone);
				break;
			case EViewerEditorKind::OgfPreview:
			case EViewerEditorKind::MaterialEditor:
				if (Lifecycle.ExpectedEditor == EViewerEditorKind::OgfPreview)
				{
					if (!Mode()->HandleRequest(*this, EViewerEditorCommand::Rotate, "yaw", 30.f))
					{
						fail("OGF rotation request failed");
						return;
					}
				}
				else if (!Mode()->HandleRequest(*this, EViewerEditorCommand::SetParameter, "apply", 0.f))
				{
					fail("material apply request failed");
					return;
				}
				advance();
				return;
			case EViewerEditorKind::ParticleEditor:
				if (!Mode()->HandleRequest(*this, EViewerEditorCommand::SetParameter, "rate", 4.f))
				{
					fail("particle source-rate request failed");
					return;
				}
				advance();
				return;
			case EViewerEditorKind::AiEditor:
				if (!Mode()->HandleRequest(*this, EViewerEditorCommand::SetParameter, "destination", 1000.f))
				{
					fail("AI navigation request failed");
					return;
				}
				break;
			}
			advance();
			break;
		}
		case 3:
		{
			if (Lifecycle.ExpectedEditor == EViewerEditorKind::LevelSandbox && !ActorMovementPassed())
				return;
			if (Lifecycle.ExpectedEditor == EViewerEditorKind::OgfPreview)
			{
				const auto* editor = dynamic_cast<const CViewerOgfEditor*>(Mode());
				if (editor == nullptr || !fsimilar(editor->RotationYawAngle(), -PI_DIV_2 + deg2rad(30.f)))
				{
					fail("OGF rotation did not reach the requested angle");
					return;
				}
			}
			if (Lifecycle.ExpectedEditor == EViewerEditorKind::ParticleEditor)
			{
				const auto* editor = dynamic_cast<const CViewerParticleEditor*>(Mode());
				if (editor == nullptr || !fsimilar(editor->SourceRateForTest(), 4.f) || !editor->PlayingForTest())
				{
					fail("particle effect did not restart with the requested rate");
					return;
				}
			}
			if (Lifecycle.ExpectedEditor == EViewerEditorKind::AiEditor)
			{
				const auto* editor = dynamic_cast<const CViewerAiEditor*>(Mode());
				if (editor == nullptr || !editor->NavigationProbeCompletedForTest())
					return;
				if (!editor->NavigationProbePassedForTest())
				{
					fail("AI navigation probe failed");
					return;
				}
			}
			advance();
			break;
		}
		case 4:
		{
			if (Lifecycle.ExpectedEditor == EViewerEditorKind::OgfPreview || Lifecycle.ExpectedEditor == EViewerEditorKind::MaterialEditor || Lifecycle.ExpectedEditor == EViewerEditorKind::ParticleEditor)
			{
				const auto* editor = dynamic_cast<const CViewerEditor*>(Mode());
				if (editor == nullptr || !editor->History.CanUndo())
				{
					fail("editor history has no committed state for the UI undo check");
					return;
				}
				Lifecycle.HistoryChangedId = editor->History.CurrentId();
				UiProbeStart("Undo");
				Lifecycle.CheckIndex = 11;
				Lifecycle.CheckWait = 0;
				return;
			}
			if (!Mode()->HandleRequest(*this, EViewerEditorCommand::ApplyMemory, "8388608 131072 1", 0.f) || GpuPool.BudgetBytes() != 8388608)
			{
				fail("GPU/CPU pool reconfiguration failed");
				return;
			}
			advance();
			break;
		}
		case 11:
		{
			const auto* editor = dynamic_cast<const CViewerEditor*>(Mode());
			if (!Lifecycle.Ui.Activated)
				return;
			if (editor == nullptr || editor->History.CurrentId() + 1 != Lifecycle.HistoryChangedId)
			{
				fail("the real Undo button did not restore the preceding editor state");
				return;
			}
			if (Lifecycle.ExpectedEditor == EViewerEditorKind::OgfPreview)
			{
				const auto* ogf = dynamic_cast<const CViewerOgfEditor*>(Mode());
				if (ogf == nullptr || !fsimilar(ogf->RotationYawAngle(), -PI_DIV_2))
				{
					Msg("! [editor-checks] undo angle %.8f, expected %.8f, cursor %llu/%llu", ogf != nullptr ? ogf->RotationYawAngle() : flt_max, -PI_DIV_2,
						static_cast<unsigned long long>(editor->History.CurrentId()), static_cast<unsigned long long>(Lifecycle.HistoryChangedId));
					for (u32 index = 0; index < editor->History.EntryCount(); ++index)
					{
						const SViewerHistoryEntry* entry = editor->History.Entry(index);
						Msg("! [editor-checks] history %llu '%s'", static_cast<unsigned long long>(entry->Id), entry->Label);
					}
					fail("the real Undo button did not restore OGF rotation");
					return;
				}
			}
			UiProbeStart("Redo");
			Lifecycle.CheckIndex = 12;
			Lifecycle.CheckWait = 0;
			break;
		}
		case 12:
		{
			const auto* editor = dynamic_cast<const CViewerEditor*>(Mode());
			if (!Lifecycle.Ui.Activated)
				return;
			if (editor == nullptr || editor->History.CurrentId() != Lifecycle.HistoryChangedId || !Mode()->HandleRequest(*this, EViewerEditorCommand::TestHistory, nullptr, 0.f))
			{
				fail("real Redo, bounded editor history, or disk reopen failed");
				return;
			}
			if (!Mode()->HandleRequest(*this, EViewerEditorCommand::ApplyMemory, "8388608 131072 1", 0.f) || GpuPool.BudgetBytes() != 8388608)
			{
				fail("GPU/CPU pool reconfiguration failed");
				return;
			}
			Lifecycle.CheckIndex = 5;
			Lifecycle.CheckWait = 0;
			break;
		}
		case 5:
		{
			auto* editor = dynamic_cast<CViewerEditor*>(Mode());
			if (editor == nullptr || editor->CpuPool.Capacity() != 131072 || !Mode()->HandleRequest(*this, EViewerEditorCommand::ApplyMemory, "4194304 65536 1", 0.f) ||
				GpuPool.BudgetBytes() != 4194304 || editor->CpuPool.Capacity() != 65536 || !Mode()->HandleRequest(*this, EViewerEditorCommand::ApplyMemory, "4194304 65536 0", 0.f))
			{
				fail("GPU/CPU pool restoration failed");
				return;
			}
			Msg("* [editor-checks] history and memory checks passed for %s", ViewerLifecycleDetail::EditorName(Lifecycle.ExpectedEditor));
			advance();
			break;
		}
		case 6:
		{
			CCustomHUD* primaryHud = g_hud;
			if (Lifecycle.ExpectedEditor == EViewerEditorKind::LevelSandbox && (!VerifyHudNow() || !ToggleInventoryNow("open") || ToggleInventoryNow("close") || primaryHud != g_hud))
			{
				fail("stock HUD/inventory or primary HUD isolation failed");
				return;
			}
			if (ViewerLifecycleDetail::PrimaryReady())
			{
				if (!ViewerLifecycleDetail::PreparePrimaryHudItem(Lifecycle.CheckWait))
					return;
				const ImGuiViewport* viewport = ImGui::GetMainViewport();
				const float editorWidth = std::max(260.f, viewport->Size.x * 0.25f - 60.f);
				ImGui::SetWindowPos("OMF Editor##ToolsInGameImGui", ImVec2(viewport->Size.x - editorWidth - 20.f, 40.f), ImGuiCond_Always);
				ImGui::SetWindowSize("OMF Editor##ToolsInGameImGui", ImVec2(editorWidth, viewport->Size.y - 80.f), ImGuiCond_Always);
				Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)] = true;
				UiProbeStart("Select HUD motion", "OMF Editor##ToolsInGameImGui");
				Lifecycle.CheckIndex = 14;
				Lifecycle.CheckWait = 0;
				return;
			}
			Lifecycle.OtherEditorsPending = true;
			Lifecycle.CheckIndex = 8;
			Lifecycle.CheckWait = 0;
			break;
		}
		case 14:
			if (!Lifecycle.Ui.Activated)
				return;
			UiProbeStart("First available HUD motion");
			Lifecycle.CheckIndex = 15;
			Lifecycle.CheckWait = 0;
			break;
		case 15:
			if (!Lifecycle.Ui.Activated)
				return;
			UiProbeStart("Reset motion");
			Lifecycle.CheckIndex = 7;
			Lifecycle.CheckWait = 0;
			break;
		case 7:
			if (!Lifecycle.Ui.Activated)
				return;
			if (!Lifecycle.Ui.WindowSeen)
			{
				fail("OMF editor window or reset control was not rendered");
				return;
			}
			Msg("* [editor-checks] OMF reset UI passed with a parallel viewer active");
			UiProbeStart("Idle");
			Lifecycle.CheckIndex = 13;
			Lifecycle.CheckWait = 0;
			break;
		case 13:
			if (!Lifecycle.Ui.Activated)
				return;
			Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)] = false;
			if (!Lifecycle.Ui.WindowSeen)
			{
				fail("HUD motion player did not expose a usable idle control");
				return;
			}
			Msg("* [editor-checks] HUD motion idle UI passed with a parallel viewer active");
			Lifecycle.CheckIndex = 8;
			Lifecycle.CheckWait = 0;
			break;
		case 8:
		{
			const Flags32 primaryFlags = g_hud != nullptr ? HUD().world_prims.m_flags : Flags32{};
			if (!SetInspectorNow("ai_map", true) || (g_hud != nullptr && primaryFlags.flags != HUD().world_prims.m_flags.flags) || !ViewerState.InspectorSceneFlags.test(LevelInspector::ESF_DRAW_L_GRID))
			{
				fail("viewer AI-map inspector leaked into the primary inspector");
				return;
			}
			if (!SetInspectorNow("waypoints", true) || (g_hud != nullptr && primaryFlags.flags != HUD().world_prims.m_flags.flags))
			{
				fail("viewer waypoint inspector leaked into the primary inspector");
				return;
			}
			SetInspectorNow("all_off", false);
			if (ViewerState.InspectorSceneFlags.flags != 0 || ViewerState.InspectorZoneFlags.flags != 0 || (SessionInspector != nullptr && SessionInspector->m_flags.flags != 0))
			{
				fail("session inspector flags did not reset independently");
				return;
			}
			Msg("* [editor-checks] HUD and inspector isolation passed for %s", ViewerLifecycleDetail::EditorName(Lifecycle.ExpectedEditor));
			advance();
			break;
		}
		case 9:
		{
			const int result = RunResourceMatrixNow();
			if (result == 0)
				return;
			if (result != 1)
			{
				fail("resource load/unload matrix failed");
				return;
			}
			advance();
			break;
		}
		case 10:
			Msg("* [editor-checks] PASS editor=%s", ViewerLifecycleDetail::EditorName(Lifecycle.ExpectedEditor));
			Lifecycle.InitialEditorChecksDone = true;
			if (!ViewerLifecycleDetail::PrimaryReady())
			{
				Lifecycle.OtherEditorsPending = true;
				SetLifecycleStage(SViewerLifecycleTestState::EStage::StartPrimary);
			}
			else
			{
				Lifecycle.OtherEditorsPending = false;
				SetLifecycleStage(Lifecycle.GameplayCompleted ? SViewerLifecycleTestState::EStage::NextEditor : SViewerLifecycleTestState::EStage::SaveLoad);
			}
			break;
		}
	}

	void CViewerSession::TickPrimaryGameplayChecks()
	{
		const auto command = [this](const char* operation)
		{
			PrimaryTestNow(operation);
			++Lifecycle.CheckIndex;
			Lifecycle.CheckWait = 0;
		};
		if (++Lifecycle.CheckWait > 2400)
		{
			Msg("! [lifecycle] gameplay stall check=%u wait=%u npc=%u actor=%p ready=%d level=%s", Lifecycle.CheckIndex, Lifecycle.CheckWait, PrimaryNpcId,
				g_actor, ViewerLifecycleDetail::PrimaryReady(), g_pGameLevel != nullptr ? g_pGameLevel->name().c_str() : "<null>");
			FinishLifecycleTest(false, "primary gameplay probe timed out");
			return;
		}
		switch (Lifecycle.CheckIndex)
		{
		case 0:
			command("dialogue_prepare");
			Msg("* [lifecycle] dialogue prepare result npc=%u online=%p", PrimaryNpcId, PrimaryNpcId != ALife::INVALID_OBJECT_ID ? Level().Objects.net_Find(PrimaryNpcId) : nullptr);
			break;
		case 1:
			if (PrimaryNpcId == ALife::INVALID_OBJECT_ID || Level().Objects.net_Find(PrimaryNpcId) == nullptr)
				return;
			command("dialogue_start");
			break;
		case 2:
		{
			CUIGameCustom* ui = CurrentGameUI();
			if (!g_actor->IsTalking() || ui == nullptr || ui->TalkMenu == nullptr || !ui->TalkMenu->IsActiveTalkUi())
			{
				FinishLifecycleTest(false, "real dialogue did not open with the viewer active");
				return;
			}
			command("dialogue_stop");
			break;
		}
		case 3:
			if (g_actor->IsTalking())
				return;
			command("quest_start");
			break;
		case 4:
			if (Level().GameTaskManager()->HasGameTask("zat_b30_compass_to_barmen_task", false) == nullptr)
			{
				FinishLifecycleTest(false, "stock quest assignment failed");
				return;
			}
			command("quest_complete");
			break;
		case 5:
		{
			const CGameTask* task = Level().GameTaskManager()->HasGameTask("zat_b30_compass_to_barmen_task", false);
			if (task == nullptr || task->GetTaskState() != eTaskStateCompleted)
				return;
			if (load_screen_renderer.IsActive())
			{
				Console->Execute("viewer_primary_continue");
				return;
			}
			command("tutorial_start");
			break;
		}
		case 6:
			if (Level().Flow.Tutorial == nullptr)
				return;
			command("tutorial_stop");
			break;
		case 7:
			if (Level().Flow.Tutorial != nullptr)
				return;
			command("anomaly_prepare");
			break;
		case 8:
			if (PrimaryAnomalyId == ALife::INVALID_OBJECT_ID || Level().Objects.net_Find(PrimaryAnomalyId) == nullptr)
				return;
			command("anomaly_enter");
			break;
		case 9:
		{
			CObject* object = Level().Objects.net_Find(PrimaryAnomalyId);
			CGameObject* gameObject = object != nullptr ? object->cast_game_object() : nullptr;
			const CAnomalyZone* zone = gameObject != nullptr ? gameObject->cast_anomaly_zone() : nullptr;
			if (zone == nullptr)
				return;
			if (!SZoneObjectInfo::get(const_cast<CAnomalyZone*>(zone), g_actor))
			{
				if (Lifecycle.CheckWait % 120 == 0)
				{
					const Fvector& actorPosition = g_actor->Position();
					const Fvector& zonePosition = zone->Position();
					Msg("* [lifecycle] anomaly wait actor=[%.3f %.3f %.3f] zone=[%.3f %.3f %.3f] radius=%.3f", VPUSH(actorPosition), VPUSH(zonePosition), zone->CFORM()->getRadius());
				}
				return;
			}
			command("anomaly_leave");
			break;
		}
		case 10:
			if (!g_actor->g_Alive())
			{
				FinishLifecycleTest(false, "primary actor died during the anomaly probe");
				return;
			}
			Msg("* [lifecycle] primary dialogue, quest transition, cutscene and anomaly probes passed (not a campaign playthrough)");
			Lifecycle.GameplayCompleted = true;
			xr_strcpy(Lifecycle.ExpectedLevel, "jupiter");
			Console->Execute("jump_to_level jupiter");
			SetLifecycleStage(SViewerLifecycleTestState::EStage::ChangeLevel);
			break;
		}
	}
}
