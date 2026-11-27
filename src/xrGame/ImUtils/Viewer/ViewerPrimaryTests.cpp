#include "StdAfx.h"
#include "ViewerSession.h"
#include "../../Level.h"
#include "../../Actor.h"
#include "../../ai_space.h"
#include "../../alife_simulator.h"
#include "../../alife_object_registry.h"
#include "../../alife_story_registry.h"
#include "../../AnomalyZone.h"
#include "../../GamePersistent.h"
#include "../../GameObject.h"
#include "../../ai_object_location.h"
#include "../../GametaskManager.h"
#include "../../GameTask.h"
#include "../../InventoryOwner.h"
#include "../../Inventory.h"
#include "../../HudItem.h"
#include "../../MainMenu.h"
#include "../../ui/UIGameCustom.h"
#include "../../ui/UITalkWnd.h"
#include "../../ui/UIGameTutorial.h"
#include "../../../xrScripts/script_engine.h"
#include "../../../xrEngine/ImGuiManager.h"
#include "../../../xrEngine/xr_collide_form.h"
#include "../../../xrServerEntities/xrServer_Objects_ALife.h"
#include "../../../xrServerEntities/character_info.h"
#include "../../ScriptsSubsystems/StoryID/StoryIDManager.h"
#include "../../saved_game_wrapper.h"

extern void start_tutorial(const char* name);
extern void stop_tutorial();
extern CUIGameCustom* CurrentGameUI();

namespace ViewerPrimaryTestDetail
{
	bool Ready()
	{
		if (g_pGameLevel == nullptr || g_pGameLevel->IsParallel() || !g_pGameLevel->bReady || g_actor == nullptr || !g_actor->g_Alive())
		{
			Msg("! viewer_primary_test: a live primary actor is required");
			return false;
		}
		if (g_pGamePersistent->m_pMainMenu != nullptr && g_pGamePersistent->m_pMainMenu->IsActive())
		{
			Msg("! viewer_primary_test: the primary main menu is active; close it first");
			return false;
		}
		return true;
	}

	bool SameText(const char* left, const char* right)
	{
		return left != nullptr && right != nullptr && 0 == _stricmp(left, right);
	}

	bool IniStoryMatches(const CInifile& ini, const char* storyId)
	{
		if (!ini.section_exist("story_object"))
			return false;
		const char* key = nullptr;
		const char* value = nullptr;
		return ini.r_line("story_object", 0, &key, &value) && SameText(key, "story_id") && SameText(value, storyId);
	}

	ALife::_STORY_ID NumericStoryId(const char* storyId)
	{
		if (ai().get_script_engine() == nullptr)
			return ALife::_STORY_ID(-1);
		const luabind::object storyIds = luabind::get_globals(ai().script_engine().lua())["story_ids"];
		if (storyIds.type() != LUA_TTABLE)
			return ALife::_STORY_ID(-1);
		const luabind::object value = storyIds[storyId];
		return value.type() == LUA_TNUMBER ? ALife::_STORY_ID(luabind::object_cast<int>(value)) : ALife::_STORY_ID(-1);
	}

	bool StoryMatches(CSE_ALifeDynamicObject& object, const char* storyId, ALife::_STORY_ID numericId)
	{
		if (SameText(object.name(), storyId) || SameText(object.name_replace(), storyId) || SameText(object.m_script_story_ID.c_str(), storyId))
			return true;
		if (numericId != ALife::_STORY_ID(-1) && object.m_story_id == numericId)
			return true;
		if (SameText(CScriptStoryIDManager::GetInstance().GetID(object.ID), storyId) || IniStoryMatches(object.spawn_ini(), storyId))
			return true;
		return SameText(pSettings->read_if_exists<str_c>(object.name(), "story_id", nullptr), storyId);
	}

	CSE_ALifeDynamicObject* FindStoryObject(const char* storyId)
	{
		if (ai().get_alife() == nullptr)
			return nullptr;
		const ALife::_STORY_ID numericId = NumericStoryId(storyId);
		const ALife::_OBJECT_ID registeredId = CScriptStoryIDManager::GetInstance().GetID(storyId);
		if (CSE_ALifeDynamicObject* registered = ai().alife().objects().object(registeredId, true); registered != nullptr && StoryMatches(*registered, storyId, numericId))
			return registered;
		if (numericId != ALife::_STORY_ID(-1))
		{
			if (CSE_ALifeDynamicObject* registered = ai().alife().story_objects().object(numericId, true); registered != nullptr && StoryMatches(*registered, storyId, numericId))
				return registered;
		}
		u32 logged = 0;
		for (const auto& [id, object] : ai().alife().objects().objects())
		{
			if (object == nullptr)
				continue;
			if (StoryMatches(*object, storyId, numericId))
			{
				Msg("* viewer_test_dialogue: matched id=%u section='%s' name='%s' script-story='%s' story=%u online=%d", object->ID, object->name(), object->name_replace(),
					object->m_script_story_ID.c_str(), unsigned(object->m_story_id), object->m_bOnline);
				return object;
			}
			if (logged < 4 && ((object->name_replace() != nullptr && strstr(object->name_replace(), "barmen") != nullptr) || (object->name() != nullptr && strstr(object->name(), "barmen") != nullptr)))
			{
				++logged;
				Msg("* viewer_test_dialogue: candidate id=%u section='%s' name='%s' script-story='%s' story=%u online=%d", object->ID, object->name(), object->name_replace(),
					object->m_script_story_ID.c_str(), unsigned(object->m_story_id), object->m_bOnline);
			}
		}
		return nullptr;
	}

	CInventoryOwner* FindStoryClientObject(const char* storyId, ALife::_STORY_ID numericId)
	{
		const u32 count = Level().Objects.o_count();
		for (u32 index = 0; index < count; ++index)
		{
			CGameObject* object = Level().Objects.o_get_by_iterator(index)->cast_game_object();
			if (object == nullptr)
				continue;
			if (SameText(object->cName().c_str(), storyId) || SameText(object->cNameSect().c_str(), storyId) ||
				(numericId != ALife::_STORY_ID(-1) && object->story_id() == numericId) || (object->spawn_ini() != nullptr && IniStoryMatches(*object->spawn_ini(), storyId)))
				return object->cast_inventory_owner();
			CInventoryOwner* owner = object->cast_inventory_owner();
			if (owner != nullptr &&
				(SameText(owner->CharacterInfo().Profile().c_str(), storyId) || SameText(owner->CharacterInfo().GetSpecificCharacterId().c_str(), storyId)))
				return owner;
		}
		return nullptr;
	}

	CAnomalyZone* FindAnomaly(ALife::_OBJECT_ID id)
	{
		if (g_pGameLevel == nullptr || id == ALife::INVALID_OBJECT_ID)
			return nullptr;
		CObject* object = g_pGameLevel->Objects.net_Find(id);
		CGameObject* gameObject = object != nullptr ? object->cast_game_object() : nullptr;
		return gameObject != nullptr ? gameObject->cast_anomaly_zone() : nullptr;
	}

	CAnomalyZone* FindAvailableAnomaly()
	{
		CAnomalyZone* nearest = nullptr;
		float nearestDistance = flt_max;
		const u32 count = Level().Objects.o_count();
		for (u32 index = 0; index < count; ++index)
		{
			CGameObject* object = Level().Objects.o_get_by_iterator(index)->cast_game_object();
			CAnomalyZone* zone = object != nullptr ? object->cast_anomaly_zone() : nullptr;
			if (zone == nullptr || zone->cast_zone_campfire() != nullptr || !zone->IsEnabled() || zone->ZoneState() == CAnomalyZone::eZoneStateDisabled ||
				zone->CFORM() == nullptr || zone->getDestroy() || zone->CFORM()->getRadius() <= 0.1f)
				continue;
			const float distance = zone->Position().distance_to_sqr(g_actor->Position());
			if (distance < nearestDistance)
			{
				nearest = zone;
				nearestDistance = distance;
			}
		}
		return nearest;
	}

	CSE_ALifeDynamicObject* FindServerAnomaly()
	{
		if (ai().get_alife() == nullptr)
			return nullptr;
		CSE_ALifeDynamicObject* nearest = nullptr;
		float nearestDistance = flt_max;
		for (const auto& [id, object] : ai().alife().objects().objects())
		{
			if (object == nullptr || object->cast_anomalous_zone() == nullptr || object->m_bOnline)
				continue;
			const float distance = object->o_Position.distance_to_sqr(g_actor->Position());
			if (distance < nearestDistance)
			{
				nearest = object;
				nearestDistance = distance;
			}
		}
		return nearest;
	}

	void QuestStatus()
	{
		const shared_str id = "zat_b30_compass_to_barmen_task";
		const CGameTask* task = Level().GameTaskManager()->HasGameTask(id, false);
		Msg("* viewer_test_quest: present=%d state=%d", task != nullptr, task != nullptr ? int(task->GetTaskState()) : -1);
	}
}

void ImUtils::Viewer::CViewerSession::PrimaryTestNow(const char* args)
{
	string64 operation = {};
	if (args == nullptr || sscanf(args, "%63s", operation) != 1)
	{
		Msg("! viewer_primary_test <dialogue_prepare|dialogue_start|dialogue_stop|tutorial_start|tutorial_stop|quest_start|quest_complete|quest_status|anomaly_prepare|anomaly_enter|anomaly_status|anomaly_leave>");
		return;
	}
	if (!ViewerPrimaryTestDetail::Ready())
		return;

	if (0 == _stricmp(operation, "dialogue_prepare"))
	{
		constexpr const char* BarmanStory = "zat_a2_stalker_barmen";
		PrimaryNpcId = ALife::INVALID_OBJECT_ID;
		CSE_ALifeDynamicObject* object = ViewerPrimaryTestDetail::FindStoryObject(BarmanStory);
		if (object != nullptr)
		{
			PrimaryNpcId = object->ID;
			Fvector position = object->o_Position;
			position.x += 2.f;
			g_actor->SetActorPosition(position);
			if (!object->m_bOnline && ai().get_alife() != nullptr)
				ai().get_alife()->update_switch();
			Msg("* viewer_test_dialogue: server id=%u online=%d graph=%u used-ai=%d direct-control=%d client=%p", PrimaryNpcId, object->m_bOnline, object->m_tGraphID,
				object->used_ai_locations(), object->m_bDirectControl, Level().Objects.net_Find(PrimaryNpcId));
			return;
		}
		CInventoryOwner* partner = ViewerPrimaryTestDetail::FindStoryClientObject(BarmanStory, ViewerPrimaryTestDetail::NumericStoryId(BarmanStory));
		if (partner == nullptr)
		{
			Msg("! viewer_test_dialogue: the real barman is absent from the primary server and client; no substitute was spawned");
			return;
		}
		CGameObject* gameObject = partner->cast_game_object();
		if (gameObject == nullptr)
		{
			Msg("! viewer_test_dialogue: the matched barman has no client game object");
			return;
		}
		PrimaryNpcId = gameObject->ID();
		Msg("* viewer_test_dialogue: existing online barman id=%u section='%s'", PrimaryNpcId, gameObject->cNameSect().c_str());
		return;
	}
	if (0 == _stricmp(operation, "dialogue_start"))
	{
		CObject* object = Level().Objects.net_Find(PrimaryNpcId);
		CInventoryOwner* partner = object != nullptr ? object->cast_inventory_owner() : nullptr;
		if (partner == nullptr)
		{
			Msg("! viewer_test_dialogue: barman is not online on the primary client");
			return;
		}
		Fvector position = object->Position();
		position.x += 1.5f;
		g_actor->SetActorPosition(position);
		g_actor->RunTalkDialog(partner, false);
		CUIGameCustom* ui = CurrentGameUI();
		const bool shown = ui != nullptr && ui->TalkMenu != nullptr && ui->TalkMenu->IsActiveTalkUi();
		Msg("* viewer_test_dialogue: talking=%d ui-shown=%d partner=%u", g_actor->IsTalking(), shown, PrimaryNpcId);
		return;
	}
	if (0 == _stricmp(operation, "dialogue_stop"))
	{
		if (g_actor->IsTalking())
			g_actor->StopTalk();
		CUIGameCustom* ui = CurrentGameUI();
		const bool shown = ui != nullptr && ui->TalkMenu != nullptr && ui->TalkMenu->IsActiveTalkUi();
		Msg("* viewer_test_dialogue: closed=%d ui-shown=%d", !g_actor->IsTalking(), shown);
		return;
	}
	if (0 == _stricmp(operation, "tutorial_start"))
	{
		start_tutorial("intro_game");
		const CUISequencer* tutorial = Level().Flow.Tutorial;
		Msg("* viewer_test_tutorial: active=%d name='%s'", tutorial != nullptr, tutorial != nullptr ? tutorial->m_name : "invalid");
		return;
	}
	if (0 == _stricmp(operation, "tutorial_stop"))
	{
		stop_tutorial();
		Msg("* viewer_test_tutorial: stopped=%d", Level().Flow.Tutorial == nullptr);
		return;
	}
	if (0 == _stricmp(operation, "quest_start"))
	{
		try
		{
			luabind::functor<luabind::object> getManager;
			if (!ai().script_engine().functor("task_manager.get_task_manager", getManager))
			{
				Msg("! viewer_test_quest: stock task manager is unavailable");
				return;
			}
			luabind::object manager = getManager();
			luabind::object giveTask = manager["give_task"];
			if (giveTask.type() != LUA_TFUNCTION)
			{
				Msg("! viewer_test_quest: stock give_task is unavailable");
				return;
			}
			giveTask(manager, "zat_b30_compass_to_barmen_task");
			ViewerPrimaryTestDetail::QuestStatus();
		}
		catch (const std::exception& error)
		{
			Msg("! viewer_test_quest: %s", error.what());
		}
		return;
	}
	if (0 == _stricmp(operation, "quest_complete"))
	{
		g_actor->GiveInfoPortion("zat_b30_barmen_got_af");
		ViewerPrimaryTestDetail::QuestStatus();
		return;
	}
	if (0 == _stricmp(operation, "quest_status"))
	{
		ViewerPrimaryTestDetail::QuestStatus();
		return;
	}
	if (0 == _stricmp(operation, "anomaly_prepare"))
	{
		PrimaryAnomalyId = ALife::INVALID_OBJECT_ID;
		if (CAnomalyZone* zone = ViewerPrimaryTestDetail::FindAvailableAnomaly())
		{
			PrimaryAnomalyId = zone->ID();
			const Fvector& position = zone->Position();
			Msg("* viewer_test_anomaly: existing zone id=%u section='%s' enabled=%d position=[%.3f %.3f %.3f]", PrimaryAnomalyId, zone->cNameSect().c_str(), zone->IsEnabled(),
				position.x, position.y, position.z);
			return;
		}
		CSE_ALifeDynamicObject* object = ViewerPrimaryTestDetail::FindServerAnomaly();
		if (object == nullptr)
		{
			Msg("! viewer_test_anomaly: no real anomaly exists in the primary level; no substitute was spawned");
			return;
		}
		PrimaryAnomalyId = object->ID;
		g_actor->SetActorPosition(object->o_Position);
		if (ai().get_alife() != nullptr)
			ai().get_alife()->update_switch();
		Msg("* viewer_test_anomaly: existing server zone id=%u section='%s' online=%d client=%p", PrimaryAnomalyId, object->name(), object->m_bOnline, ViewerPrimaryTestDetail::FindAnomaly(PrimaryAnomalyId));
		return;
	}
	if (0 == _stricmp(operation, "anomaly_enter"))
	{
		CAnomalyZone* zone = ViewerPrimaryTestDetail::FindAnomaly(PrimaryAnomalyId);
		if (zone == nullptr)
		{
			Msg("! viewer_test_anomaly: client zone is unavailable");
			return;
		}
		g_actor->SetActorPosition(zone->Position());
		Msg("* viewer_test_anomaly: entered id=%u section='%s'", PrimaryAnomalyId, zone->cNameSect().c_str());
		return;
	}
	if (0 == _stricmp(operation, "anomaly_status"))
	{
		CAnomalyZone* zone = ViewerPrimaryTestDetail::FindAnomaly(PrimaryAnomalyId);
		if (zone == nullptr)
		{
			Msg("! viewer_test_anomaly: client zone is unavailable");
			return;
		}
		const bool inside = SZoneObjectInfo::get(zone, g_actor);
		Msg("* viewer_test_anomaly: inside=%d zone-state=%d actor-alive=%d primary-objects=%u", inside,
			int(zone->ZoneState()), g_actor->g_Alive(), Level().Objects.o_count());
		return;
	}
	if (0 == _stricmp(operation, "anomaly_leave"))
	{
		CAnomalyZone* zone = ViewerPrimaryTestDetail::FindAnomaly(PrimaryAnomalyId);
		if (zone != nullptr)
		{
			Fvector position = zone->Position();
			position.z += 12.f;
			g_actor->SetActorPosition(position);
		}
		Msg("* viewer_test_anomaly: left id=%u", PrimaryAnomalyId);
		PrimaryAnomalyId = ALife::INVALID_OBJECT_ID;
		return;
	}
	Msg("! viewer_primary_test: unknown operation '%s'", operation);
}


namespace ViewerLifecycleDetail
{
	constexpr const char* EditorNames[] = { "sandbox", "ogf", "material", "particle", "ai" };
	constexpr ImUtils::Viewer::EViewerEditorKind EditorKinds[] = {
		ImUtils::Viewer::EViewerEditorKind::LevelSandbox,
		ImUtils::Viewer::EViewerEditorKind::OgfPreview,
		ImUtils::Viewer::EViewerEditorKind::MaterialEditor,
		ImUtils::Viewer::EViewerEditorKind::ParticleEditor,
		ImUtils::Viewer::EViewerEditorKind::AiEditor,
	};
	constexpr const char* StageNames[] = { "viewer-ready", "start-primary", "skip-intro", "editor-checks", "save-load", "wait-save", "primary-gameplay", "change-level", "save-transition", "next-editor" };

	const char* EditorName(ImUtils::Viewer::EViewerEditorKind kind)
	{
		switch (kind)
		{
		case ImUtils::Viewer::EViewerEditorKind::LevelSandbox: return "sandbox";
		case ImUtils::Viewer::EViewerEditorKind::OgfPreview: return "ogf";
		case ImUtils::Viewer::EViewerEditorKind::MaterialEditor: return "material";
		case ImUtils::Viewer::EViewerEditorKind::ParticleEditor: return "particle";
		case ImUtils::Viewer::EViewerEditorKind::AiEditor: return "ai";
		}
		return "unknown";
	}

	bool PrimaryAvailable(bool allowLoading)
	{
		const CLevel* primary = ImUtils::Viewer::ViewerSessionDetail::Manager != nullptr ? ImUtils::Viewer::ViewerSessionDetail::Manager->PrimaryLevel : nullptr;
		if (primary == nullptr && g_pGameLevel != nullptr && !g_pGameLevel->IsParallel())
			primary = static_cast<const CLevel*>(g_pGameLevel);
		const CActor* actor = ImUtils::Viewer::ViewerSessionDetail::Manager != nullptr ? ImUtils::Viewer::ViewerSessionDetail::Manager->PrimaryActor : g_actor;
		return primary != nullptr && !primary->IsParallel() && primary->bReady && actor != nullptr && actor->g_Alive() &&
			(allowLoading || !load_screen_renderer.IsActive()) && (g_pGamePersistent->m_pMainMenu == nullptr || !g_pGamePersistent->m_pMainMenu->IsActive());
	}

	bool PrimaryCanContinue()
	{
		return PrimaryAvailable(true);
	}

	bool PrimaryReady();

	bool PreparePrimaryHudItem(u32 Wait)
	{
		if (!PrimaryReady() || ImUtils::Viewer::ViewerSessionDetail::Manager == nullptr)
			return false;
		ImUtils::Viewer::CPrimaryEditorContextGuard editorContext;
		ImUtils::Viewer::CViewerSessionManager& manager = *ImUtils::Viewer::ViewerSessionDetail::Manager;
		CActor* actor = manager.PrimaryActor;
		if (Wait % 120 == 0)
		{
			Msg("! [editor-checks] primary HUD prerequisites: actor=%p primary-hud=%p model=%p", actor, manager.PrimaryPlayerHud,
				manager.PrimaryPlayerHud != nullptr ? manager.PrimaryPlayerHud->GetModel() : nullptr);
		}
		if (actor == nullptr || manager.PrimaryPlayerHud == nullptr || manager.PrimaryPlayerHud->GetModel() == nullptr)
			return false;
		CInventory& inventory = actor->inventory();
		if (Wait % 120 == 0)
		{
			CUIGameCustom* gameUI = CurrentGameUI();
			Msg("! [editor-checks] primary input: disabled=%d precache=%u paused=%d local-player=%p current=%p actor=%p remote=%d ui=%p active=%u next=%u",
				manager.PrimaryLevel->InputDisabled, Device.dwPrecacheFrame, Device.Paused(), manager.PrimaryLevel->game != nullptr ? manager.PrimaryLevel->game->local_player : nullptr,
				manager.PrimaryLevel->CurrentEntity(), actor, actor->Remote(), gameUI != nullptr ? gameUI->TopInputReceiver() : nullptr, inventory.GetActiveSlot(), inventory.GetNextActiveSlot());
		}
		PIItem active = inventory.ActiveItem();
		CHudItem* activeHud = active != nullptr ? active->cast_hud_item() : nullptr;
		if (activeHud != nullptr && activeHud->HudItemData() != nullptr)
			return true;
		if (inventory.GetNextActiveSlot() != NO_ACTIVE_SLOT && inventory.ItemFromSlot(inventory.GetNextActiveSlot()) != nullptr)
			return false;

		constexpr u16 Slots[] = { INV_SLOT_2, INV_SLOT_3, KNIFE_SLOT, BINOCULAR_SLOT };
		constexpr EGameActions Actions[] = { kWPN_2, kWPN_3, kWPN_1, kWPN_6 };
		for (u32 index = 0; index < std::size(Slots); ++index)
		{
			PIItem item = inventory.ItemFromSlot(Slots[index]);
			if (item == nullptr || item->cast_hud_item() == nullptr)
				continue;
			if (inventory.IsSlotBlocked(item) && !actor->IsTalking() && (actor->GetMovementState(eReal) & mcClimb) == 0 &&
				(actor->HudAnimator() == nullptr || !actor->HudAnimator()->IsAnyAnimatorActive()))
			{
				actor->SetWeaponHideState(INV_STATE_BLOCK_ALL, false);
				Msg("* [editor-checks] restoring the primary actor's normal weapon visibility before the OMF check");
				return false;
			}
			for (int key = 0; key < SDL_SCANCODE_COUNT; ++key)
			{
				if (!is_binded(Actions[index], key))
					continue;
				if (Wait % 120 == 0)
				{
					Msg("! [editor-checks] equip guards: slot=%u current=%u can=%d blocked=%d skip=%d focus=%d external=%d animator=%d action=%d level-block=%d capture=%d",
						item->BaseSlot(), item->CurrSlot(), inventory.m_slots[Slots[index]].CanBeActivated(), inventory.IsSlotBlocked(item),
						manager.PrimaryLevel->game->local_player != nullptr && manager.PrimaryLevel->game->local_player->IsSkip(),
						actor->IsCameraFocusInputLocked(), actor->input_external_handler_installed(), actor->HudAnimator() != nullptr && actor->HudAnimator()->IsAnyAnimatorActive(),
						get_binded_action(key), manager.PrimaryLevel->is_block_action(Actions[index]), GImGUIManager->IsCapturingInputs());
				}
				manager.PrimaryLevel->IR_OnKeyboardPress(key);
				manager.PrimaryLevel->IR_OnKeyboardRelease(key);
				Msg("* [editor-checks] requested the primary inventory's existing '%s' through its normal key binding", item->object().cNameSect().c_str());
				return false;
			}
		}
		if (Wait % 120 == 0)
		{
			CUIGameCustom* gameUI = CurrentGameUI();
			Msg("! [editor-checks] primary input: disabled=%d precache=%u paused=%d local-player=%p current=%p actor=%p remote=%d ui=%p active=%u next=%u",
				manager.PrimaryLevel->InputDisabled, Device.dwPrecacheFrame, Device.Paused(), manager.PrimaryLevel->game != nullptr ? manager.PrimaryLevel->game->local_player : nullptr,
				manager.PrimaryLevel->CurrentEntity(), actor, actor->Remote(), gameUI != nullptr ? gameUI->TopInputReceiver() : nullptr, inventory.GetActiveSlot(), inventory.GetNextActiveSlot());
		}
		return false;
	}

	bool PrimaryReady()
	{
		const bool ready = PrimaryAvailable(false);
		Msg("* [editor-checks] primary readiness=%d loading-screen=%d", ready, load_screen_renderer.IsActive());
		return ready;
	}
}

void ImUtils::Viewer::CViewerSession::StartLifecycleTest(const char* args)
{
	string64 mode = {};
	string64 saveName = {};
	string64 editor = {};
	if (Lifecycle.Active || args == nullptr || sscanf(args, "%63s %63s %63s", mode, saveName, editor) < 2 ||
		(0 != _stricmp(mode, "viewer_first") && 0 != _stricmp(mode, "in_game")))
	{
		Msg("! viewer_lifecycle_test <viewer_first|in_game> <save_name> [sandbox|ogf|material|particle|ai]");
		return;
	}
	if (!CSavedGameWrapper::saved_game_exist(saveName) || !CSavedGameWrapper::valid_saved_game(saveName))
	{
		Msg("! [lifecycle] save '%s' is absent or invalid", saveName);
		return;
	}

	Lifecycle = {};
	Lifecycle.MenuWasActive = g_pGamePersistent->m_pMainMenu != nullptr && g_pGamePersistent->m_pMainMenu->IsActive();
	Lifecycle.Active = true;
	Lifecycle.ViewerFirst = 0 == _stricmp(mode, "viewer_first");
	Lifecycle.SingleEditor = editor[0] != 0;
	xr_strcpy(Lifecycle.SaveName, saveName);
	if (Lifecycle.SingleEditor)
	{
		int index = 0;
		while (index < int(std::size(ViewerLifecycleDetail::EditorNames)) && 0 != _stricmp(editor, ViewerLifecycleDetail::EditorNames[index]))
			++index;
		if (index == int(std::size(ViewerLifecycleDetail::EditorNames)))
		{
			Lifecycle = {};
			Msg("! [lifecycle] unknown editor '%s'", editor);
			return;
		}
		Lifecycle.EditorIndex = index;
	}
	Lifecycle.ExpectedEditor = ViewerLifecycleDetail::EditorKinds[Lifecycle.EditorIndex];
	if (Lifecycle.ViewerFirst)
	{
		string128 command;
		xr_sprintf(command, "viewer_editor %s on", ViewerLifecycleDetail::EditorNames[Lifecycle.EditorIndex]);
		Console->Execute(command);
		Lifecycle.EditorStarted = true;
	}
	else
	{
		SetLifecycleStage(SViewerLifecycleTestState::EStage::StartPrimary);
	}
	Msg("* [lifecycle] start mode=%s save=%s editor=%s", mode, saveName, Lifecycle.SingleEditor ? editor : "all");
}

void ImUtils::Viewer::CViewerSession::SetLifecycleStage(SViewerLifecycleTestState::EStage stage)
{
	Lifecycle.Stage = stage;
	Lifecycle.Ticks = 0;
	Lifecycle.CheckIndex = 0;
	Lifecycle.CheckWait = 0;
	Lifecycle.Ui.Active = false;
	Lifecycle.WaitObserved = false;
	Msg("* [lifecycle] stage %s", ViewerLifecycleDetail::StageNames[u32(stage)]);
}

void ImUtils::Viewer::CViewerSession::FinishLifecycleTest(bool passed, const char* reason)
{
	Msg("* [lifecycle] %s: editor=%s, %s", passed ? "PASS" : "FAIL", ViewerLifecycleDetail::EditorName(Lifecycle.ExpectedEditor), reason);
	Lifecycle.Active = false;
	Lifecycle.Ui.Active = false;
	if (Lifecycle.RestoreCapture)
	{
		GImGUIManager->SetCaptureInputs(false);
		Lifecycle.RestoreCapture = false;
	}
	Engine.External.EditorStates[static_cast<u8>(EditorUI::Tools_OMFEditor)] = false;
	Console->Execute("sandbox_stop");
	Console->Execute("keypress_on_start 1");
	if (ViewerLifecycleDetail::PrimaryReady() || !Lifecycle.MenuWasActive)
		Console->Execute("main_menu off");
	else
		Lifecycle.RestoreMenu = true;
	if (AutorunDone)
		Console->Execute("quit");
}

void ImUtils::Viewer::CViewerSession::TickLifecycleTest()
{
	if (!Lifecycle.Active)
		return;
	if (++Lifecycle.Ticks > 18000)
	{
		FinishLifecycleTest(false, "stage timeout");
		return;
	}

	switch (Lifecycle.Stage)
	{
	case SViewerLifecycleTestState::EStage::WaitViewer:
	{
		if (!IsActive() || ActivePreset().EditorKind != Lifecycle.ExpectedEditor)
			return;
		Msg("* [lifecycle] editor '%s' remains active", ViewerLifecycleDetail::EditorName(Lifecycle.ExpectedEditor));
		SetLifecycleStage(SViewerLifecycleTestState::EStage::EditorChecks);
		break;
	}
	case SViewerLifecycleTestState::EStage::StartPrimary:
	{
		if (ViewerLifecycleDetail::PrimaryCanContinue())
		{
			SetLifecycleStage(SViewerLifecycleTestState::EStage::SkipIntro);
			return;
		}
		if (!Lifecycle.StartCommandSent)
		{
			Lifecycle.StartCommandSent = true;
			Console->Execute("start server(all/single/alife/new)");
		}
		return;
	}
	case SViewerLifecycleTestState::EStage::SkipIntro:
	{
		if (!ViewerLifecycleDetail::PrimaryCanContinue())
			return;
		if (!ViewerLifecycleDetail::PrimaryReady())
		{
			Console->Execute("viewer_primary_continue");
			return;
		}
		if (g_pGameLevel->Flow.ActiveInput != nullptr)
		{
			Console->Execute("viewer_primary_continue");
			return;
		}
		if (Lifecycle.OtherEditorsPending)
			SetLifecycleStage(SViewerLifecycleTestState::EStage::EditorChecks);
		else
			SetLifecycleStage(Lifecycle.EditorStarted ? SViewerLifecycleTestState::EStage::SaveLoad : SViewerLifecycleTestState::EStage::NextEditor);
		break;
	}
	case SViewerLifecycleTestState::EStage::EditorChecks:
		TickEditorChecks();
		break;
	case SViewerLifecycleTestState::EStage::SaveLoad:
	{
		string128 command;
		xr_sprintf(command, "load %s", Lifecycle.SaveName);
		Console->Execute(command);
		SetLifecycleStage(SViewerLifecycleTestState::EStage::WaitSave);
		break;
	}
	case SViewerLifecycleTestState::EStage::WaitSave:
	{
		if (!ViewerLifecycleDetail::PrimaryReady())
		{
			Lifecycle.WaitObserved = true;
			if (ViewerLifecycleDetail::PrimaryCanContinue())
				Console->Execute("viewer_primary_continue");
			return;
		}
		if (!Lifecycle.WaitObserved)
			return;
		if (!IsActive() || ActivePreset().EditorKind != Lifecycle.ExpectedEditor ||
			(ActivePreset().EditorKind == EViewerEditorKind::LevelSandbox && !ViewerState.HasActor))
		{
			FinishLifecycleTest(false, "viewer session or actor was lost during save loading");
			return;
		}
		Msg("* [lifecycle] save loaded, primary level=%s, viewer=%s", g_pGameLevel->name().c_str(), ViewerLifecycleDetail::EditorName(Lifecycle.ExpectedEditor));
		if (Lifecycle.TransitionReload)
		{
			Lifecycle.TransitionReload = false;
			xr_strcpy(Lifecycle.ExpectedLevel, "pripyat");
			Console->Execute("jump_to_level pripyat");
			SetLifecycleStage(SViewerLifecycleTestState::EStage::ChangeLevel);
		}
		else if (Lifecycle.GameplayCompleted)
		{
			xr_strcpy(Lifecycle.ExpectedLevel, "jupiter");
			Console->Execute("jump_to_level jupiter");
			SetLifecycleStage(SViewerLifecycleTestState::EStage::ChangeLevel);
		}
		else
		{
			SetLifecycleStage(SViewerLifecycleTestState::EStage::PrimaryGameplay);
		}
		break;
	}
	case SViewerLifecycleTestState::EStage::PrimaryGameplay:
		TickPrimaryGameplayChecks();
		break;
	case SViewerLifecycleTestState::EStage::ChangeLevel:
	{
		if (!ViewerLifecycleDetail::PrimaryReady())
		{
			Lifecycle.WaitObserved = true;
			if (ViewerLifecycleDetail::PrimaryCanContinue())
				Console->Execute("viewer_primary_continue");
			return;
		}
		if (!Lifecycle.WaitObserved || 0 != _stricmp(g_pGameLevel->name().c_str(), Lifecycle.ExpectedLevel))
			return;
		if (!IsActive() || ActivePreset().EditorKind != Lifecycle.ExpectedEditor ||
			(ActivePreset().EditorKind == EViewerEditorKind::LevelSandbox && !ViewerState.HasActor))
		{
			FinishLifecycleTest(false, "viewer session or actor was lost during level transition");
			return;
		}
		Msg("* [lifecycle] primary level changed to %s, viewer remains active", Lifecycle.ExpectedLevel);
		if (0 == _stricmp(Lifecycle.ExpectedLevel, "jupiter"))
		{
			Console->Execute("save viewer_transition_jupiter");
			SetLifecycleStage(SViewerLifecycleTestState::EStage::SaveTransition);
			return;
		}
		SetLifecycleStage(SViewerLifecycleTestState::EStage::NextEditor);
		break;
	}
	case SViewerLifecycleTestState::EStage::SaveTransition:
		if (Lifecycle.Ticks > 60 && CSavedGameWrapper::saved_game_exist("viewer_transition_jupiter") && CSavedGameWrapper::valid_saved_game("viewer_transition_jupiter"))
		{
			Lifecycle.TransitionReload = true;
			Console->Execute("load viewer_transition_jupiter");
			SetLifecycleStage(SViewerLifecycleTestState::EStage::WaitSave);
		}
		break;
	case SViewerLifecycleTestState::EStage::NextEditor:
	{
		if (Lifecycle.EditorStarted && (Lifecycle.ViewerFirst || Lifecycle.SingleEditor || Lifecycle.EditorIndex >= int(std::size(ViewerLifecycleDetail::EditorNames))))
		{
			FinishLifecycleTest(true, "editor UI, save/load, Jupiter save/reload and Pripyat transition passed");
			return;
		}
		Lifecycle.ExpectedEditor = ViewerLifecycleDetail::EditorKinds[Lifecycle.EditorIndex];
		string128 command;
		xr_sprintf(command, "viewer_editor %s on", ViewerLifecycleDetail::EditorNames[Lifecycle.EditorIndex++]);
		Console->Execute(command);
		Lifecycle.EditorStarted = true;
		Lifecycle.ActorRequested = false;
		SetLifecycleStage(SViewerLifecycleTestState::EStage::WaitViewer);
		break;
	}
	}
}
