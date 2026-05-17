#include "StdAfx.h"

#include "ImUtils.h"
#include "../xrEngine/xr_input.h"

clsid_manager* g_pClsidManager;
CImGuiGameSearchManager imgui_search_manager;
CHudAdjustManager imgui_hud_adjust_manager;

void RegisterImGuiInGame()
{
	if (!Device.IsEditorMode())
	{
		CImGuiManager::Instance().Subscribe("Time Manager", CImGuiManager::ERenderPriority::eMedium, RenderTimeManagerWindow);
		CImGuiManager::Instance().Subscribe("Spawn Manager", CImGuiManager::ERenderPriority::eMedium, RenderSpawnManagerWindow);
		CImGuiManager::Instance().Subscribe("Weapon Manager", CImGuiManager::ERenderPriority::eMedium, RenderWeaponManagerWindow);
		CImGuiManager::Instance().Subscribe("Search Manager", CImGuiManager::ERenderPriority::eMedium, RenderSearchManagerWindow);
		CImGuiManager::Instance().Subscribe("OMF Editor", CImGuiManager::ERenderPriority::eMedium, RenderToolsOMFEditorWindow);
		CImGuiManager::Instance().Subscribe("Car Editor", CImGuiManager::ERenderPriority::eMedium, RenderCarConfigEditor);
		CImGuiManager::Instance().Subscribe("Texture Editor", CImGuiManager::ERenderPriority::eMedium, RenderTextureEditor);
		CImGuiManager::Instance().Subscribe("Quest Editor", CImGuiManager::ERenderPriority::eMedium, RenderQuestEditor);
		CImGuiManager::Instance().Subscribe("Input Manager", CImGuiManager::ERenderPriority::eMedium, RenderToolsInputManagerWindow);
		CImGuiManager::Instance().Subscribe("SVGStorageViewer", CImGuiManager::ERenderPriority::eMedium, RenderToolsRenderDebugSVGStorageViewerWindow);
		CImGuiManager::Instance().Subscribe("Hud Adjust", CImGuiManager::ERenderPriority::eMedium, RenderHUDAdjustManager);

		InitImGuiCLSIDInGame();
		InitImGuiSearchInGame();
		InitImGuiHudAdjustInGame();
		InitSections();
		InitImGuiInGameInputReceiver();
	}
}

void DestroyImGuiInGame()
{
	if (Device.IsEditorMode())
	{
		return;
	}

	DestroySpawnManagerWindow();
}

eSelectedType CImGuiGameSearchManager::convertCLSIDToType(CLASS_ID id) {
	eSelectedType result = eSelectedType::kSelectedType_Count;

	if (class_to_type.find(id) != class_to_type.end())
		result = class_to_type.at(id);

	return result;
}

const char* CImGuiGameSearchManager::convertTypeToString(int type) {
	switch (static_cast<eSelectedType>(type))
	{
	case eSelectedType::kSelectedType_All:
	{
		return "All";
	}
	case eSelectedType::kSelectedType_Monster_All:
	{
		return "Monster - All";
	}
	case eSelectedType::kSelectedType_Weapon_All:
	{
		return "Weapon - All";
	}
	}

	return nullptr;
}

bool CImGuiGameSearchManager::valid(CLASS_ID id) {

	bool result{};

	if (selected_type == eSelectedType::kSelectedType_All)
	{
		result = true;
		return result;
	}

	if (selected_type == eSelectedType::kSelectedType_Monster_All)
	{
		if (g_pClsidManager && g_pClsidManager->is_monster(id))
		{
			result = true;
			return result;
		}
	}

	if (selected_type == eSelectedType::kSelectedType_Weapon_All)
	{
		if (g_pClsidManager && g_pClsidManager->is_weapon(id))
		{
			result = true;
			return result;
		}
	}

	if (class_to_type.find(id) != class_to_type.end())
	{
		result = selected_type == class_to_type.at(id);
	}

	return result;
}

void CImGuiGameSearchManager::count(CLASS_ID id) {
	counts[(eSelectedType::kSelectedType_All)] += 1;

	if (g_pClsidManager == nullptr)
	{
		return;
	}

	if (g_pClsidManager->is_monster(id))
	{
		counts[eSelectedType::kSelectedType_Monster_All] += 1;

		if (class_to_type.find(id) != class_to_type.end())
			counts[class_to_type.at(id)] += 1;

	}
	else if (g_pClsidManager->is_weapon(id))
	{
		counts[eSelectedType::kSelectedType_Weapon_All] += 1;

		if (class_to_type.find(id) != class_to_type.end())
			counts[class_to_type.at(id)] += 1;
	}
	else
	{
		if (class_to_type.find(id) != class_to_type.end())
		{
			counts[class_to_type.at(id)] += 1;
		}
	}
}

void CImGuiGameSearchManager::init()
{
	if (g_pClsidManager == nullptr)
		return;

	type_to_class[eSelectedType::kSelectedType_SmartTerrain] = g_pClsidManager->smart_terrain;
	type_to_class[eSelectedType::kSelectedType_SmartCover] = g_pClsidManager->smart_cover;
	type_to_class[eSelectedType::kSelectedType_LevelChanger] = g_pClsidManager->level_changer;
	type_to_class[eSelectedType::kSelectedType_Artefact] = g_pClsidManager->artefact;
	type_to_class[eSelectedType::kSelectedType_Stalker] = g_pClsidManager->stalker;
	type_to_class[eSelectedType::kSelectedType_Car] = g_pClsidManager->car;

	type_to_class[eSelectedType::kSelectedType_Monster_BloodSucker] = g_pClsidManager->monster_bloodsucker;
	type_to_class[eSelectedType::kSelectedType_Monster_Boar] = g_pClsidManager->monster_boar;
	type_to_class[eSelectedType::kSelectedType_Monster_Dog] = g_pClsidManager->monster_dog;
	type_to_class[eSelectedType::kSelectedType_Monster_Flesh] = g_pClsidManager->monster_flesh;
	type_to_class[eSelectedType::kSelectedType_Monster_PseudoDog] = g_pClsidManager->monster_pseudodog;
	type_to_class[eSelectedType::kSelectedType_Monster_Burer] = g_pClsidManager->monster_burer;
	type_to_class[eSelectedType::kSelectedType_Monster_Cat] = g_pClsidManager->monster_cat;
	type_to_class[eSelectedType::kSelectedType_Monster_Chimera] = g_pClsidManager->monster_chimera;
	type_to_class[eSelectedType::kSelectedType_Monster_Controller] = g_pClsidManager->monster_controller;
	type_to_class[eSelectedType::kSelectedType_Monster_Izlom] = g_pClsidManager->monster_izlom;
	type_to_class[eSelectedType::kSelectedType_Monster_Poltergeist] = g_pClsidManager->monster_poltergeist;
	type_to_class[eSelectedType::kSelectedType_Monster_PseudoGigant] = g_pClsidManager->monster_pseudogigant;
	type_to_class[eSelectedType::kSelectedType_Monster_Zombie] = g_pClsidManager->monster_zombie;
	type_to_class[eSelectedType::kSelectedType_Monster_Snork] = g_pClsidManager->monster_snork;
	type_to_class[eSelectedType::kSelectedType_Monster_Tushkano] = g_pClsidManager->monster_tushkano;
	type_to_class[eSelectedType::kSelectedType_Monster_PsyDog] = g_pClsidManager->monster_psydog;
	type_to_class[eSelectedType::kSelectedType_Monster_PsyDogPhantom] = g_pClsidManager->monster_psydogphantom;

	type_to_class[eSelectedType::kSelectedType_Weapon_Binocular] = g_pClsidManager->weapon_binocular;
	type_to_class[eSelectedType::kSelectedType_Weapon_Knife] = g_pClsidManager->weapon_knife;
	type_to_class[eSelectedType::kSelectedType_Weapon_BM16] = g_pClsidManager->weapon_bm16;
	type_to_class[eSelectedType::kSelectedType_Weapon_Groza] = g_pClsidManager->weapon_groza;
	type_to_class[eSelectedType::kSelectedType_Weapon_SVD] = g_pClsidManager->weapon_svd;
	type_to_class[eSelectedType::kSelectedType_Weapon_AK74] = g_pClsidManager->weapon_ak74;
	type_to_class[eSelectedType::kSelectedType_Weapon_LR300] = g_pClsidManager->weapon_lr300;
	type_to_class[eSelectedType::kSelectedType_Weapon_HPSA] = g_pClsidManager->weapon_hpsa;
	type_to_class[eSelectedType::kSelectedType_Weapon_PM] = g_pClsidManager->weapon_pm;
	type_to_class[eSelectedType::kSelectedType_Weapon_RG6] = g_pClsidManager->weapon_rg6;
	type_to_class[eSelectedType::kSelectedType_Weapon_RPG7] = g_pClsidManager->weapon_rpg7;
	type_to_class[eSelectedType::kSelectedType_Weapon_Shotgun] = g_pClsidManager->weapon_shotgun;
	type_to_class[eSelectedType::kSelectedType_Weapon_AutoShotgun] = g_pClsidManager->weapon_autoshotgun;
	type_to_class[eSelectedType::kSelectedType_Weapon_SVU] = g_pClsidManager->weapon_svu;
	type_to_class[eSelectedType::kSelectedType_Weapon_USP45] = g_pClsidManager->weapon_usp45;
	type_to_class[eSelectedType::kSelectedType_Weapon_VAL] = g_pClsidManager->weapon_val;
	type_to_class[eSelectedType::kSelectedType_Weapon_VINTOREZ] = g_pClsidManager->weapon_vintorez;
	type_to_class[eSelectedType::kSelectedType_Weapon_WALTHER] = g_pClsidManager->weapon_walther;
	type_to_class[eSelectedType::kSelectedType_Weapon_Magazine] = g_pClsidManager->weapon_magazine;
	type_to_class[eSelectedType::kSelectedType_Weapon_StationaryMachineGun] = g_pClsidManager->weapon_stationary_machine_gun;

	for (const xr_pair<eSelectedType, CLASS_ID>& pair : type_to_class)
	{
		class_to_type[pair.second] = pair.first;
	}

	for (int i = 0; i < (eSelectedType::kSelectedType_Count); ++i)
	{
		char* pPtr = &category_names[i][0];
		const char* pStr = convertTypeToString(i);
		string32 result{};

		if (pStr == nullptr && type_to_class.find(eSelectedType(i)) != type_to_class.end())
		{
			string16 name{};
			CLASS_ID id = type_to_class.at(eSelectedType(i));
			CLSID2TEXT(id, name);

			for (int i = 0; i < 16; ++i)
			{
				if (name[i] == 32)
				{
					name[i] = '\0';
				}
			}
			const char* pTranslatedName = g_pStringTable ? g_pStringTable->translate(name).c_str() : name;

			if (g_pClsidManager && g_pClsidManager->is_monster(id))
			{
			    memcpy(result, "Monster - ", sizeof("Monster - "));
			    memcpy(&result[0] + sizeof("Monster - ") - 1, pTranslatedName, strlen(pTranslatedName) + 1);
			}
			else if (g_pClsidManager && g_pClsidManager->is_weapon(id))
			{
			    memcpy(result, "Weapon - ", sizeof("Weapon - "));
			    memcpy(&result[0] + sizeof("Weapon - ") - 1, pTranslatedName, strlen(pTranslatedName) + 1);
			}
			else
			{
			    memcpy(result, pTranslatedName, strlen(pTranslatedName) + 1);
			}

			pStr = result;
		}
		else
		{
			// unable to obtain the pointer of string, so we just mark it as warning to developers
			if (pStr == nullptr)
				pStr = "FAILED_TO_TRANSLATE";
		}

		memcpy(pPtr, pStr, strlen(pStr) + 1);

		combo_items[i] = pPtr;
	}

	initTranslatedLabels();


	is_initialized = true;
}

// pre-caching naming for fast accessing and reducing requests to StringTable manager, it is slow...

void CImGuiGameSearchManager::initTranslatedLabels()
{
	if (g_pClsidManager == nullptr)
		return;

	// if we unable to get info from StringTable manager we get a persistent pointer from .text section of dll so it is just string defines on "stack", see getDefaultNameOfSelectedType

	pTranslatedLabel_Artefact = getTranslatedString(eSelectedType::kSelectedType_Artefact);
	pTranslatedLabel_Car = getTranslatedString(eSelectedType::kSelectedType_Car);
	pTranslatedLabel_LevelChanger = getTranslatedString(eSelectedType::kSelectedType_LevelChanger);
	pTranslatedLabel_SmartCover = getTranslatedString(eSelectedType::kSelectedType_SmartCover);
	pTranslatedLabel_SmartTerrain = getTranslatedString(eSelectedType::kSelectedType_SmartTerrain);
	pTranslatedLabel_Stalker = getTranslatedString(eSelectedType::kSelectedType_Stalker);
}

const char* CImGuiGameSearchManager::getDefaultNameOfSelectedType(eSelectedType type)
{
	switch (type)
	{
	case eSelectedType::kSelectedType_SmartCover:
		return "default_Smart Cover";
	case eSelectedType::kSelectedType_SmartTerrain:
		return "default_Smart Terrain";
	case eSelectedType::kSelectedType_Stalker:
		return "default_Stalker";
	case eSelectedType::kSelectedType_Car:
		return "default_Car";
	case eSelectedType::kSelectedType_LevelChanger:
		return "default_LevelChanger";
	case eSelectedType::kSelectedType_Artefact:
		return "default_Artefact";
	default:
		return "DEFAULT_NAME_FAILED_TO_TRANSLATE";
	}
}
const char* CImGuiGameSearchManager::getTranslatedString(eSelectedType type)
{
	string16 name{};
	CLASS_ID id = type_to_class.at(type);
	CLSID2TEXT(id, name);

	for (int i = 0; i < 16; ++i)
	{
		if (name[i] == 32)
		{
			name[i] = '\0';
		}
	}

	const char* pResult = nullptr;

	pResult = g_pStringTable ? g_pStringTable->translate(name).c_str() : getDefaultNameOfSelectedType(type);

	return pResult;
}

void clsid_manager::add_mp_stuff(CLASS_ID id) {
	mp_stuffs.insert(id);
}

bool clsid_manager::is_mp_stuff(CLASS_ID id) {
	return mp_stuffs.contains(id);
}

void clsid_manager::add_item(CLASS_ID id) {
	items.insert(id);
}

bool clsid_manager::is_item(CLASS_ID id) {
	return items.contains(id);
}

void clsid_manager::add_item_used(CLASS_ID id) {
	items_used.insert(id);
}

bool clsid_manager::is_item_used(CLASS_ID id) {
	return items_used.contains(id);
}

void clsid_manager::add_device(CLASS_ID id) {
	devices.insert(id);
}

bool clsid_manager::is_device(CLASS_ID id) {
	return devices.contains(id);
}

void clsid_manager::add_dynamic_object(CLASS_ID id) {
	dynamic_objects.insert(id);
}

bool clsid_manager::is_dynamic_object(CLASS_ID id) {
	return dynamic_objects.contains(id);
}

void clsid_manager::add_outfit(CLASS_ID id) {
	outfits.insert(id);
}

bool clsid_manager::is_outfit(CLASS_ID id) {
	return outfits.contains(id);
}
void clsid_manager::add_ammo(CLASS_ID id) {
	ammo.insert(id);
}

bool clsid_manager::is_ammo(CLASS_ID id) {
	return ammo.contains(id);
}
void clsid_manager::add_weapon(CLASS_ID id) {
	weapons.insert(id);
}

bool clsid_manager::is_weapon(CLASS_ID id) {
	return weapons.contains(id);
}
void clsid_manager::add_monster(CLASS_ID id) {
	monsters.insert(id);
}

bool clsid_manager::is_monster(CLASS_ID id) {
	return monsters.contains(id);
}
void clsid_manager::add_addon(CLASS_ID id) {
	addons.insert(id);
}

bool clsid_manager::is_addon(CLASS_ID id) {
	return addons.contains(id);
}
void clsid_manager::add_artefact(CLASS_ID id) {
	artefacts.insert(id);
}

bool clsid_manager::is_artefact(CLASS_ID id) {
	return artefacts.contains(id);
}

void clsid_manager::add_vehicle(CLASS_ID id) {
	vehicles.insert(id);
}

bool clsid_manager::is_vehicle(CLASS_ID id) {
	return vehicles.contains(id);
}

void clsid_manager::add_explo(CLASS_ID id) {
	explosives.insert(id);
}

bool clsid_manager::is_explo(CLASS_ID id) {
	return explosives.contains(id);
}

void clsid_manager::add_npc(CLASS_ID id) {
	npc_list.insert(id);
}

bool clsid_manager::is_npc(CLASS_ID id) {
	return npc_list.contains(id);
}

bool clsid_manager::is_anomaly(CLASS_ID id) {
	return anomalies.contains(id);
}
void clsid_manager::add_anomaly(CLASS_ID id) {
	anomalies.insert(id);
}

bool clsid_manager::is_squad(CLASS_ID id) {
	return squads.contains(id);
}
void clsid_manager::add_squad(CLASS_ID id) {
	squads.insert(id);
}

const char* clsid_manager::translateCLSID(CLASS_ID id) {
	string16 name{};
	CLSID2TEXT(id, name);

	for (int i = 0; i < 16; ++i)
	{
		if (name[i] == 32)
		{
			name[i] = '\0';
		}
	}
	return g_pStringTable ? g_pStringTable->translate(name).c_str() : name;
}

void InitImGuiInGameInputReceiver()
{
	R_ASSERT(pInput);
	R_ASSERT(pInput->xrgame_sdk_input_pressed == nullptr);
	R_ASSERT(pInput->xrgame_sdk_input_released == nullptr);

	if (pInput)
	{
		if (pInput->xrgame_sdk_input_pressed == nullptr)
		{
			pInput->xrgame_sdk_input_pressed = AllEditors_OnPressed;
		}

		if (pInput->xrgame_sdk_input_released == nullptr)
		{
			pInput->xrgame_sdk_input_released = AllEditors_OnReleased;
		}
	}

}

void AllEditors_SendRequest(const SRequestData& req)
{
	g_imgui_editor_request_manager.requests.run([req]()
		{
			AllEditors_ExecuteRequest(req);
		});
}

void AllEditors_ExecuteRequest(const SRequestData& req)
{
	PROF_START_THREAD("AllEditors_SendRequest");
	eImGuiEditorType et = static_cast<eImGuiEditorType>(req.editor_type);

	switch (et)
	{
	case eImGuiEditorType::kTextureEditor:
	{
		RequestHandler_TextureEditor(req);
		break;
	}
	case eImGuiEditorType::kOMFEditor:
	{
		RequestHandler_OMFEditor(req);
		break;
	}
	case eImGuiEditorType::kQuestEditor:
	{
		RequestHandler_QuestEditor(req);
		break;
	}
	case eImGuiEditorType::kNoEditor:break;
	default:
	{
		R_ASSERT2(false, "you forgot to register new workload!");
		break;
	}
	}
	PROF_STOP_THREAD();
}

void AllEditors_OnPressed(int key)
{
	if (!CImGuiManager::Instance().IsCapturingInputs() || pInput->xrgame_sdk_input_pressed == nullptr)
		return;

	if (Engine.External.EditorStates[u8(EditorUI::Tools_TextureEditor)])
	{
		TextureEditor_OnPressed(key);
	}

	if (Engine.External.EditorStates[u8(EditorUI::Game_SpawnManager)])
	{
		SpawnManager_OnPressed(key);
	}

	if (Engine.External.EditorStates[u8(EditorUI::Tools_QuestEditor)])
	{
		QuestEditor_OnPressed(key);
	}

	if (Engine.External.EditorStates[u8(EditorUI::Tools_OMFEditor)])
	{
		OMFEditor_OnPressed(key);
	}
}

void AllEditors_OnReleased(int key)
{
	if (!CImGuiManager::Instance().IsCapturingInputs() || pInput->xrgame_sdk_input_released == nullptr)
		return;

	if (Engine.External.EditorStates[u8(EditorUI::Tools_TextureEditor)])
	{
		TextureEditor_OnReleased(key);
	}

	if (Engine.External.EditorStates[u8(EditorUI::Game_SpawnManager)])
	{
		SpawnManager_OnReleased(key);
	}

	if (Engine.External.EditorStates[u8(EditorUI::Tools_QuestEditor)])
	{
		QuestEditor_OnReleased(key);
	}

	if (Engine.External.EditorStates[u8(EditorUI::Tools_OMFEditor)])
	{
		OMFEditor_OnReleased(key);
	}
}

#include "imgui_internal.h"
void CImGuiTextureEditor::SImGuiWindowState::Capture(const char* windowName)
{
	ImGuiWindow* w = ImGui::FindWindowByName(windowName);
	if (!w) return;

	if (canApply)
		return;

	canApply = true;

	pos = w->Pos;
	size = w->Size;

	ImGuiDockNode* node = w->DockNode;
	wasDocked = (node != nullptr);
	isCentralNode = false;
	dockDir = ImGuiDir_None;

	if (!node) return;

	/* If the node has no parent it is the root (central) node */
	ImGuiDockNode* parent = node->ParentNode;
	if (!parent)
	{
		isCentralNode = true;
		return;
	}

	/* Otherwise we are one of the two children of a split node.
	   SplitAxis tells us which axis the parent was split on. */
	ImGuiAxis axis = parent->SplitAxis;   // ImGuiAxis_X  -> horizontal split
	// ImGuiAxis_Y  -> vertical split

/* ChildNodes[0] is always the “leading”  child (left / top)
   ChildNodes[1] is always the “trailing” child (right / bottom) */
	if (parent->ChildNodes[0] == node)
		dockDir = (axis == ImGuiAxis_X) ? ImGuiDir_Left : ImGuiDir_Up;
	else
		dockDir = (axis == ImGuiAxis_X) ? ImGuiDir_Right : ImGuiDir_Down;
}

void CImGuiTextureEditor::SImGuiWindowState::Apply(const char* windowName)
{
	if (canApply == false)
		return;

	canApply = false;

	if (pos.x != FLT_MAX)  // we have a saved floating position
	{
		ImGui::SetNextWindowPos(pos, ImGuiCond_FirstUseEver);
		ImGui::SetNextWindowSize(size, ImGuiCond_FirstUseEver);
	}

	if (wasDocked)
	{
		// Build a persistent dock-space if you do not have one yet:
		ImGuiID dockspaceId = ImGui::GetID("MyDockSpace");
		if (!ImGui::DockBuilderGetNode(dockspaceId))
		{
			ImGui::DockBuilderAddNode(dockspaceId, ImGuiDockNodeFlags_DockSpace);
			ImGui::DockBuilderSetNodeSize(dockspaceId, ImGui::GetMainViewport()->Size);
			ImGui::DockBuilderFinish(dockspaceId);
		}

		// Re-dock the window into the same node (left/right/top/bottom/central)
		ImGuiID targetId = dockspaceId;

		if (!isCentralNode && dockDir != ImGuiDir_None)
		{
			// split the root once in the remembered direction
			ImGuiID central = 0;
			ImGuiID side = 0;
			ImGui::DockBuilderSplitNode(dockspaceId, dockDir, 0.35f, &side, &central);
			targetId = side;
		}

		ImGui::SetNextWindowDockID(targetId, ImGuiCond_FirstUseEver);
	}

	ImGuiWindow* w = ImGui::FindWindowByName(windowName);

	if (w)
	{
		ImGui::FocusWindow(w);
	}
}

void AllEditors_Shutdown()
{
	// texture editor
	{
		AllEditors_SendRequests_Sequential(xr_array<SRequestData, 2>{
			SRequestData{
				.editor_type = u32(eImGuiEditorType::kTextureEditor),
				.request_type = u32(eRequestType_TextureEditor::kWriteSettings)
			},
			SRequestData{
				.editor_type = u32(eImGuiEditorType::kTextureEditor),
				.request_type = u32(eRequestType_TextureEditor::kShutdown)
			}
		});
	}

	// quest editor
	{
		AllEditors_SendRequests_Sequential(xr_array<SRequestData, 2>{
			SRequestData{
				.editor_type = u32(eImGuiEditorType::kQuestEditor),
				.request_type = u32(eRequestType_QuestEditor::kWriteSettings)
			},
			SRequestData{
				.editor_type = u32(eImGuiEditorType::kQuestEditor),
				.request_type = u32(eRequestType_QuestEditor::kShutdown)
			}
			});
	}

	// omf editor
	{
		AllEditors_SendRequests_Sequential(xr_array<SRequestData, 2>{
			SRequestData{
				.editor_type = u32(eImGuiEditorType::kOMFEditor),
				.request_type = u32(eRequestType_OMFEditor::kWriteSettings)
			},
			SRequestData{
				.editor_type = u32(eImGuiEditorType::kOMFEditor),
				.request_type = u32(eRequestType_OMFEditor::kShutdown)
			}
		});
	}
}
