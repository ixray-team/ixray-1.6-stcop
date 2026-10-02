#include "StdAfx.h"
#include "../Level.h"
#include "../Actor.h"
#include "../alife_simulator.h"
#include "../alife_object_registry.h"

#include "../xrEngine/XR_IOConsole.h"
#include "../xrEngine/string_table.h"

#include "ai_space.h"

#include "ImUtils.h"

void RenderSearchManagerWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_SearchManager)])
		return;

	if (!g_pGameLevel)
		return;

	if (!ai().get_alife())
		return;

	if (g_pClsidManager == nullptr)
		return;

	if (imgui_search_manager.is_initialized == false)
		return;

	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.0f, 0.0f, 0.0f, kGeneralAlphaLevelForImGuiWindows));
	if (ImGui::Begin("Search Manager", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_SearchManager)]))
	{
		constexpr size_t kItemSize = sizeof(imgui_search_manager.combo_items) / sizeof(imgui_search_manager.combo_items[0]);
		ImGui::Combo("Category", &imgui_search_manager.selected_type, imgui_search_manager.combo_items, kItemSize);

		ImGui::SeparatorText("Stats");
		ImGui::Text("Current category: %s (%d)", imgui_search_manager.convertTypeToString(imgui_search_manager.selected_type), imgui_search_manager.selected_type);
		ImGui::Text("Level: %s", Level().name().c_str());

		ImGui::Text("All: %d", imgui_search_manager.counts[(eSelectedType::kSelectedType_All)]);
		ImGui::Text("%s: %d", imgui_search_manager.pTranslatedLabel_SmartCover, imgui_search_manager.counts[(eSelectedType::kSelectedType_SmartCover)]);
		ImGui::Text("%s: %d", imgui_search_manager.pTranslatedLabel_SmartTerrain, imgui_search_manager.counts[(eSelectedType::kSelectedType_SmartTerrain)]);
		ImGui::Text("%s: %d", imgui_search_manager.pTranslatedLabel_Stalker, imgui_search_manager.counts[(eSelectedType::kSelectedType_Stalker)]);
		ImGui::Text("%s: %d", imgui_search_manager.pTranslatedLabel_Car, imgui_search_manager.counts[(eSelectedType::kSelectedType_Car)]);
		ImGui::Text("%s: %d", imgui_search_manager.pTranslatedLabel_LevelChanger, imgui_search_manager.counts[(eSelectedType::kSelectedType_LevelChanger)]);
		ImGui::Text("%s: %d", imgui_search_manager.pTranslatedLabel_Artefact, imgui_search_manager.counts[(eSelectedType::kSelectedType_Artefact)]);

		string32 colh_monsters;
		xr_sprintf(colh_monsters, sizeof(colh_monsters), "Monsters: %d", imgui_search_manager.counts[eSelectedType::kSelectedType_Monster_All]);

		if (ImGui::CollapsingHeader(colh_monsters))
		{
			for (const auto& id : g_pClsidManager->get_monsters())
			{
				string32 monster_name;
				xr_sprintf(monster_name, sizeof(monster_name), "%s: %d", g_pClsidManager->translateCLSID(id), imgui_search_manager.counts[imgui_search_manager.convertCLSIDToType(id)]);
				ImGui::Text(monster_name);
			}
		}

		string32 colh_weapons;
		xr_sprintf(colh_weapons, sizeof(colh_weapons), "Weapons: %d", imgui_search_manager.counts[eSelectedType::kSelectedType_Weapon_All]);

		if (ImGui::CollapsingHeader(colh_weapons))
		{
			for (const auto& id : g_pClsidManager->get_weapons())
			{
				string32 weapon_name;
				xr_sprintf(weapon_name, sizeof(weapon_name), "%s: %d", g_pClsidManager->translateCLSID(id), imgui_search_manager.counts[imgui_search_manager.convertCLSIDToType(id)]);
				ImGui::Text(weapon_name);
			}
		}

		ImGui::SeparatorText("Settings");
		ImGui::Checkbox("Alive", &imgui_search_manager.show_alive_creatures);
		if (ImGui::BeginItemTooltip())
		{
			ImGui::Text("Shows alive or not alive creature(if it is not creature this flag doesn't affect)");
			ImGui::EndTooltip();
		}

		ImGui::SeparatorText("Simulation");

		auto teleport_to = [](const Fvector& position)
		{
			CActor* actor = Level().CurrentEntity() ? Level().CurrentEntity()->cast_actor() : nullptr;
			if (!actor)
				return;

			xr_string cmd = "set_actor_position ";
			cmd += cmd.ToString(position.x);
			cmd += ",";
			cmd += cmd.ToString(position.y);
			cmd += ",";
			cmd += cmd.ToString(position.z);
			execute_console_command_deferred(Console, cmd.c_str());
		};

		if (ImGui::BeginTabBar("##TB_InGameSearchManager"))
		{
			if (ImGui::BeginTabItem("Online##TB_Online_InGameSearchManager"))
			{
				ZeroMemory(imgui_search_manager.counts, sizeof(imgui_search_manager.counts));

				ImGui::InputText("##IT_InGameSeachManager", imgui_search_manager.search_string, sizeof(imgui_search_manager.search_string));

				string64 category_name_separator;
				xr_string pTranslatedCategoryName = imgui_search_manager.convertTypeToString(imgui_search_manager.selected_type);
				xr_strcpy(category_name_separator, pTranslatedCategoryName.c_str());
				ImGui::SeparatorText(category_name_separator);

				xr_vector<CObject*> filtered_objects;
				const auto size = Level().Objects.o_count();
				filtered_objects.reserve(size);
				const bool has_filter = imgui_search_manager.search_string[0] != '\0';

				for (u32 i = 0; i < size; ++i)
				{
					CObject* object = Level().Objects.o_get_by_iterator(i);
					if (!object || object->H_Parent() || !imgui_search_manager.valid(object->CLS_ID))
						continue;

					CGameObject* game_object = object->cast_game_object();
					if (game_object)
					{
						if (has_filter)
						{
							const xr_string_view name = object->cName().c_str();
							const xr_string translated_name = Platform::ANSI_TO_UTF8(g_pStringTable->translate(game_object->Name()).c_str());
							if (name.find(imgui_search_manager.search_string) == xr_string_view::npos &&
								translated_name.find(imgui_search_manager.search_string) == xr_string::npos)
								continue;
						}

						if (imgui_search_manager.show_alive_creatures)
						{
							CEntity* entity = game_object->cast_entity();
							if (!entity || !entity->g_Alive())
								continue;
						}
					}
					filtered_objects.push_back(object);
				}

				ImGuiListClipper clipper;
				clipper.Begin(static_cast<int>(filtered_objects.size()));
				while (clipper.Step())
				{
					for (int i = clipper.DisplayStart; i < clipper.DisplayEnd; ++i)
					{
						CObject* object = filtered_objects[i];
						CGameObject* game_object = object->cast_game_object();
						xr_string name = object->cName().c_str();
						if (game_object)
						{
							name += " [";
							name += Platform::ANSI_TO_UTF8(g_pStringTable->translate(game_object->Name()).c_str());
							name += "]";
						}
						name += "###object";

						ImGui::PushID(static_cast<int>(object->ID()));
						if (ImGui::Button(name.c_str()))
							teleport_to(object->Position());

						if (ImGui::BeginItemTooltip())
						{
							ImGui::Text("system name: [%s]", object->cName().c_str());
							ImGui::Text("section name: [%s]", object->cNameSect().c_str());
							if (game_object)
								ImGui::Text("translated name: [%s]", Platform::ANSI_TO_UTF8(g_pStringTable->translate(game_object->Name()).c_str()).c_str());
							ImGui::Text("position: %f %f %f", object->Position().x, object->Position().y, object->Position().z);
							ImGui::EndTooltip();
						}
						ImGui::PopID();
					}
				}

				ImGui::EndTabItem();
			}

			if (ImGui::BeginTabItem("Offline##TB_Offline_InGameSearchManager"))
			{
				memset(imgui_search_manager.counts, 0, sizeof(imgui_search_manager.counts));

				ImGui::InputText("##IT_InGameSearchManager", imgui_search_manager.search_string, sizeof(imgui_search_manager.search_string));

				string64 category_name_separator;
				xr_string pTranslatedCategoryName = imgui_search_manager.convertTypeToString(imgui_search_manager.selected_type);
				xr_strcpy(category_name_separator, pTranslatedCategoryName.c_str());
				ImGui::SeparatorText(category_name_separator);

				const auto& objects = ai().alife().objects().objects_vec();
				xr_vector<CSE_ALifeDynamicObject*> filtered_objects;
				filtered_objects.reserve(objects.size());
				const bool has_filter = imgui_search_manager.search_string[0] != '\0';

				for (CSE_ALifeDynamicObject* object : objects)
				{
					if (!object || object->ID_Parent != ALife::INVALID_OBJECT_ID ||
						!imgui_search_manager.valid(object->m_tClassID))
						continue;

					if (has_filter)
					{
						auto matches_filter = [](const char* name)
						{
							if (!name)
								return false;
							const xr_string translated_name = Platform::ANSI_TO_UTF8(g_pStringTable->translate(name).c_str());
							return xr_string_view(name).find(imgui_search_manager.search_string) != xr_string_view::npos ||
								translated_name.find(imgui_search_manager.search_string) != xr_string::npos;
						};
						if (!matches_filter(object->name_replace()) && !matches_filter(object->s_name.c_str()))
							continue;
					}
					filtered_objects.push_back(object);
				}

				ImGuiListClipper clipper;
				clipper.Begin(static_cast<int>(filtered_objects.size()));
				while (clipper.Step())
				{
					for (int i = clipper.DisplayStart; i < clipper.DisplayEnd; ++i)
					{
						CSE_ALifeDynamicObject* object = filtered_objects[i];
						xr_string name = object->name_replace() ? object->name_replace() : "";
						name += " [";
						name += Platform::ANSI_TO_UTF8(g_pStringTable->translate(object->s_name).c_str());
						name += "]###object";

						ImGui::PushID(static_cast<int>(object->ID));
						if (ImGui::Button(name.c_str()))
							teleport_to(object->Position());
						ImGui::PopID();
					}
				}

				ImGui::EndTabItem();
			}

			ImGui::EndTabBar();
		}

	}
	ImGui::End();
	ImGui::PopStyleColor(1);
}

clsid_manager imgui_clsid_manager;

void InitImGuiCLSIDInGame()
{
	imgui_clsid_manager.add_npc(imgui_clsid_manager.stalker);

	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_bloodsucker);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_boar);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_dog);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_flesh);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_pseudodog);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_burer);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_cat);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_chimera);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_controller);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_izlom);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_poltergeist);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_pseudogigant);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_zombie);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_snork);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_tushkano);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_psydog);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_psydogphantom);
	imgui_clsid_manager.add_monster(imgui_clsid_manager.monster_crow);

	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_binocular);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_knife);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_bm16);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_groza);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_svd);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_ak74);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_lr300);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_hpsa);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_pm);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_rg6);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_rpg7);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_shotgun);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_autoshotgun);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_svu);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_usp45);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_val);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_vintorez);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_walther);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_magazine);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.weapon_stationary_machine_gun);

	imgui_clsid_manager.add_item(imgui_clsid_manager.item_torch);
	imgui_clsid_manager.add_item(imgui_clsid_manager.item_d_pda);
	imgui_clsid_manager.add_item(imgui_clsid_manager.item_pda);
	imgui_clsid_manager.add_item(imgui_clsid_manager.item_medkit);
	imgui_clsid_manager.add_item(imgui_clsid_manager.item_bandage);
	imgui_clsid_manager.add_item(imgui_clsid_manager.item_antirad);
	imgui_clsid_manager.add_item(imgui_clsid_manager.item_bottle);
	imgui_clsid_manager.add_item(imgui_clsid_manager.item_ii_attch);

	imgui_clsid_manager.add_item(imgui_clsid_manager.item_ii_doc);
	imgui_clsid_manager.add_item(imgui_clsid_manager.item_ii_bttch);
	imgui_clsid_manager.add_item(imgui_clsid_manager.item_nw_attch);
	imgui_clsid_manager.add_item(imgui_clsid_manager.item_ii_bolt);

	// Items used
	imgui_clsid_manager.add_item_used(imgui_clsid_manager.item_food);
	imgui_clsid_manager.add_item_used(imgui_clsid_manager.item_ii_antir);
	imgui_clsid_manager.add_item_used(imgui_clsid_manager.item_ii_medki);
	imgui_clsid_manager.add_item_used(imgui_clsid_manager.item_ii_bandg);
	imgui_clsid_manager.add_item_used(imgui_clsid_manager.item_ii_food);
	imgui_clsid_manager.add_item_used(imgui_clsid_manager.item_ii_bottl);

	imgui_clsid_manager.add_ammo(imgui_clsid_manager.ammo_base);
	imgui_clsid_manager.add_ammo(imgui_clsid_manager.ammo_vog25);
	imgui_clsid_manager.add_ammo(imgui_clsid_manager.ammo_og7b);
	imgui_clsid_manager.add_ammo(imgui_clsid_manager.ammo_m209);
	imgui_clsid_manager.add_ammo(imgui_clsid_manager.ammo_f1);
	imgui_clsid_manager.add_ammo(imgui_clsid_manager.ammo_rgd5);

	imgui_clsid_manager.add_outfit(imgui_clsid_manager.outfit);
	imgui_clsid_manager.add_outfit(imgui_clsid_manager.helmet);

	imgui_clsid_manager.add_addon(imgui_clsid_manager.addon_scope);
	imgui_clsid_manager.add_addon(imgui_clsid_manager.addon_silen);
	imgui_clsid_manager.add_addon(imgui_clsid_manager.addon_glaun);

	imgui_clsid_manager.add_artefact(imgui_clsid_manager.artefact);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.artefact_s);

	imgui_clsid_manager.add_vehicle(imgui_clsid_manager.car);

	imgui_clsid_manager.add_outfit(imgui_clsid_manager.mp_helmet);
	imgui_clsid_manager.add_outfit(imgui_clsid_manager.mp_out_exo);
	imgui_clsid_manager.add_outfit(imgui_clsid_manager.mp_out_military);
	imgui_clsid_manager.add_outfit(imgui_clsid_manager.mp_out_scientific);
	imgui_clsid_manager.add_outfit(imgui_clsid_manager.mp_out_stalker);

	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_ak74);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_magazine_gl);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_binocular);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_bm16);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_fn2000);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_fort);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_groza);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_hpsa);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_knife);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_lr300);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_magazine);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_pm);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_rg6);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_rpg7);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_shotgun);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_svd);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_svu);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_usp45);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_val);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_vintorez);
	imgui_clsid_manager.add_weapon(imgui_clsid_manager.mp_weapon_walther);

	imgui_clsid_manager.add_ammo(imgui_clsid_manager.mp_ammo_base);
	imgui_clsid_manager.add_ammo(imgui_clsid_manager.mp_ammo_og7b);
	imgui_clsid_manager.add_ammo(imgui_clsid_manager.mp_ammo_m209);
	imgui_clsid_manager.add_ammo(imgui_clsid_manager.mp_ammo_vog25);
	imgui_clsid_manager.add_ammo(imgui_clsid_manager.mp_f1);
	imgui_clsid_manager.add_ammo(imgui_clsid_manager.mp_rgd5);
	//imgui_clsid_manager.add_mp_stuff(imgui_clsid_manager.mp_rpg7);

	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_mercury_ball);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_black_drops);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_needles);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_bast_artefact);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_gravi_black);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_dummy);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_zuda);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_thorn);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_faded_ball);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_electric_ball);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_rusty_hair);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_galantine);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_gravi);
	imgui_clsid_manager.add_artefact(imgui_clsid_manager.mp_art_cta);

	imgui_clsid_manager.add_addon(imgui_clsid_manager.mp_addon_scope);
	imgui_clsid_manager.add_addon(imgui_clsid_manager.mp_addon_silen);
	imgui_clsid_manager.add_addon(imgui_clsid_manager.mp_addon_glaun);

	imgui_clsid_manager.add_device(imgui_clsid_manager.item_detector_scientific);
	imgui_clsid_manager.add_device(imgui_clsid_manager.item_detector_elite);
	imgui_clsid_manager.add_device(imgui_clsid_manager.item_detector_advanced);
	imgui_clsid_manager.add_device(imgui_clsid_manager.item_detector_simple);
	imgui_clsid_manager.add_device(imgui_clsid_manager.item_d_elite);
	imgui_clsid_manager.add_device(imgui_clsid_manager.item_d_scientific);
	imgui_clsid_manager.add_device(imgui_clsid_manager.item_d_advanc);
	imgui_clsid_manager.add_device(imgui_clsid_manager.item_d_flare);
	imgui_clsid_manager.add_device(imgui_clsid_manager.item_d_simple);
	imgui_clsid_manager.add_device(imgui_clsid_manager.item_d_smetr);
	imgui_clsid_manager.add_device(imgui_clsid_manager.item_d_custom);

	imgui_clsid_manager.add_dynamic_object(imgui_clsid_manager.do_dstr_s);
	imgui_clsid_manager.add_dynamic_object(imgui_clsid_manager.o_physic_s);
	imgui_clsid_manager.add_dynamic_object(imgui_clsid_manager.do_object_item_std);
	imgui_clsid_manager.add_dynamic_object(imgui_clsid_manager.do_object_breakable);
	imgui_clsid_manager.add_dynamic_object(imgui_clsid_manager.do_object_climable);
	imgui_clsid_manager.add_dynamic_object(imgui_clsid_manager.do_object_holder_ent);
	imgui_clsid_manager.add_dynamic_object(imgui_clsid_manager.do_ph_skeleton_object);
	imgui_clsid_manager.add_dynamic_object(imgui_clsid_manager.do_object_physic);
	imgui_clsid_manager.add_dynamic_object(imgui_clsid_manager.do_physics_destr);
	imgui_clsid_manager.add_dynamic_object(imgui_clsid_manager.do_invbox);
	imgui_clsid_manager.add_dynamic_object(imgui_clsid_manager.s_invbox);

	// Explo
	imgui_clsid_manager.add_explo(imgui_clsid_manager.item_s_explo);
	imgui_clsid_manager.add_explo(imgui_clsid_manager.item_ii_explo);

	// Anomalies
	imgui_clsid_manager.add_anomaly(imgui_clsid_manager.zs_bfuzz);
	imgui_clsid_manager.add_anomaly(imgui_clsid_manager.zs_galan);
	imgui_clsid_manager.add_anomaly(imgui_clsid_manager.zs_mbald);
	imgui_clsid_manager.add_anomaly(imgui_clsid_manager.zs_mince);
	imgui_clsid_manager.add_anomaly(imgui_clsid_manager.zs_radio);
	imgui_clsid_manager.add_anomaly(imgui_clsid_manager.zs_torrd);
	imgui_clsid_manager.add_anomaly(imgui_clsid_manager.z_cfire);
	imgui_clsid_manager.add_anomaly(imgui_clsid_manager.z_mbald);
	imgui_clsid_manager.add_anomaly(imgui_clsid_manager.z_nograv);
	imgui_clsid_manager.add_anomaly(imgui_clsid_manager.z_radio);
	imgui_clsid_manager.add_anomaly(imgui_clsid_manager.z_teambs);

	imgui_clsid_manager.add_squad(imgui_clsid_manager.sim_squad_scripted);

	g_pClsidManager = &imgui_clsid_manager;
}


void InitImGuiSearchInGame()
{
	imgui_search_manager.init();
}

void InitImGuiHudAdjustInGame()
{
	// TODO: add message sound for saving

	imgui_hud_adjust_manager.is_initialized = true;
}
