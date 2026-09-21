#include "StdAfx.h"
#include "../Level.h"
#include "../Actor.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../player_hud.h"
#include "ai_space.h"
#include "../../xrUI/ui_base.h"
#include "ImUtils.h"
#include "../game_news.h"
#include "../HudTorchLight.h"
#include "../Weapon.h"

extern bool hud_adj_crosshair;
extern bool forceFPDraw;
extern bool forceFP2Draw;
extern bool forceSPDraw;
extern bool b_toggle_weapon_aim;
extern float _delta_pos;
extern float _delta_rot;
extern bool forceLPDraw;
extern bool forceTPDraw;
extern bool forceCBDraw;

static void HudAdjustDrawSaveButton()
{
	//ImGui::PushItemWidth(-1);
	//ImGui::SameLine(ImGui::CalcItemWidth() - ImGui::CalcTextSize("Save").x);

	//ImGui::SameLine(ImGui::CalcItemWidth() - ImGui::CalcTextSize("?").x);
	//ImGui::Button("?");
	//
	//if (ImGui::IsItemHovered())
	//{
	//	ImGui::SetTooltip("* Shift + drag\nFor slower value change\n* Ctrl + click(or double click)\nInput text into slider\n* Alt + drag\nFor quick value change, opposite of Shift key");
	//}

	if (!ImGui::Button("Save"))
	{
		return;
	}

	string_path fn = {};
	attachable_hud_item* p_hud_item_first = g_player_hud->attached_item(0);
	attachable_hud_item* p_hud_item_second = g_player_hud->attached_item(1);
	
	FS.update_path(fn, "$app_data_root$", "hud_adjust\\saved.ltx");
	CInifile file(fn, false, true, true);
	file.set_override_names(true);
	
	auto writeParams = [](attachable_hud_item* p_item, CInifile& file) -> void
	{
		string64 sect = {};
		xr_sprintf(sect, sizeof(sect), p_item->m_sect_name.c_str());
	
		string64 _prefix = {};
		xr_sprintf(_prefix, "%s", UI().is_widescreen() ? "_16x9" : "");
		string128 val_name = {};

		if (!p_item->m_model_combined)
		{
			xr_strconcat(val_name, "hands_position", _prefix);
			file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_attach_real[0]);
			xr_strconcat(val_name, "hands_orientation", _prefix);
			file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_attach_real[1]);

			file.w_fvector3(sect, "item_position", p_item->m_measures.m_item_attach[0]);
			file.w_fvector3(sect, "item_orientation", p_item->m_measures.m_item_attach[1]);
		}
		else
		{
			file.w_fvector3(sect, "position", p_item->m_measures.m_item_attach[0]);
			file.w_fvector3(sect, "orientation", p_item->m_measures.m_item_attach[1]);
		}

		if (p_item->m_measures.m_prop_flags.test(p_item->m_measures.e_shell_point))
		{
			file.w_fvector3(sect, "shell_point", p_item->m_measures.m_shell_point_offset);
		}
	
		if (p_item->m_measures.m_prop_flags.test(p_item->m_measures.e_fire_point))
		{
			file.w_fvector3(sect, "fire_point", p_item->m_measures.m_fire_point_offset);
		}
	
		if (p_item->m_measures.m_prop_flags.test(p_item->m_measures.e_fire_point2))
		{
			file.w_fvector3(sect, "fire_point2", p_item->m_measures.m_fire_point2_offset);
		}

		if (!p_item->m_model_combined)
		{
			xr_strconcat(val_name, "aim_hud_offset_pos", _prefix);
			file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[0][EHudOffsetType::eAim]);
			xr_strconcat(val_name, "aim_hud_offset_rot", _prefix);
			file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[1][EHudOffsetType::eAim]);

			xr_strconcat(val_name, "gl_hud_offset_pos", _prefix);
			file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[0][EHudOffsetType::eAimGL]);
			xr_strconcat(val_name, "gl_hud_offset_rot", _prefix);
			file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[1][EHudOffsetType::eAimGL]);
		}
		else
		{
			file.w_fvector3(sect, "zoom_offset", p_item->m_measures.m_hands_positions.hands_offsets[0][EHudOffsetType::eAim]);
			file.w_float(sect, "zoom_rotate_x", p_item->m_measures.m_hands_positions.hands_offsets[1][EHudOffsetType::eAim].x);
			file.w_float(sect, "zoom_rotate_y", p_item->m_measures.m_hands_positions.hands_offsets[1][EHudOffsetType::eAim].y);
			file.w_float(sect, "zoom_rotate_z", p_item->m_measures.m_hands_positions.hands_offsets[1][EHudOffsetType::eAim].z);

			file.w_fvector3(sect, "grenade_zoom_offset", p_item->m_measures.m_hands_positions.hands_offsets[0][EHudOffsetType::eAimGL]);
			file.w_float(sect, "grenade_zoom_rotate_x", p_item->m_measures.m_hands_positions.hands_offsets[1][EHudOffsetType::eAimGL].x);
			file.w_float(sect, "grenade_zoom_rotate_y", p_item->m_measures.m_hands_positions.hands_offsets[1][EHudOffsetType::eAimGL].y);
			file.w_float(sect, "grenade_zoom_rotate_z", p_item->m_measures.m_hands_positions.hands_offsets[1][EHudOffsetType::eAimGL].z);
		}

		xr_strconcat(val_name, "alter_aim_hud_offset_pos", _prefix);
		file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[0][EHudOffsetType::eAimAlt]);
		xr_strconcat(val_name, "alter_aim_hud_offset_rot", _prefix);
		file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[1][EHudOffsetType::eAimAlt]);
	
		xr_strconcat(val_name, "safemode_hud_offset_pos", _prefix);
		file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[0][EHudOffsetType::eSafemode]);
		xr_strconcat(val_name, "safemode_hud_offset_rot", _prefix);
		file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[1][EHudOffsetType::eSafemode]);
	
		xr_strconcat(val_name, "collision_hud_offset_pos", _prefix);
		file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[0][EHudOffsetType::eCollision]);
		xr_strconcat(val_name, "collision_hud_offset_rot", _prefix);
		file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[1][EHudOffsetType::eCollision]);

		file.w_fvector3(sect, "collision_box_pos", p_item->m_measures.m_collision_params.obb_pos);
		file.w_fvector3(sect, "collision_box_scale", p_item->m_measures.m_collision_params.obb_scale);
		
		file.w_float(sect, "collision_box_stifness", p_item->m_measures.m_collision_params.stifness);
		file.w_float(sect, "collision_box_damping", p_item->m_measures.m_collision_params.damping);
	};

	FS.update_path(fn, "$app_data_root$", "hud_adjust\\saved_attachments.ltx");
	CInifile file_att(fn, false, true, true);
	file_att.set_override_names(true);

	auto writeAttParams = [](attachable_hud_item* p_item, CInifile& file)
	{
		for (auto& pair : p_item->m_parent_hud_item->item().m_attachments)
		{
			xr_string addon_modifers_sect = xr_string(p_item->m_parent_hud_item->object().cNameSect_str()) + '_' + xr_string(*pair.first);

			file.w_u8(addon_modifers_sect.c_str(), "attachment_type", pair.second.attachment_type);
			if (pair.second.state.test(eAStatePermanent))
			{
				file.w_bool(addon_modifers_sect.c_str(), "attachment_permanent", true);
			}
			file.w_fvector3(addon_modifers_sect.c_str(), "attachment_hud_position", pair.second.hud_place.position);
			file.w_fvector3(addon_modifers_sect.c_str(), "attachment_hud_direction", pair.second.hud_place.direction);
			file.w_fvector3(addon_modifers_sect.c_str(), "attachment_hud_scale", pair.second.hud_place.scale);
			file.w_string(addon_modifers_sect.c_str(), "attachment_hud_bone_name", p_item->m_model->LL_BoneName_dbg(pair.second.hud_place.parent_bone_id));
			if (pair.second.hud_place.m_model)
			{
				file.w_string(addon_modifers_sect.c_str(), "attachment_hud_visual", xr_string(xr_string(pair.second.hud_place.m_model->getDebugName().c_str()) + xr_string(".ogf")).c_str());
			}

			file.w_fvector3(addon_modifers_sect.c_str(), "attachment_position", pair.second.place.position);
			file.w_fvector3(addon_modifers_sect.c_str(), "attachment_direction", pair.second.place.direction);
			file.w_fvector3(addon_modifers_sect.c_str(), "attachment_scale", pair.second.place.scale);
			file.w_string(addon_modifers_sect.c_str(), "attachment_bone_name", PKinematics(p_item->m_parent_hud_item->object().Visual())->LL_BoneName_dbg(pair.second.place.parent_bone_id));
			if (pair.second.place.m_model)
			{
				file.w_string(addon_modifers_sect.c_str(), "attachment_visual", xr_string(xr_string(pair.second.place.m_model->getDebugName().c_str()) + xr_string(".ogf")).c_str());
			}
		}
	};
	
	if (p_hud_item_first)
	{
		writeParams(p_hud_item_first, file);
		writeAttParams(p_hud_item_first, file);
	}

	if (p_hud_item_second)
	{
		writeParams(p_hud_item_second, file);
		writeAttParams(p_hud_item_second, file);
	}

	/*{
		const char* section = g_player_hud->section_name().c_str();

		file.w_float(section, "watches_scale", g_player_hud->m_watches_scale);
		file.w_fvector3(section, "watches_pos", g_player_hud->m_watches_pos);
		file.w_fvector3(section, "watches_rot", g_player_hud->m_watches_rot);
	}*/

	GAME_NEWS_DATA news_data;
	news_data.m_type = GAME_NEWS_DATA::ENewsKind::eNews;
	news_data.news_caption = "Saved result to:";
	news_data.news_text = fn;
	news_data.show_time = 5000;
	news_data.texture_name = "ui_iconsTotal_bar_darklab_documents2";
	Actor()->AddGameNews(news_data);
}

static void HudAdjustDrawAdjustSettings()
{
	ImGui::Checkbox("Show crosshair", &hud_adj_crosshair);
	ImGui::Checkbox("Toggle weapon aim", &b_toggle_weapon_aim);
	ImGui::Checkbox("Show fire point box", &forceFPDraw);
	ImGui::Checkbox("Show fire point 2 box", &forceFP2Draw);
	ImGui::Checkbox("Show shell point box", &forceSPDraw);
	ImGui::Checkbox("Show laser point box", &forceLPDraw);
	ImGui::Checkbox("Show torch point box", &forceTPDraw);
	ImGui::Checkbox("Show collision box", &forceCBDraw);
	ImGui::SetNextItemWidth(80.0f);
	ImGui::InputFloat("Position step", &_delta_pos, 0.0f, 0.0f, "%.6f");
	ImGui::SetNextItemWidth(80.0f);
	ImGui::InputFloat("Rotation step", &_delta_rot, 0.0f, 0.0f, "%.6f");
}
/*
static void HudAdjustDrawHandsSettings()
{
	if (g_player_hud->attached_item(0) == nullptr && g_player_hud->attached_item(1) == nullptr)
	{
		return;
	}

	if (!ImGui::CollapsingHeader(*shared_str().printf("Hands: %s", g_player_hud->section_name().c_str())))
	{
		return;
	}

	if (g_player_hud->m_model_watches != nullptr)
	{
		if (ImGui::CollapsingHeader("Watches Settings"))
		{
			ImGui::InputFloat("Watches Scale", &g_player_hud->m_watches_scale, 0.0f, 0.0f, "%.6f");

			ImGui::SeparatorText("Position##WS");

			if (ImGui::Button("Reset##WP"))
			{
				g_player_hud->m_watches_pos = READ_IF_EXISTS(pSettings, r_fvector3, g_player_hud->section_name(), "watches_pos", zero_vel);
			}

			if (ImGui::BeginTable("Data##WP", 1))
			{
				ImGui::TableNextRow();

				ImGui::TableNextColumn();

				ImGui::DragFloat("X##WP", &g_player_hud->m_watches_pos.x, _delta_pos, -100.0f, 100.0f, "%.6f");
				ImGui::DragFloat("Y##WP", &g_player_hud->m_watches_pos.y, _delta_pos, -100.0f, 100.0f, "%.6f");
				ImGui::DragFloat("Z##WP", &g_player_hud->m_watches_pos.z, _delta_pos, -100.0f, 100.0f, "%.6f");

				ImGui::EndTable();
			}

			ImGui::SeparatorText("Rotation##WS");

			if (ImGui::Button("Reset##WR"))
			{
				g_player_hud->m_watches_rot = READ_IF_EXISTS(pSettings, r_fvector3, g_player_hud->section_name(), "watches_rot", zero_vel);
			}

			if (ImGui::BeginTable("Data##WR", 1))
			{
				ImGui::TableNextRow();

				ImGui::TableNextColumn();

				ImGui::DragFloat("X##WR", &g_player_hud->m_watches_rot.x, _delta_pos, -360.0f, 360.0f, "%.6f");
				ImGui::DragFloat("Y##WR", &g_player_hud->m_watches_rot.y, _delta_pos, -360.0f, 360.0f, "%.6f");
				ImGui::DragFloat("Z##WR", &g_player_hud->m_watches_rot.z, _delta_pos, -360.0f, 360.0f, "%.6f");

				ImGui::EndTable();
			}
		}
	}
}
*/

void AdjustDrawItemAttachmentsSettings(CInventoryItem* item, IKinematics* pK, bool hud_mode)
{
	if (ImGui::CollapsingHeader("Attachments"))
	{
		u8 mode = hud_mode ? 0 : 1;

		static FS_Path* pMeshesFolder = FS.get_path(_game_meshes_);
		static FS_FileSet files[2];
		FS.file_list(files[mode], _game_meshes_, FS_ListFiles, "*.ogf");

		static shared_str pending_model_sect[2];
		static bool show_model_popup[2] = {false, false};
		static char model_search_buf[2][64] = {"", ""};

		static xr_vector<shared_str> to_delete[2];
		static xr_vector<const char*> bones_names[2];
		bones_names[mode].resize(pK->LL_BoneCount());
		for (auto& pair : *pK->LL_Bones())
		{
			bones_names[mode][pair.second] = *pair.first;
		}

		static bool show_add_attach_window[2] = {false, false};
		static char attach_search_buf[2][64] = {"", ""};
		static EattachmentType pending_attachment_type[2] = {eTypeCustom, eTypeCustom};
		static bool select_type_step[2] = {true, true};

		static const std::pair<const char*, EattachmentType> kTypes[] = {
			{"None", eTypeNone},
			{"Scope", eTypeScope},
			{"Muzzle", eTypeMuzzle},
			{"Mount", eTypeMount},
			{"GLauncher", eTypeGLauncher},
			{"Magazine", eTypeMagazine},
			{"Custom", eTypeCustom},
			{"Decore", eTypeDecore},
		};
		static constexpr int kTypeCount = std::size(kTypes);

		static const char* kTypeNames[kTypeCount] = {};
		static EattachmentType kTypeValues[kTypeCount] = {};
		static bool kTypesInit = false;
		if (!kTypesInit)
		{
			for (int i = 0; i < kTypeCount; ++i)
			{
				kTypeNames[i] = kTypes[i].first;
				kTypeValues[i] = kTypes[i].second;
			}
			kTypesInit = true;
		}

		if (ImGui::Button("Add new attach"))
		{
			show_add_attach_window[mode] = true;
			select_type_step[mode] = true;
			pending_attachment_type[mode] = EattachmentType(-1);
			attach_search_buf[mode][0] = '\0';
		}

		if (show_add_attach_window[mode])
		{
			ImGui::SetNextWindowSize(ImVec2(340, 420), ImGuiCond_FirstUseEver);
			if (ImGui::Begin("Add new attach", &show_add_attach_window[mode]))
			{
				if (select_type_step[mode])
				{
					ImGui::TextUnformatted("Select attachment type:");
					ImGui::Separator();

					for (auto& [label, type] : kTypes)
					{
						bool selected = (pending_attachment_type[mode] == type);
						if (ImGui::Selectable(label, selected))
						{
							pending_attachment_type[mode] = type;
							select_type_step[mode] = false;
						}
					}

					ImGui::Separator();
					if (ImGui::Button("Cancel"))
					{
						show_add_attach_window[mode] = false;
					}
				}
				else
				{
					ImGui::Text("Type: %s", kTypeNames[pending_attachment_type[mode]]);
					ImGui::SameLine();
					if (ImGui::SmallButton("< Back"))
					{
						select_type_step[mode] = true;
					}

					ImGui::Separator();
					ImGui::InputTextWithHint("##attach_search", "Search...", attach_search_buf[mode], std::size(attach_search_buf[mode]));

					ImGui::BeginChild("##attach_list", ImVec2(0, 260), true);
					for (const auto& sect : pSettings->sections())
					{
						const char* sect_name = sect.Name.c_str();

						if (attach_search_buf[mode][0] != '\0' &&
							strstr(sect_name, attach_search_buf[mode]) == nullptr)
						{
							continue;
						}

						if (item->m_attachments.find(sect.Name.c_str()) !=
							item->m_attachments.end())
						{
							continue;
						}

						if (!pSettings->line_exist(sect_name, "inv_name"))
						{
							continue;
						}

						if (ImGui::Selectable(sect_name))
						{
							item_attachment new_attach{};
							new_attach.m_parent = item;

							xr_string attachment_modifiers_sect =
								xr_string(item->object().cNameSect_str()) + '_' + xr_string(sect_name);
							new_attach.mod_sect_name = attachment_modifiers_sect.c_str();

							new_attach.state.set(eAStateMCombined, !pSettings->line_exist(*new_attach.mod_sect_name, "attachment_hud_visual"));

							new_attach.place.m_model = PKinematics(::Render->model_Create(
								pSettings->line_exist(*new_attach.mod_sect_name, "attachment_visual")
									? pSettings->r_string(*new_attach.mod_sect_name, "attachment_visual")
									: pSettings->r_string(sect_name, "visual")
							));

							new_attach.hud_place.m_model = new_attach.state.test(eAStateMCombined)
															   ? new_attach.place.m_model
															   : PKinematics(::Render->model_Create(
																	 pSettings->r_string(*new_attach.mod_sect_name, "attachment_hud_visual")
																 ));

							new_attach.attachment_type = pending_attachment_type[mode];

							item->m_attachments.emplace(
								sect.Name.c_str(), new_attach
							);
							if (CWeapon* wpn = item->cast_weapon())
							{
								if (new_attach.attachment_type == eTypeScope)
								{
									wpn->m_scopes.push_back(sect_name);
								}
							}
							show_add_attach_window[mode] = false;
							select_type_step[mode] = true;
						}
					}
					ImGui::EndChild();

					ImGui::Separator();
					if (ImGui::Button("Cancel"))
					{
						show_add_attach_window[mode] = false;
					}
				}
			}
			ImGui::End();
		}
		if (!item->m_attachments.empty())
		{
			ImGui::Separator();
			ImGui::Indent(20.0f);
			for (auto& pair : item->m_attachments)
			{
				xr_string attach_sect_name = *pair.first;
				if (ImGui::CollapsingHeader(attach_sect_name.c_str()))
				{
					ImGui::SeparatorText(xr_string("Offset##" + attach_sect_name).c_str());

					Fvector& position = hud_mode ? pair.second.hud_place.position : pair.second.place.position;
					if (ImGui::BeginTable(xr_string("Data##P" + attach_sect_name).c_str(), 1))
					{
						ImGui::TableNextRow();
						ImGui::TableNextColumn();

						ImGui::DragFloat(xr_string("X##P" + attach_sect_name).c_str(), &position.x, _delta_pos, -10.0f, 10.0f, "%.6f");
						ImGui::DragFloat(xr_string("Y##P" + attach_sect_name).c_str(), &position.y, _delta_pos, -10.0f, 10.0f, "%.6f");
						ImGui::DragFloat(xr_string("Z##P" + attach_sect_name).c_str(), &position.z, _delta_pos, -10.0f, 10.0f, "%.6f");

						ImGui::EndTable();
					}

					ImGui::SeparatorText(xr_string("Direction##" + attach_sect_name).c_str());

					Fvector& direction = hud_mode ? pair.second.hud_place.direction : pair.second.place.direction;
					if (ImGui::BeginTable(xr_string("Data##D" + attach_sect_name).c_str(), 1))
					{
						ImGui::TableNextRow();
						ImGui::TableNextColumn();

						ImGui::DragFloat(xr_string("X##D" + attach_sect_name).c_str(), &direction.x, 5, 0, 360, "%.0f");
						ImGui::DragFloat(xr_string("Y##D" + attach_sect_name).c_str(), &direction.y, 5, 0, 360, "%.0f");
						ImGui::DragFloat(xr_string("Z##D" + attach_sect_name).c_str(), &direction.z, 5, 0, 360, "%.0f");

						ImGui::EndTable();
					}

					ImGui::SeparatorText(xr_string("Scale##" + attach_sect_name).c_str());

					Fvector& scale = hud_mode ? pair.second.hud_place.scale : pair.second.place.scale;
					if (ImGui::BeginTable(xr_string("Data##S" + attach_sect_name).c_str(), 1))
					{
						ImGui::TableNextRow();
						ImGui::TableNextColumn();

						ImGui::DragFloat(xr_string("X##S" + attach_sect_name).c_str(), &scale.x, _delta_pos, 0.0f, 10.0f, "%.2f");
						ImGui::DragFloat(xr_string("Y##S" + attach_sect_name).c_str(), &scale.y, _delta_pos, 0.0f, 10.0f, "%.2f");
						ImGui::DragFloat(xr_string("Z##S" + attach_sect_name).c_str(), &scale.z, _delta_pos, 0.0f, 10.0f, "%.2f");

						ImGui::EndTable();
					}

					// ---- attachment Type ----
					ImGui::SeparatorText(xr_string("attachment Type##" + attach_sect_name).c_str());

					int current_idx = 0;
					for (int i = 0; i < kTypeCount; ++i)
					{
						if (kTypeValues[i] == pair.second.attachment_type)
						{
							current_idx = i;
							break;
						}
					}

					if (ImGui::Combo(xr_string("##Type" + attach_sect_name).c_str(), &current_idx, kTypeNames, kTypeCount))
					{
						EattachmentType old_type = pair.second.attachment_type;
						EattachmentType new_type = kTypeValues[current_idx];

						if (old_type != new_type)
						{
							if (CWeapon* wpn = item->cast_weapon())
							{
								if (old_type == eTypeScope)
								{
									auto it = std::find(wpn->m_scopes.begin(), wpn->m_scopes.end(), pair.first);
									if (it != wpn->m_scopes.end())
									{
										wpn->m_scopes.erase(it);
									}
								}
								if (new_type == eTypeScope)
								{
									if (std::find(wpn->m_scopes.begin(), wpn->m_scopes.end(), pair.first) == wpn->m_scopes.end())
									{
										wpn->m_scopes.push_back(pair.first);
									}
								}
							}
							pair.second.attachment_type = new_type;
						}
					}

					ImGui::SeparatorText(xr_string("Select Bone##" + attach_sect_name).c_str());
					int& selectable_bone_id = hud_mode ? pair.second.hud_place.parent_bone_id : pair.second.place.parent_bone_id;
					ImGui::Combo(xr_string("##SelectBone" + attach_sect_name).c_str(), &selectable_bone_id, &bones_names[mode][0], bones_names[mode].size());

					if (ImGui::Button(xr_string("Change model##" + attach_sect_name).c_str()))
					{
						pending_model_sect[mode] = pair.first;
						show_model_popup[mode] = true;
						model_search_buf[mode][0] = '\0';
						ImGui::OpenPopup("##model_pick");
					}

					if (ImGui::Button(xr_string("Remove Attach##" + attach_sect_name).c_str()))
					{
						if (pair.second.place.m_model)
						{
							IRenderVisual* visual = pair.second.place.m_model->dcast_RenderVisual();
							::Render->model_Delete(visual);
						}
						if (pair.second.hud_place.m_model != pair.second.place.m_model)
						{
							IRenderVisual* visual = pair.second.hud_place.m_model->dcast_RenderVisual();
							::Render->model_Delete(visual);
						}

						to_delete[mode].push_back(pair.first);
						if (CWeapon* wpn = item->cast_weapon())
						{
							if (pair.second.attachment_type == eTypeScope)
							{
								auto it = std::find(wpn->m_scopes.begin(), wpn->m_scopes.end(), pair.first);
								if (it != wpn->m_scopes.end())
								{
									wpn->m_scopes.erase(it);
								}
							}
						}
					}
					ImGui::CheckboxFlags("Show attachment", &pair.second.state.flags, eAStateVisible);
					ImGui::CheckboxFlags("Permanent attachment", &pair.second.state.flags, eAStatePermanent);
				}
			}

			if (show_model_popup[mode])
			{
				ImGui::SetNextWindowSize(ImVec2(400, 500), ImGuiCond_FirstUseEver);
				ImGui::SetNextWindowSizeConstraints(ImVec2(250, 200), ImVec2(FLT_MAX, FLT_MAX));

				if (ImGui::Begin("Select model", &show_model_popup[mode]))
				{
					ImGui::TextUnformatted("Select model:");
					ImGui::Separator();
					ImGui::InputTextWithHint("##_search", "Search...", model_search_buf[mode], std::size(model_search_buf[mode]));

					ImGui::BeginChild("##models_list", ImVec2(0, 0), true);

					for (auto& file : files[mode])
					{
						const char* fname = file.name.c_str();
						if (!fname)
						{
							continue;
						}

						if (model_search_buf[mode][0] != '\0' &&
							strstr(fname, model_search_buf[mode]) == nullptr)
						{
							continue;
						}

						if (ImGui::Selectable(fname))
						{
							auto it = item->m_attachments.find(pending_model_sect[mode]);
							if (it != item->m_attachments.end())
							{
								item_attachment& att = it->second;
								item_attachment::placement& other_place = hud_mode ? att.place : att.hud_place;
								item_attachment::placement& curr_place = hud_mode ? att.hud_place : att.place;
								if (curr_place.m_model && curr_place.m_model != other_place.m_model)
								{
									IRenderVisual* visual = curr_place.m_model->dcast_RenderVisual();
									::Render->model_Delete(visual);
									curr_place.m_model = nullptr;
								}

								curr_place.m_model = PKinematics(::Render->model_Create(fname));
								att.state.set(eAStateMCombined, curr_place.m_model == other_place.m_model);
							}
						}
					}
					ImGui::EndChild();

					ImGui::Separator();
					if (ImGui::Button("Cancel"))
					{
						show_model_popup[mode] = false;
					}
				}
				ImGui::End();
			}

			for (auto& k : to_delete[mode])
			{
				item->m_attachments.erase(k);
			}
			to_delete[mode].clear();
			ImGui::Unindent(20.0f);
			ImGui::Separator();
		}
	}
}

static void HudAdjustDrawItemSettings(attachable_hud_item* item)
{
	if (item == nullptr)
	{
		return;
	}

	if (!ImGui::CollapsingHeader(*shared_str().printf("Item: %s", item->m_sect_name.c_str())))
	{
		return;
	}

	THudLightLaser* ll = nullptr;
	THudLightTorch* lt = nullptr;

	if (CHudItem* hud_item = item->m_parent_hud_item)
	{
		lt = hud_item->GetHudLight();

		if (CWeapon* wpn = hud_item->cast_weapon())
		{
			ll = wpn->GetLightLaser();
		}
	}

	firedeps fd = {};
	item->setup_firedeps(fd);

	if (item->m_measures.m_prop_flags.test(item->m_measures.e_fire_point))
	{
		if (ImGui::CollapsingHeader("Fire point"))
		{
			ImGui::SeparatorText("Offset##FP");
	
			Fvector& position = item->m_measures.m_fire_point_offset;
			if (ImGui::Button("Reset##FPOffset"))
			{
				position = pSettings->r_fvector3(item->m_sect_name, "fire_point");
			}
	
			if (ImGui::BeginTable("Data##FPP", 1))
			{
				ImGui::TableNextRow();
	
				ImGui::TableNextColumn();
	
				ImGui::DragFloat("X##FPP", &position.x, _delta_pos, -1.0f, 1.0f, "%.6f");
				ImGui::DragFloat("Y##FPP", &position.y, _delta_pos, -1.0f, 1.0f, "%.6f");
				ImGui::DragFloat("Z##FPP", &position.z, _delta_pos, -1.0f, 1.0f, "%.6f");
	
				ImGui::EndTable();
			}
		}
	}
	
	if (item->m_measures.m_prop_flags.test(item->m_measures.e_fire_point2))
	{
		if (ImGui::CollapsingHeader("Fire point 2"))
		{
			ImGui::SeparatorText("Offset##FP2");
	
			Fvector& position = item->m_measures.m_fire_point2_offset;
			if (ImGui::Button("Reset##FP2Offset"))
			{
				position = pSettings->r_fvector3(item->m_sect_name, "fire_point2");
			}
	
			if (ImGui::BeginTable("Data##FP2P", 1))
			{
				ImGui::TableNextRow();
	
				ImGui::TableNextColumn();
	
				ImGui::DragFloat("X##FP2P", &position.x, _delta_pos, -1.0f, 1.0f, "%.6f");
				ImGui::DragFloat("Y##FP2P", &position.y, _delta_pos, -1.0f, 1.0f, "%.6f");
				ImGui::DragFloat("Z##FP2P", &position.z, _delta_pos, -1.0f, 1.0f, "%.6f");
	
				ImGui::EndTable();
			}
		}
	}
	
	if (item->m_measures.m_prop_flags.test(item->m_measures.e_shell_point))
	{
		if (ImGui::CollapsingHeader("Shell point"))
		{
			ImGui::SeparatorText("Offset##SP");
	
			Fvector& position = item->m_measures.m_shell_point_offset;
			if (ImGui::Button("Reset##SPOffset"))
			{
				position = pSettings->r_fvector3(item->m_sect_name, "shell_point");
			}
	
			if (ImGui::BeginTable("Data##SPP", 1))
			{
				ImGui::TableNextRow();
	
				ImGui::TableNextColumn();
	
				ImGui::DragFloat("X##SPP", &position.x, _delta_pos, -1.0f, 1.0f, "%.6f");
				ImGui::DragFloat("Y##SPP", &position.y, _delta_pos, -1.0f, 1.0f, "%.6f");
				ImGui::DragFloat("Z##SPP", &position.z, _delta_pos, -1.0f, 1.0f, "%.6f");
	
				ImGui::EndTable();
			}
		}
	}

	AdjustDrawItemAttachmentsSettings(item->m_parent_hud_item->cast_inventory_item(), item->m_model, true);

	auto drawPositions = [&](EHudOffsetType offset_type) -> void
	{
		ImGui::SeparatorText("Position##HUD");

		Fvector& position = offset_type ? item->m_measures.m_hands_positions.hands_offsets[0][offset_type] : item->m_measures.m_hands_attach_real[0];
		string32 btnName;
		xr_sprintf(btnName, "Reset##HPosition_%d", (u8)offset_type);

		string64 _prefix = {};
		xr_sprintf(_prefix, "%s", UI().is_widescreen() ? "_16x9" : "");
		string128 val_name = {};

		if (ImGui::Button(btnName))
		{
			switch (offset_type)
			{
			case EHudOffsetType::eAim:
			{
				position = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, "zoom_offset", zero_vel));
				break;
			}
			case EHudOffsetType::eAimGL:
			{
				xr_strconcat(val_name, "gl_hud_offset_pos", _prefix);
				position = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, "grenade_zoom_offset", zero_vel));
				break;
			}
			case EHudOffsetType::eAimAlt:
			{
				xr_strconcat(val_name, "alter_aim_hud_offset_pos", _prefix);
				position = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, zero_vel);
				break;
			}
			case EHudOffsetType::eSafemode:
			{
				xr_strconcat(val_name, "safemode_hud_offset_pos", _prefix);
				position = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, zero_vel);
				break;
			}
			case EHudOffsetType::eCollision:
			{
				xr_strconcat(val_name, "collision_hud_offset_pos", _prefix);
				position = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, zero_vel);
				break;
			}
			default:
			{
				xr_strconcat(val_name, "hands_position", _prefix);
				position = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, zero_vel);
				break;
			}
			};
		}

		if (ImGui::BeginTable("Data##HUDP", 1))
		{
			ImGui::TableNextRow();

			ImGui::TableNextColumn();

			ImGui::DragFloat("X##HUDP", &position.x, _delta_pos, -1.0f, 1.0f, "%.6f");
			ImGui::DragFloat("Y##HUDP", &position.y, _delta_pos, -1.0f, 1.0f, "%.6f");
			ImGui::DragFloat("Z##HUDP", &position.z, _delta_pos, -1.0f, 1.0f, "%.6f");

			ImGui::EndTable();
		}

		ImGui::SeparatorText("Rotation##HUD");

		Fvector& rotation = offset_type ? item->m_measures.m_hands_positions.hands_offsets[1][offset_type] : item->m_measures.m_hands_attach_real[1];
		xr_sprintf(btnName, "Reset##HRotation_%d", (u8)offset_type);
		if (ImGui::Button(btnName))
		{
			switch (offset_type)
			{
			case EHudOffsetType::eAim:
			{
				xr_strconcat(val_name, "aim_hud_offset_rot", _prefix);
				rotation = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, zero_vel);
				break;
			}
			case EHudOffsetType::eAimGL:
			{
				xr_strconcat(val_name, "gl_hud_offset_rot", _prefix);
				rotation = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, zero_vel);
				break;
			}
			case EHudOffsetType::eAimAlt:
			{
				xr_strconcat(val_name, "alter_aim_hud_offset_rot", _prefix);
				rotation = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, zero_vel);
				break;
			}
			case EHudOffsetType::eSafemode:
			{
				xr_strconcat(val_name, "safemode_hud_offset_rot", _prefix);
				rotation = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, zero_vel);
				break;
			}
			case EHudOffsetType::eCollision:
			{
				xr_strconcat(val_name, "collision_hud_offset_rot", _prefix);
				rotation = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, zero_vel);
				break;
			}
			default:
			{
				xr_strconcat(val_name, "hands_orientation", _prefix);
				rotation = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, zero_vel);
				break;
			}
			}
		}

		if (ImGui::BeginTable("Data##HUDR", 1))
		{
			ImGui::TableNextRow();

			ImGui::TableNextColumn();

			ImGui::DragFloat("X##HUDR", &rotation.x, _delta_rot, -360.0f, 360.0f, "%.6f");
			ImGui::DragFloat("Y##HUDR", &rotation.y, _delta_rot, -360.0f, 360.0f, "%.6f");
			ImGui::DragFloat("Z##HUDR", &rotation.z, _delta_rot, -360.0f, 360.0f, "%.6f");

			ImGui::TableNextColumn();

			ImGui::EndTable();
		}

		if (offset_type == EHudOffsetType::eCollision)
		{
			ImGui::SeparatorText("Box Scale##OBBS");
			if (ImGui::BeginTable("Data##OBBS", 1))
			{
				ImGui::TableNextRow();

				ImGui::TableNextColumn();

				Fvector& obb_scale = item->m_measures.m_collision_params.obb_scale;

				ImGui::DragFloat("X##OBBS", &obb_scale.x, _delta_pos, -360.0f, 360.0f, "%.6f");
				ImGui::DragFloat("Y##OBBS", &obb_scale.y, _delta_pos, -360.0f, 360.0f, "%.6f");
				ImGui::DragFloat("Z##OBBS", &obb_scale.z, _delta_pos, -360.0f, 360.0f, "%.6f");
				ImGui::TableNextColumn();

				ImGui::EndTable();
			}

			ImGui::SeparatorText("Box Center##OBBC");

			if (ImGui::BeginTable("Data##OBBC", 1))
			{
				ImGui::TableNextRow();

				ImGui::TableNextColumn();

				Fvector& obb_center = item->m_measures.m_collision_params.obb_pos;

				ImGui::DragFloat("X##OBBC", &obb_center.x, _delta_pos, -360.0f, 360.0f, "%.6f");
				ImGui::DragFloat("Y##OBBC", &obb_center.y, _delta_pos, -360.0f, 360.0f, "%.6f");
				ImGui::DragFloat("Z##OBBC", &obb_center.z, _delta_pos, -360.0f, 360.0f, "%.6f");
				ImGui::TableNextColumn();

				ImGui::EndTable();
			}
			
			ImGui::SeparatorText("Spring inertion params##SIP");

			if (ImGui::BeginTable("Data##SIP", 1))
			{
				ImGui::TableNextRow();

				ImGui::TableNextColumn();
				
				ImGui::DragFloat("Stifness##SIP", &item->m_measures.m_collision_params.stifness, _delta_pos, EPS_S, FLT_MAX, "%.6f");
				ImGui::DragFloat("Damping##SIP", &item->m_measures.m_collision_params.damping, _delta_pos, EPS_S, FLT_MAX, "%.6f");
				ImGui::TableNextColumn();

				ImGui::EndTable();
			}
		}
	};

	if (!item->m_model_combined)
	{
		if (ImGui::CollapsingHeader("Offset 0 (default)"))
		{
			drawPositions(EHudOffsetType::eDefault);
		}
	}
	
	if (ImGui::CollapsingHeader("Offset 1 (aim)"))
	{
		drawPositions(EHudOffsetType::eAim);
	}
	
	if (ImGui::CollapsingHeader("Offset 2 (aim gl)"))
	{
		drawPositions(EHudOffsetType::eAimGL);
	}
	
	if (ImGui::CollapsingHeader("Offset 3 (aim alter)"))
	{
		drawPositions(EHudOffsetType::eAimAlt);
	}
	
	if (ImGui::CollapsingHeader("Offset 4 (safemode)"))
	{
		drawPositions(EHudOffsetType::eSafemode);
	}
	
	if (ImGui::CollapsingHeader("Offset 5 (collision)"))
	{
		drawPositions(EHudOffsetType::eCollision);
	}

	if (ImGui::CollapsingHeader("Item Offset"))
	{
		ImGui::SeparatorText("Position##Item");
		Fvector& position = item->m_measures.m_item_attach[0];

		string64 _prefix = {};
		xr_sprintf(_prefix, "%s", UI().is_widescreen() ? "_16x9" : "");
		string128 val_name = {};

		if (ImGui::Button("Reset##IPosition"))
		{
			position = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, "item_position", pSettings->r_fvector3(item->m_sect_name, "position"));
		}

		if (ImGui::BeginTable("Data##HUDPI", 1))
		{
			ImGui::TableNextRow();

			ImGui::TableNextColumn();

			ImGui::DragFloat("X##HUDP", &position.x, _delta_pos, -1.0f, 1.0f, "%.6f");
			ImGui::DragFloat("Y##HUDP", &position.y, _delta_pos, -1.0f, 1.0f, "%.6f");
			ImGui::DragFloat("Z##HUDP", &position.z, _delta_pos, -1.0f, 1.0f, "%.6f");

			ImGui::EndTable();
		}

		ImGui::SeparatorText("Rotation##Item");
		Fvector& rotation = item->m_measures.m_item_attach[1];
		if (ImGui::Button("Reset##IRotation"))
		{
			rotation = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, "item_orientation", pSettings->r_fvector3(item->m_sect_name, "orientation"));
		}

		if (ImGui::BeginTable("Data##HUDR", 1))
		{
			ImGui::TableNextRow();

			ImGui::TableNextColumn();

			ImGui::DragFloat("X##HUDR", &rotation.x, _delta_rot, -360.0f, 360.0f, "%.6f");
			ImGui::DragFloat("Y##HUDR", &rotation.y, _delta_rot, -360.0f, 360.0f, "%.6f");
			ImGui::DragFloat("Z##HUDR", &rotation.z, _delta_rot, -360.0f, 360.0f, "%.6f");

			ImGui::TableNextColumn();

			ImGui::EndTable();
		}
	}

	if (lt && lt->GetTorchInstalled())
	{
		if (ImGui::CollapsingHeader("Torch params"))
		{
			ImGui::SeparatorText("Position##TL");
	
			Fvector& position = lt->LightOffset;
	
			if (ImGui::Button("Reset##TLOffset"))
			{
				position.x = READ_IF_EXISTS(pSettings, r_float, lt->Section, "torch_attach_offset_x", 0.0f);
				position.y = READ_IF_EXISTS(pSettings, r_float, lt->Section, "torch_attach_offset_y", 0.0f);
				position.z = READ_IF_EXISTS(pSettings, r_float, lt->Section, "torch_attach_offset_z", 0.0f);
			}
	
			if (ImGui::BeginTable("Data##TLOffset", 1))
			{
				ImGui::TableNextRow();
	
				ImGui::TableNextColumn();
	
				ImGui::DragFloat("X##TLOffset", &position.x, _delta_pos, -1.0f, 1.0f, "%.6f");
				ImGui::DragFloat("Y##TLOffset", &position.y, _delta_pos, -1.0f, 1.0f, "%.6f");
				ImGui::DragFloat("Z##TLOffset", &position.z, _delta_pos, -1.0f, 1.0f, "%.6f");
	
				ImGui::EndTable();
			}
		}
	}

	if (ll && ll->GetTorchInstalled())
	{
		if (ImGui::CollapsingHeader("Laser params"))
		{
			ImGui::SeparatorText("Position##TL");
	
			Fvector& position = ll->LightOffset;
	
			if (ImGui::Button("Reset##TLOffset"))
			{
				position.x = READ_IF_EXISTS(pSettings, r_float, ll->Section, "laserdot_attach_offset_x", 0.0f);
				position.y = READ_IF_EXISTS(pSettings, r_float, ll->Section, "laserdot_attach_offset_y", 0.0f);
				position.z = READ_IF_EXISTS(pSettings, r_float, ll->Section, "laserdot_attach_offset_z", 0.0f);
	
				position = READ_IF_EXISTS(pSettings, r_fvector3, ll->Section, "laserdot_attach_offset", position);
			}
	
			if (ImGui::BeginTable("Data##TLOffset", 1))
			{
				ImGui::TableNextRow();
	
				ImGui::TableNextColumn();
	
				ImGui::DragFloat("X##TLOffset", &position.x, _delta_pos, -1.0f, 1.0f, "%.6f");
				ImGui::DragFloat("Y##TLOffset", &position.y, _delta_pos, -1.0f, 1.0f, "%.6f");
				ImGui::DragFloat("Z##TLOffset", &position.z, _delta_pos, -1.0f, 1.0f, "%.6f");
	
				ImGui::EndTable();
			}
	
			ImGui::SeparatorText("Rotation##TL");
	
			Fvector2& angle = ll->LightSpotAngle;
			if (ImGui::Button("Reset##TLAngle"))
			{
				angle = READ_IF_EXISTS(pSettings, r_fvector2, ll->Section, "laser_spot_angle", angle.set(2, 5));
				angle.mul(M_PI / 180);
			}
	
			if (ImGui::BeginTable("Data##TLAngle", 1))
			{
				ImGui::TableNextRow();
	
				ImGui::TableNextColumn();
	
				ImGui::DragFloat("X##TLAngle", &angle.x, _delta_rot, -360.0f, 360.0f, "%.6f");
				ImGui::DragFloat("Y##TLAngle", &angle.y, _delta_rot, -360.0f, 360.0f, "%.6f");
				ImGui::TableNextColumn();
	
				ImGui::EndTable();
			}
		}
	}
}

void RenderHUDAdjustManager()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_HudAdjustManager)])
		return;

	if (!g_pGameLevel)
		return;

	if (!ai().get_alife())
		return;

	if (imgui_hud_adjust_manager.is_initialized == false)
		return;

	if (!g_actor)
		return;

	if (!g_player_hud)
		return;

	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.0f, 0.0f, 0.0f, kGeneralAlphaLevelForImGuiWindows));

	ImGui::BeginDisabled(g_player_hud->attached_item(0) == nullptr && g_player_hud->attached_item(1) == nullptr);

	if (ImGui::Begin("Hud Adjust", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_HudAdjustManager)]))
	{
		HudAdjustDrawSaveButton();
		HudAdjustDrawAdjustSettings();
		//HudAdjustDrawHandsSettings();
		HudAdjustDrawItemSettings(g_player_hud->attached_item(0));
		HudAdjustDrawItemSettings(g_player_hud->attached_item(1));
	}

	ImGui::End();
	ImGui::EndDisabled();
	ImGui::PopStyleColor(1);
}