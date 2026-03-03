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

	THudLightLaser* ll = nullptr;
	THudLightTorch* lt = nullptr;
	CInventoryItem* p_item = g_actor->inventory().ActiveItem();
	if (p_item)
	{
		if (p_item->cast_hud_item())
		{
			lt = p_item->cast_hud_item()->GetHudLight();
		}
		if (p_item->cast_weapon())
		{
			ll = p_item->cast_weapon()->GetLightLaser();
		}
	}

	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.0f, 0.0f, 0.0f, kGeneralAlphaLevelForImGuiWindows));

	ImGui::BeginDisabled(!p_item);

	if (ImGui::Begin("Hud Adjust", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_HudAdjustManager)]))
	{
		xr_string p_active_weapon_name = "NO ACTIVE WEAPON";

		if (p_item)
		{
			p_active_weapon_name = Platform::ANSI_TO_UTF8(p_item->NameShort());
		}
		ImGui::Text("Active weapon: %s", p_active_weapon_name.c_str());
		ImGui::PushItemWidth(-1);
		ImGui::SameLine(ImGui::CalcItemWidth() - ImGui::CalcTextSize("Save").x);
		if (ImGui::Button("Save"))
		{
			string_path fn;
			attachable_hud_item* p_hud_item_first = g_player_hud->attached_item(0);
			attachable_hud_item* p_hud_item_second = g_player_hud->attached_item(1);

			FS.update_path(fn, "$app_data_root$", "hud_adjust\\saved.ltx");
			CInifile file(fn, FALSE, TRUE, TRUE);
			file.set_override_names(TRUE);

			auto writeParams = [](attachable_hud_item* p_item, CInifile& file) -> void
			{
				string64 sect = "";
				xr_sprintf(sect, sizeof(sect), p_item->m_sect_name.c_str());
				file.w_u8(sect, "attach_place_idx", p_item->m_attach_place_idx);

				string64 _prefix = {};
				xr_sprintf(_prefix, "%s", UI().is_widescreen() ? "_16x9" : "");
				string128 val_name = {};

				xr_strconcat(val_name, "hands_position", _prefix);
				file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_attach_real[0]);
				xr_strconcat(val_name, "hands_orientation", _prefix);
				file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_attach_real[1]);

				file.w_fvector3(sect, "item_position", p_item->m_measures.m_item_attach[0]);
				file.w_fvector3(sect, "item_orientation", p_item->m_measures.m_item_attach[1]);

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

				xr_strconcat(val_name, "aim_hud_offset_pos", _prefix);
				file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[0][EHudOffsetType::eAim]);
				xr_strconcat(val_name, "aim_hud_offset_rot", _prefix);
				file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[1][EHudOffsetType::eAim]);

				xr_strconcat(val_name, "gl_hud_offset_pos", _prefix);
				file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[0][EHudOffsetType::eAimGL]);
				xr_strconcat(val_name, "gl_hud_offset_rot", _prefix);
				file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[1][EHudOffsetType::eAimGL]);

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
			};

			if (p_hud_item_first)
			{
				writeParams(p_hud_item_first, file);
			}
			if (p_hud_item_second)
			{
				writeParams(p_hud_item_second, file);
			}
			GAME_NEWS_DATA				news_data;
			news_data.m_type = GAME_NEWS_DATA::eNewsType::eNews;
			news_data.news_caption = "Saved result to:";
			news_data.news_text = fn;
			news_data.show_time = 5000;
			news_data.texture_name = "ui_iconsTotal_bar_darklab_documents2";
			Actor()->AddGameNews(news_data);
		}
		xr_string itemSection = "Unknown";
		if (p_item)
		{
			itemSection = p_item->m_section_id.c_str();
		}

		ImGui::Text("Item Section: %s", itemSection.c_str());
		ImGui::SameLine(ImGui::CalcItemWidth() - ImGui::CalcTextSize("?").x);
		ImGui::Button("?");
		if (ImGui::IsItemHovered())
		{
			ImGui::SetTooltip("* Shift + drag\nFor slower value change\n* Ctrl + click(or double click)\nInput text into slider\n* Alt + drag\nFor quick value change, opposite of Shift key");
		}

		if (p_item)
		{
			if (g_player_hud)
			{
				const char* p_hand = "single hand";
				bool two_hands = false;
				if (g_player_hud->attached_item(1))
				{
					p_hand = "two hands";
					two_hands = true;
				}

				ImGui::Text("Mode: %s", p_hand);

				ImGui::Checkbox("Show crosshair", &hud_adj_crosshair);
				ImGui::Checkbox("Toggle weapon aim", &b_toggle_weapon_aim);
				ImGui::Checkbox("Show fire point box", &forceFPDraw);
				ImGui::Checkbox("Show fire point 2 box", &forceFP2Draw);
				ImGui::Checkbox("Show shell point box", &forceSPDraw);
				ImGui::Checkbox("Show laser point box", &forceLPDraw);
				ImGui::Checkbox("Show torch point box", &forceTPDraw);
				ImGui::SetNextItemWidth(80.0f);
				ImGui::InputFloat("Position step", &_delta_pos, 0, 0, "%.6f");
				ImGui::SetNextItemWidth(80.0f);
				ImGui::InputFloat("Rotation step", &_delta_rot, 0, 0, "%.6f");

				auto p_draw_info_hud_item = [](attachable_hud_item* p_item, u8 index, THudLightLaser* ll, THudLightTorch* lt) -> void
				{
					if (p_item)
					{
						string16 name = "";
						xr_sprintf(name, sizeof(name), "attached_item#%d", index);
						ImGui::SeparatorText(name);

						R_ASSERT2(p_item->m_parent, "must be valid!");

						string32 item_header_name = "";
						string64 hud_header_name = "";
						xr_sprintf(hud_header_name, sizeof(hud_header_name), "Hud = %s##hh%d", p_item->m_parent->section_name().c_str(), index);

						xr_sprintf(item_header_name, "Item = %s##hh%d", p_item->m_sect_name.c_str(), index);

						firedeps fd;
						p_item->setup_firedeps(fd);
						if (p_item->m_measures.m_prop_flags.test(p_item->m_measures.e_fire_point))
						{
							if (ImGui::CollapsingHeader("Fire point"))
							{
								ImGui::SeparatorText("Offset##FP");

								Fvector& position = p_item->m_measures.m_fire_point_offset;
								if (ImGui::Button("Reset##FPOffset"))
								{
									position = pSettings->r_fvector3(p_item->m_sect_name, "fire_point");
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

						if (p_item->m_measures.m_prop_flags.test(p_item->m_measures.e_fire_point2))
						{
							if (ImGui::CollapsingHeader("Fire point 2"))
							{
								ImGui::SeparatorText("Offset##FP2");

								Fvector& position = p_item->m_measures.m_fire_point2_offset;
								if (ImGui::Button("Reset##FP2Offset"))
								{
									position = pSettings->r_fvector3(p_item->m_sect_name, "fire_point2");
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

						if (p_item->m_measures.m_prop_flags.test(p_item->m_measures.e_shell_point))
						{
							if (ImGui::CollapsingHeader("Shell point"))
							{
								ImGui::SeparatorText("Offset##SP");

								Fvector& position = p_item->m_measures.m_shell_point_offset;
								if (ImGui::Button("Reset##SPOffset"))
								{
									position = pSettings->r_fvector3(p_item->m_sect_name, "shell_point");
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

						{
							if (ImGui::CollapsingHeader(hud_header_name))
							{
								xr_string fmt;
								const EHudOffsetType offsetIdx = p_item->m_parent_hud_item->GetCurrentHudOffsetIdx();
								switch (offsetIdx)
								{
									case EHudOffsetType::eAim:
									{
										fmt = "aim";
										break;
									}
									case EHudOffsetType::eAimGL:
									{
										fmt = "aim gl";
										break;
									}
									case EHudOffsetType::eAimAlt:
									{
										fmt = "aim alt";
										break;
									}
									case EHudOffsetType::eSafemode:
									{
										fmt = "safemode";
										break;
									}
									case EHudOffsetType::eCollision:
									{
										fmt = "collision";
										break;
									}
									default:
									{
										fmt = "default";
										break;
									}
								};
								ImGui::Text("Hud offset index: %d (%s)", offsetIdx, fmt.c_str());

								auto drawHudParameters = [](attachable_hud_item* p_item, const EHudOffsetType attach_idx) -> void
									{
										ImGui::SeparatorText("Position##HUD");

										Fvector& position = attach_idx ? p_item->m_measures.m_hands_positions.hands_offsets[0][attach_idx] : p_item->m_measures.m_hands_attach_real[0];
										string32 btnName;
										xr_sprintf(btnName, "Reset##HPosition_%d", attach_idx);

										string64 _prefix = {};
										xr_sprintf(_prefix, "%s", UI().is_widescreen() ? "_16x9" : "");
										string128 val_name = {};

										if (ImGui::Button(btnName))
										{
											switch (attach_idx)
											{
											case EHudOffsetType::eAim:
											{
												xr_strconcat(val_name, "aim_hud_offset_pos", _prefix);
												position = READ_IF_EXISTS(pSettings, r_fvector3, p_item->m_sect_name, val_name, zero_vel);
												break;
											}
											case EHudOffsetType::eAimGL:
											{
												xr_strconcat(val_name, "gl_hud_offset_pos", _prefix);
												position = READ_IF_EXISTS(pSettings, r_fvector3, p_item->m_sect_name, val_name, zero_vel);
												break;
											}
											case EHudOffsetType::eAimAlt:
											{
												xr_strconcat(val_name, "alter_aim_hud_offset_pos", _prefix);
												position = READ_IF_EXISTS(pSettings, r_fvector3, p_item->m_sect_name, val_name, zero_vel);
												break;
											}
											case EHudOffsetType::eSafemode:
											{
												xr_strconcat(val_name, "safemode_hud_offset_pos", _prefix);
												position = READ_IF_EXISTS(pSettings, r_fvector3, p_item->m_sect_name, val_name, zero_vel);
												break;
											}
											case EHudOffsetType::eCollision:
											{
												xr_strconcat(val_name, "collision_hud_offset_pos", _prefix);
												position = READ_IF_EXISTS(pSettings, r_fvector3, p_item->m_sect_name, val_name, zero_vel);
												break;
											}
											default:
											{
												xr_strconcat(val_name, "hands_position", _prefix);
												position = READ_IF_EXISTS(pSettings, r_fvector3, p_item->m_sect_name, val_name, zero_vel);
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

										Fvector& rotation = attach_idx ? p_item->m_measures.m_hands_positions.hands_offsets[1][attach_idx] : p_item->m_measures.m_hands_attach_real[1];
										xr_sprintf(btnName, "Reset##HRotation_%d", attach_idx);
										if (ImGui::Button(btnName))
										{
											switch (attach_idx)
											{
											case EHudOffsetType::eAim:
											{
												xr_strconcat(val_name, "aim_hud_offset_rot", _prefix);
												rotation = READ_IF_EXISTS(pSettings, r_fvector3, p_item->m_sect_name, val_name, zero_vel);
												break;
											}
											case EHudOffsetType::eAimGL:
											{
												xr_strconcat(val_name, "gl_hud_offset_rot", _prefix);
												rotation = READ_IF_EXISTS(pSettings, r_fvector3, p_item->m_sect_name, val_name, zero_vel);
												break;
											}
											case EHudOffsetType::eAimAlt:
											{
												xr_strconcat(val_name, "alter_aim_hud_offset_rot", _prefix);
												rotation = READ_IF_EXISTS(pSettings, r_fvector3, p_item->m_sect_name, val_name, zero_vel);
												break;
											}
											case EHudOffsetType::eSafemode:
											{
												xr_strconcat(val_name, "safemode_hud_offset_rot", _prefix);
												rotation = READ_IF_EXISTS(pSettings, r_fvector3, p_item->m_sect_name, val_name, zero_vel);
												break;
											}
											case EHudOffsetType::eCollision:
											{
												xr_strconcat(val_name, "collision_hud_offset_rot", _prefix);
												rotation = READ_IF_EXISTS(pSettings, r_fvector3, p_item->m_sect_name, val_name, zero_vel);
												break;
											}
											default:
											{
												xr_strconcat(val_name, "hands_orientation", _prefix);
												rotation = READ_IF_EXISTS(pSettings, r_fvector3, p_item->m_sect_name, val_name, zero_vel);
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

										if (attach_idx == EHudOffsetType::eCollision)
										{
											ImGui::SeparatorText("Box Scale##OBBS");
											if (ImGui::BeginTable("Data##OBBS", 1))
											{
												ImGui::TableNextRow();

												ImGui::TableNextColumn();

												Fvector& obb_scale = p_item->m_measures.m_collision_params.obb_scale;

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

												Fvector& obb_center = p_item->m_measures.m_collision_params.obb_pos;

												ImGui::DragFloat("X##OBBC", &obb_center.x, _delta_pos, -360.0f, 360.0f, "%.6f");
												ImGui::DragFloat("Y##OBBC", &obb_center.y, _delta_pos, -360.0f, 360.0f, "%.6f");
												ImGui::DragFloat("Z##OBBC", &obb_center.z, _delta_pos, -360.0f, 360.0f, "%.6f");
												ImGui::TableNextColumn();

												ImGui::EndTable();
											}
										}
									};

								if (ImGui::CollapsingHeader("Offset 0 (default)"))
								{
									drawHudParameters(p_item, EHudOffsetType::eDefault);
								}

								if (ImGui::CollapsingHeader("Offset 1 (aim)"))
								{
									drawHudParameters(p_item, EHudOffsetType::eAim);
								}

								if (ImGui::CollapsingHeader("Offset 2 (aim gl)"))
								{
									drawHudParameters(p_item, EHudOffsetType::eAimGL);
								}

								if (ImGui::CollapsingHeader("Offset 3 (aim alter)"))
								{
									drawHudParameters(p_item, EHudOffsetType::eAimAlt);
								}

								if (ImGui::CollapsingHeader("Offset 4 (safemode)"))
								{
									drawHudParameters(p_item, EHudOffsetType::eSafemode);
								}

								if (ImGui::CollapsingHeader("Offset 5 (collision)"))
								{
									drawHudParameters(p_item, EHudOffsetType::eCollision);
								}
							}
						}

						if (ImGui::CollapsingHeader(item_header_name))
						{
							ImGui::SeparatorText("Position##Item");
							Fvector& position = p_item->m_measures.m_item_attach[0];

							string64 _prefix = {};
							xr_sprintf(_prefix, "%s", UI().is_widescreen() ? "_16x9" : "");
							string128 val_name = {};

							if (ImGui::Button("Reset##IPosition"))
							{
								position = pSettings->r_fvector3(p_item->m_sect_name, "item_position");
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
							Fvector& rotation = p_item->m_measures.m_item_attach[1];
							if (ImGui::Button("Reset##IRotation"))
							{
								rotation = pSettings->r_fvector3(p_item->m_sect_name, "item_orientation");
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
				};

				attachable_hud_item* p_hud_item_first = g_player_hud->attached_item(0);

				p_draw_info_hud_item(p_hud_item_first, 0, ll, lt);

				if (two_hands)
				{
					attachable_hud_item* p_hud_item_second = g_player_hud->attached_item(1);
					lt = p_hud_item_second->m_parent_hud_item->GetHudLight();
					p_draw_info_hud_item(p_hud_item_second, 1, nullptr, lt);
				}
			}
		}

		ImGui::End();
	}

	ImGui::EndDisabled();
	ImGui::PopStyleColor(1);
}