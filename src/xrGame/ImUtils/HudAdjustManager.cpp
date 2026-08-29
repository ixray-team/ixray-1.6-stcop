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

		file.w_fvector3(sect, "collision_box_pos", p_item->m_measures.m_collision_params.obb_pos);
		file.w_fvector3(sect, "collision_box_scale", p_item->m_measures.m_collision_params.obb_scale);
	};
	
	if (p_hud_item_first)
	{
		writeParams(p_hud_item_first, file);
	}

	if (p_hud_item_second)
	{
		writeParams(p_hud_item_second, file);
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
				xr_strconcat(val_name, "aim_hud_offset_pos", _prefix);
				position = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, zero_vel);
				break;
			}
			case EHudOffsetType::eAimGL:
			{
				xr_strconcat(val_name, "gl_hud_offset_pos", _prefix);
				position = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, val_name, zero_vel);
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
		}
	};

	if (ImGui::CollapsingHeader("Offset 0 (default)"))
	{
		drawPositions(EHudOffsetType::eDefault);
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
			position = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, "item_position", zero_vel);
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
			rotation = READ_IF_EXISTS(pSettings, r_fvector3, item->m_sect_name, "item_orientation", zero_vel);
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