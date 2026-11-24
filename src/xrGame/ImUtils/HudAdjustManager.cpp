#include "StdAfx.h"
#include "../Level.h"
#include "../Actor.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../player_hud.h"
#include "../WatchDevice/WatchDevice.h"
#include "ai_space.h"
#include "../../xrUI/ui_base.h"
#include "ImUtils.h"
#include "../game_news.h"
#include "../HudTorchLight.h"
#include "../Weapon.h"
#include "../CameraRig.h"

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
			file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_attach_real[EHudOffsetAxis::eAxisPos]);
			xr_strconcat(val_name, "hands_orientation", _prefix);
			file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_attach_real[EHudOffsetAxis::eAxisRot]);

			file.w_fvector3(sect, "item_position", p_item->m_measures.m_item_attach[EHudOffsetAxis::eAxisPos]);
			file.w_fvector3(sect, "item_orientation", p_item->m_measures.m_item_attach[EHudOffsetAxis::eAxisRot]);

			if (!p_item->IkEnabled || pSettings->line_exist(p_item->m_sect_name, "hud_ik"))
			{
				file.w_bool(sect, "hud_ik", p_item->IkEnabled);
			}
		}
		else
		{
			file.w_fvector3(sect, "position", p_item->m_measures.m_item_attach[EHudOffsetAxis::eAxisPos]);
			file.w_fvector3(sect, "orientation", p_item->m_measures.m_item_attach[EHudOffsetAxis::eAxisRot]);
		}

		const hud_item_measures& measures = p_item->m_measures;

		if (measures.m_prop_flags.test(measures.e_shell_point))
		{
			file.w_fvector3(sect, "shell_point", measures.m_shell_point_offset);

			if (measures.m_shell_bone != BI_NONE)
			{
				file.w_string(sect, "shell_bone", p_item->m_model->LL_BoneName_dbg(measures.m_shell_bone));
			}
		}
	
		if (measures.m_prop_flags.test(measures.e_fire_point))
		{
			file.w_fvector3(sect, "fire_point", measures.m_fire_point_offset);
			file.w_string(sect, "fire_bone", p_item->m_model->LL_BoneName_dbg(measures.m_fire_bone));
		}
	
		if (measures.m_prop_flags.test(measures.e_fire_point2))
		{
			file.w_fvector3(sect, "fire_point2", measures.m_fire_point2_offset);

			if (measures.m_fire_bone2 != BI_NONE)
			{
				file.w_string(sect, "fire_bone2", p_item->m_model->LL_BoneName_dbg(measures.m_fire_bone2));
			}
		}

		if (!p_item->m_model_combined)
		{
			xr_strconcat(val_name, "aim_hud_offset_pos", _prefix);
			file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAim]);
			xr_strconcat(val_name, "aim_hud_offset_rot", _prefix);
			file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim]);

			xr_strconcat(val_name, "gl_hud_offset_pos", _prefix);
			file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimGL]);
			xr_strconcat(val_name, "gl_hud_offset_rot", _prefix);
			file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL]);
		}
		else
		{
			file.w_fvector3(sect, "zoom_offset", p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAim]);
			file.w_float(sect, "zoom_rotate_x", p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].x);
			file.w_float(sect, "zoom_rotate_y", p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].y);
			file.w_float(sect, "zoom_rotate_z", p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAim].z);

			file.w_fvector3(sect, "grenade_zoom_offset", p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimGL]);
			file.w_float(sect, "grenade_zoom_rotate_x", p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].x);
			file.w_float(sect, "grenade_zoom_rotate_y", p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].y);
			file.w_float(sect, "grenade_zoom_rotate_z", p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimGL].z);
		}

		xr_strconcat(val_name, "alter_aim_hud_offset_pos", _prefix);
		file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eAimAlt]);
		xr_strconcat(val_name, "alter_aim_hud_offset_rot", _prefix);
		file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eAimAlt]);
	
		xr_strconcat(val_name, "safemode_hud_offset_pos", _prefix);
		file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eSafemode]);
		xr_strconcat(val_name, "safemode_hud_offset_rot", _prefix);
		file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eSafemode]);
	
		xr_strconcat(val_name, "collision_hud_offset_pos", _prefix);
		file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisPos][EHudOffsetType::eCollision]);
		xr_strconcat(val_name, "collision_hud_offset_rot", _prefix);
		file.w_fvector3(sect, val_name, p_item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][EHudOffsetType::eCollision]);

		file.w_fvector3(sect, "collision_box_pos", p_item->m_measures.m_collision_params.obb_pos);
		file.w_fvector3(sect, "collision_box_scale", p_item->m_measures.m_collision_params.obb_scale);
		
		file.w_float(sect, "collision_stifness", p_item->m_measures.m_collision_params.stifness);
		file.w_float(sect, "collision_damping", p_item->m_measures.m_collision_params.damping);

		if (CWeapon* wpn = p_item->m_parent_hud_item ? p_item->m_parent_hud_item->cast_weapon() : nullptr)
		{
			SaveWeaponAttachFirePoints(file, wpn, true);
		}
	};
	
	if (p_hud_item_first)
	{
		writeParams(p_hud_item_first, file);
	}

	if (p_hud_item_second)
	{
		writeParams(p_hud_item_second, file);
	}

	{
		const char* section = g_player_hud->section_name().c_str();

		file.w_float(section, "watches_scale", g_player_hud->m_watches_scale);
		file.w_fvector3(section, "watches_pos", g_player_hud->m_watches_pos);
		file.w_fvector3(section, "watches_rot", g_player_hud->m_watches_rot);
	}

	GAME_NEWS_DATA news_data;
	news_data.m_type = GAME_NEWS_DATA::ENewsKind::eNews;
	news_data.news_caption = "Saved result to:";
	news_data.news_text = fn;
	news_data.show_time = 5000;
	news_data.texture_name = "ui_iconsTotal_bar_darklab_documents2";
	Actor()->AddGameNews(news_data);
}

static bool hud_adj_show_golden_ratio = false;
static ImVec4 hud_adj_golden_ratio_color = ImVec4(0.0f, 0.47f, 0.84f, 0.85f);
static float hud_adj_golden_ratio_thickness = 1.5f;
static int hud_adj_golden_ratio_depth = 3;
static bool hud_adj_golden_ratio_flip_h = false;
static bool hud_adj_golden_ratio_flip_v = false;
static bool hud_adj_golden_ratio_show_rects = true;
static bool hud_adj_golden_ratio_show_circles = false;
static bool hud_adj_golden_ratio_fit_screen = true;

static bool hud_adj_show_center_lines = false;
static ImVec4 hud_adj_center_lines_color = ImVec4(1.0f, 0.2f, 0.2f, 0.8f);
static float hud_adj_center_lines_thickness = 1.0f;

static void HudAdjustRenderOverlays()
{
	if (!hud_adj_show_golden_ratio && !hud_adj_show_center_lines)
		return;

	// Over the game's picture: the Game window's image, or where the view is shown in the main window
	const CImGuiManager::GameViewRect Picture = CImGuiManager::Instance().GetGameViewRect();
	const float screen_w = Picture.Size.x;
	const float screen_h = Picture.Size.y;

	if (screen_w <= 0.0f || screen_h <= 0.0f)
		return;

	ImDrawList* draw_list = ImGui::GetForegroundDrawList(CImGuiManager::Instance().GetGameViewport());
	if (!draw_list)
		return;

	draw_list->PushClipRect(Picture.Pos, ImVec2(Picture.Pos.x + screen_w, Picture.Pos.y + screen_h));

	if (hud_adj_show_golden_ratio)
	{
		const ImU32 col = ImGui::ColorConvertFloat4ToU32(hud_adj_golden_ratio_color);
		const ImU32 col_sub = (col & 0x00FFFFFF) | ((u32)(((hud_adj_golden_ratio_color.w * 0.4f) * 255.0f)) << 24);

		float start_x = 0.0f;
		float start_y = 0.0f;
		float rect_w = screen_w;
		float rect_h = screen_h;

		if (!hud_adj_golden_ratio_fit_screen)
		{
			const float phi = 1.618033988749895f;
			rect_h = screen_w / phi;
			if (rect_h > screen_h)
			{
				rect_h = screen_h;
				rect_w = screen_h * phi;
			}
			start_x = (screen_w - rect_w) * 0.5f;
			start_y = (screen_h - rect_h) * 0.5f;
		}

		float cur_x = start_x;
		float cur_y = start_y;
		float cur_w = rect_w;
		float cur_h = rect_h;

		auto transform_pt = [&](float px, float py) -> ImVec2
		{
			float nx = px;
			float ny = py;
			if (hud_adj_golden_ratio_flip_h)
				nx = screen_w - nx;
			if (hud_adj_golden_ratio_flip_v)
				ny = screen_h - ny;
			return ImVec2(Picture.Pos.x + nx, Picture.Pos.y + ny);
		};

		auto draw_transformed_line = [&](ImVec2 p1, ImVec2 p2, ImU32 lcol, float th)
		{
			draw_list->AddLine(transform_pt(p1.x, p1.y), transform_pt(p2.x, p2.y), lcol, th);
		};

		if (hud_adj_golden_ratio_show_rects)
		{
			draw_transformed_line(ImVec2(start_x, start_y), ImVec2(start_x + rect_w, start_y), col, hud_adj_golden_ratio_thickness);
			draw_transformed_line(ImVec2(start_x + rect_w, start_y), ImVec2(start_x + rect_w, start_y + rect_h), col, hud_adj_golden_ratio_thickness);
			draw_transformed_line(ImVec2(start_x + rect_w, start_y + rect_h), ImVec2(start_x, start_y + rect_h), col, hud_adj_golden_ratio_thickness);
			draw_transformed_line(ImVec2(start_x, start_y + rect_h), ImVec2(start_x, start_y), col, hud_adj_golden_ratio_thickness);
		}

		const float phi_inv = 0.618033988749895f;

		int orientation = 0;
		for (int i = 0; i < hud_adj_golden_ratio_depth; ++i)
		{
			if (cur_w < 1.0f || cur_h < 1.0f)
				break;

			float s_w = 0.0f;
			float s_h = 0.0f;
			ImVec2 arc_center(0.0f, 0.0f);
			float a_min = 0.0f;
			float a_max = 0.0f;
			ImVec2 sq_min, sq_max;

			if (orientation == 0)
			{
				s_w = hud_adj_golden_ratio_fit_screen ? (cur_w * phi_inv) : cur_h;
				s_h = cur_h;
				sq_min = ImVec2(cur_x, cur_y);
				sq_max = ImVec2(cur_x + s_w, cur_y + s_h);
				arc_center = ImVec2(cur_x + s_w, cur_y + s_h);
				a_min = M_PI;
				a_max = 1.5f * M_PI;

				if (hud_adj_golden_ratio_show_rects && i > 0)
					draw_transformed_line(ImVec2(cur_x + s_w, cur_y), ImVec2(cur_x + s_w, cur_y + s_h), col, hud_adj_golden_ratio_thickness);

				cur_x += s_w;
				cur_w -= s_w;
			}
			else if (orientation == 1)
			{
				s_w = cur_w;
				s_h = hud_adj_golden_ratio_fit_screen ? (cur_h * phi_inv) : cur_w;
				sq_min = ImVec2(cur_x, cur_y);
				sq_max = ImVec2(cur_x + s_w, cur_y + s_h);
				arc_center = ImVec2(cur_x, cur_y + s_h);
				a_min = 1.5f * M_PI;
				a_max = 2.0f * M_PI;

				if (hud_adj_golden_ratio_show_rects)
					draw_transformed_line(ImVec2(cur_x, cur_y + s_h), ImVec2(cur_x + s_w, cur_y + s_h), col, hud_adj_golden_ratio_thickness);

				cur_y += s_h;
				cur_h -= s_h;
			}
			else if (orientation == 2)
			{
				s_w = hud_adj_golden_ratio_fit_screen ? (cur_w * phi_inv) : cur_h;
				s_h = cur_h;
				sq_min = ImVec2(cur_x + cur_w - s_w, cur_y);
				sq_max = ImVec2(cur_x + cur_w, cur_y + s_h);
				arc_center = ImVec2(cur_x + cur_w - s_w, cur_y);
				a_min = 0.0f;
				a_max = 0.5f * M_PI;

				if (hud_adj_golden_ratio_show_rects)
					draw_transformed_line(ImVec2(cur_x + cur_w - s_w, cur_y), ImVec2(cur_x + cur_w - s_w, cur_y + s_h), col, hud_adj_golden_ratio_thickness);

				cur_w -= s_w;
			}
			else
			{
				s_w = cur_w;
				s_h = hud_adj_golden_ratio_fit_screen ? (cur_h * phi_inv) : cur_w;
				sq_min = ImVec2(cur_x, cur_y + cur_h - s_h);
				sq_max = ImVec2(cur_x + s_w, cur_y + cur_h);
				arc_center = ImVec2(cur_x + s_w, cur_y + cur_h - s_h);
				a_min = 0.5f * M_PI;
				a_max = M_PI;

				if (hud_adj_golden_ratio_show_rects)
					draw_transformed_line(ImVec2(cur_x, cur_y + cur_h - s_h), ImVec2(cur_x + s_w, cur_y + cur_h - s_h), col, hud_adj_golden_ratio_thickness);

				cur_h -= s_h;
			}

			if (hud_adj_golden_ratio_show_circles)
			{
				ImVec2 sq_center((sq_min.x + sq_max.x) * 0.5f, (sq_min.y + sq_max.y) * 0.5f);
				float rx = s_w * 0.5f;
				float ry = s_h * 0.5f;
				const int el_segs = 40;
				ImVec2 prev_el;
				for (int k = 0; k <= el_segs; ++k)
				{
					float ea = (float)k / (float)el_segs * 2.0f * M_PI;
					ImVec2 el_pt = transform_pt(sq_center.x + rx * cosf(ea), sq_center.y + ry * sinf(ea));
					if (k > 0)
						draw_list->AddLine(prev_el, el_pt, col_sub, hud_adj_golden_ratio_thickness);
					prev_el = el_pt;
				}
			}

			const int segs = 20;
			ImVec2 prev_pt;
			for (int j = 0; j <= segs; ++j)
			{
				float a = a_min + (a_max - a_min) * ((float)j / (float)segs);
				float px = arc_center.x + s_w * cosf(a);
				float py = arc_center.y + s_h * sinf(a);
				ImVec2 pt = transform_pt(px, py);
				if (j > 0)
				{
					draw_list->AddLine(prev_pt, pt, col, hud_adj_golden_ratio_thickness * 1.5f);
				}
				prev_pt = pt;
			}

			orientation = (orientation + 1) % 4;
		}
	}

	if (hud_adj_show_center_lines)
	{
		const ImU32 col = ImGui::ColorConvertFloat4ToU32(hud_adj_center_lines_color);
		const float cx = screen_w * 0.5f;
		const float cy = screen_h * 0.5f;

		draw_list->AddLine(ImVec2(Picture.Pos.x + cx, Picture.Pos.y), ImVec2(Picture.Pos.x + cx, Picture.Pos.y + screen_h), col, hud_adj_center_lines_thickness);
		draw_list->AddLine(ImVec2(Picture.Pos.x, Picture.Pos.y + cy), ImVec2(Picture.Pos.x + screen_w, Picture.Pos.y + cy), col, hud_adj_center_lines_thickness);
	}

	draw_list->PopClipRect();
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

	ImGui::SeparatorText("Screen Overlay Guides");
	ImGui::Checkbox("Show golden spiral", &hud_adj_show_golden_ratio);
	if (hud_adj_show_golden_ratio)
	{
		ImGui::ColorEdit4("Spiral color", (float*)&hud_adj_golden_ratio_color, ImGuiColorEditFlags_AlphaBar);
		ImGui::SetNextItemWidth(80.0f);
		ImGui::SliderFloat("Spiral thickness", &hud_adj_golden_ratio_thickness, 0.5f, 5.0f, "%.1f");
		ImGui::SameLine();
		ImGui::SetNextItemWidth(80.0f);
		ImGui::SliderInt("Depth", &hud_adj_golden_ratio_depth, 1, 9);
		ImGui::Checkbox("Fit to screen", &hud_adj_golden_ratio_fit_screen);
		ImGui::SameLine();
		ImGui::Checkbox("Flip horizontal", &hud_adj_golden_ratio_flip_h);
		ImGui::SameLine();
		ImGui::Checkbox("Flip vertical", &hud_adj_golden_ratio_flip_v);
		ImGui::Checkbox("Show rectangles", &hud_adj_golden_ratio_show_rects);
		ImGui::SameLine();
		ImGui::Checkbox("Show circles", &hud_adj_golden_ratio_show_circles);
	}

	ImGui::Checkbox("Show center lines", &hud_adj_show_center_lines);
	if (hud_adj_show_center_lines)
	{
		ImGui::ColorEdit4("Center lines color", (float*)&hud_adj_center_lines_color, ImGuiColorEditFlags_AlphaBar);
		ImGui::SetNextItemWidth(80.0f);
		ImGui::SliderFloat("Center line thickness", &hud_adj_center_lines_thickness, 0.5f, 5.0f, "%.1f");
	}

	ImGui::Separator();
	ImGui::SetNextItemWidth(80.0f);
	ImGui::InputFloat("Position step", &_delta_pos, 0.0f, 0.0f, "%.6f");
	ImGui::SetNextItemWidth(80.0f);
	ImGui::InputFloat("Rotation step", &_delta_rot, 0.0f, 0.0f, "%.6f");
}

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

	if (g_player_hud->m_watches_model != nullptr)
	{
		if (ImGui::CollapsingHeader("Watches Settings"))
		{
			ImGui::InputFloat("Watches Scale", &g_player_hud->m_watches_scale, 0.0f, 0.0f, "%.6f");

			ImGui::SeparatorText("Position##WS");

			if (ImGui::Button("Reset##WP"))
			{
				g_player_hud->m_watches_pos = pSettings->read_if_exists<Fvector3>(g_player_hud->section_name(), "watches_pos", zero_vel);
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
				g_player_hud->m_watches_rot = pSettings->read_if_exists<Fvector3>(g_player_hud->section_name(), "watches_rot", zero_vel);
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
ICF void HudAdjustDragAngleDeg(const char* label, float* v, float v_speed = 1.0f, float v_limit = 360.f, const char* format = "%.0f")
{
	float max_limit = v_limit * 10.f;
	ImGui::DragFloat(label, v, v_speed, -max_limit, max_limit, format);

	// нормализация в [-v_limit/2, +v_limit/2), иначе -1 превращается в 359 и смещения интерполируются через полный оборот
	*v -= v_limit * floorf(*v / v_limit + 0.5f);
}
ICF void HudAdjustDragAngleRad(const char* label, float* v, float v_speed = 1.0f, float v_limit = PI_MUL_2, const char* format = "%.0f")
{
	float max_limit = v_limit * 10.f;
	ImGui::DragFloat(label, v, v_speed, -max_limit, max_limit, format);

	// нормализация в [-v_limit/2, +v_limit/2)
	*v -= v_limit * floorf(*v / v_limit + 0.5f);
}
static void HudAdjustDrawItemSettings(attachable_hud_item* item)
{
	if (item == nullptr)
	{
		return;
	}

	ImGui::PushID(item);
	if (!ImGui::CollapsingHeader(*shared_str().printf("Item: %s", item->m_sect_name.c_str())))
	{
		ImGui::PopID();
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

	if (!item->m_model_combined && CCameraRig::Enabled())
	{
		ImGui::Checkbox("Hand IK", &item->IkEnabled);
		ImGui::SetItemTooltip("hud_ik: off = hud offsets (aim, safemode, collision...) move the whole HUD instead of the weapon through arm IK; sway, grip, draw and holster keep IK");
	}

	firedeps fd = {};
	item->setup_firedeps(fd);

	auto boneCombo = [item](const char* label, u16& bone_id, bool allow_none) -> void
	{
		shared_str bone = bone_id != BI_NONE ? item->m_model->LL_BoneName_dbg(bone_id) : nullptr;
		if (ImGuiBoneCombo(label, item->m_model, bone, allow_none))
		{
			bone_id = bone.size() ? item->m_model->LL_BoneID(bone) : BI_NONE;
		}
	};

	auto iniBone = [item](const char* line) -> u16
	{
		return pSettings->line_exist(item->m_sect_name, line) ? item->m_model->LL_BoneID(pSettings->r_string(item->m_sect_name, line)) : BI_NONE;
	};

	CWeapon* wpn = item->m_parent_hud_item ? item->m_parent_hud_item->cast_weapon() : nullptr;
	CInventoryItem* muzzle = wpn ? WeaponFirePointAttach(wpn, false) : nullptr;
	CInventoryItem* launcher = wpn ? WeaponFirePointAttach(wpn, true) : nullptr;

	if (muzzle && ImGui::CollapsingHeader("Fire point"))
	{
		ImGuiAttachFirePoint(muzzle, true, _delta_pos);
	}
	else if (!muzzle && item->m_measures.m_prop_flags.test(item->m_measures.e_fire_point))
	{
		if (ImGui::CollapsingHeader("Fire point"))
		{
			ImGui::SeparatorText("Offset##FP");
	
			Fvector& position = item->m_measures.m_fire_point_offset;
			if (ImGui::Button("Reset##FPOffset"))
			{
				position = pSettings->read_if_exists<Fvector3>(item->m_sect_name, "fire_point", zero_vel);
				item->m_measures.m_fire_bone = iniBone("fire_bone");
			}

			boneCombo("Bone##FPB", item->m_measures.m_fire_bone, false);
	
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
	
	if (launcher && ImGui::CollapsingHeader("Fire point 2"))
	{
		ImGuiAttachFirePoint(launcher, true, _delta_pos);
	}
	else if (!launcher && item->m_measures.m_prop_flags.test(item->m_measures.e_fire_point2))
	{
		if (ImGui::CollapsingHeader("Fire point 2"))
		{
			ImGui::SeparatorText("Offset##FP2");
	
			Fvector& position = item->m_measures.m_fire_point2_offset;
			if (ImGui::Button("Reset##FP2Offset"))
			{
				position = pSettings->read_if_exists<Fvector3>(item->m_sect_name, "fire_point2", zero_vel);
				item->m_measures.m_fire_bone2 = iniBone("fire_bone2");
			}

			boneCombo("Bone##FP2B", item->m_measures.m_fire_bone2, true);
	
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
				position = pSettings->read_if_exists<Fvector3>(item->m_sect_name, "shell_point", zero_vel);
				item->m_measures.m_shell_bone = iniBone("shell_bone");
			}

			boneCombo("Bone##SPB", item->m_measures.m_shell_bone, true);
	
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
		ImGui::PushID(static_cast<int>(offset_type));
		ImGui::SeparatorText("Position##HUD");

		Fvector& position = offset_type ? item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisPos][offset_type] : item->m_measures.m_hands_attach_real[EHudOffsetAxis::eAxisPos];
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
				position = pSettings->read_if_exists<Fvector3>(item->m_sect_name, val_name, pSettings->read_if_exists<Fvector3>(item->m_sect_name, "zoom_offset", zero_vel));
				break;
			}
			case EHudOffsetType::eAimGL:
			{
				xr_strconcat(val_name, "gl_hud_offset_pos", _prefix);
				position = pSettings->read_if_exists<Fvector3>(item->m_sect_name, val_name, pSettings->read_if_exists<Fvector3>(item->m_sect_name, "grenade_zoom_offset", zero_vel));
				break;
			}
			case EHudOffsetType::eAimAlt:
			{
				xr_strconcat(val_name, "alter_aim_hud_offset_pos", _prefix);
				position = pSettings->read_if_exists<Fvector3>(item->m_sect_name, val_name, zero_vel);
				break;
			}
			case EHudOffsetType::eSafemode:
			{
				xr_strconcat(val_name, "safemode_hud_offset_pos", _prefix);
				position = pSettings->read_if_exists<Fvector3>(item->m_sect_name, val_name, zero_vel);
				break;
			}
			case EHudOffsetType::eCollision:
			{
				xr_strconcat(val_name, "collision_hud_offset_pos", _prefix);
				position = pSettings->read_if_exists<Fvector3>(item->m_sect_name, val_name, zero_vel);
				break;
			}
			default:
			{
				xr_strconcat(val_name, "hands_position", _prefix);
				position = pSettings->read_if_exists<Fvector3>(item->m_sect_name, val_name, zero_vel);
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

		Fvector& rotation = offset_type ? item->m_measures.m_hands_positions.hands_offsets[EHudOffsetAxis::eAxisRot][offset_type] : item->m_measures.m_hands_attach_real[EHudOffsetAxis::eAxisRot];
		xr_sprintf(btnName, "Reset##HRotation_%d", (u8)offset_type);
		if (ImGui::Button(btnName))
		{
			switch (offset_type)
			{
			case EHudOffsetType::eAim:
			{
				xr_strconcat(val_name, "aim_hud_offset_rot", _prefix);
				if (pSettings->line_exist(item->m_sect_name, val_name))
				{
					rotation = pSettings->r_fvector3(item->m_sect_name, val_name);
				}
				else
				{
					rotation.x = pSettings->read_if_exists<float>(item->m_sect_name, "zoom_rotate_x", 0.f);
					rotation.y = pSettings->read_if_exists<float>(item->m_sect_name, "zoom_rotate_y", 0.f);
					rotation.z = pSettings->read_if_exists<float>(item->m_sect_name, "zoom_rotate_z", 0.f);
				}
				break;
			}
			case EHudOffsetType::eAimGL:
			{
				xr_strconcat(val_name, "gl_hud_offset_rot", _prefix);
				if (pSettings->line_exist(item->m_sect_name, val_name))
				{
					rotation = pSettings->r_fvector3(item->m_sect_name, val_name);
				}
				else
				{
					rotation.x = pSettings->read_if_exists<float>(item->m_sect_name, "grenade_zoom_rotate_x", 0.f);
					rotation.y = pSettings->read_if_exists<float>(item->m_sect_name, "grenade_zoom_rotate_y", 0.f);
					rotation.z = pSettings->read_if_exists<float>(item->m_sect_name, "grenade_zoom_rotate_z", 0.f);
				}
				break;
			}
			case EHudOffsetType::eAimAlt:
			{
				xr_strconcat(val_name, "alter_aim_hud_offset_rot", _prefix);
				rotation = pSettings->read_if_exists<Fvector3>(item->m_sect_name, val_name, zero_vel);
				break;
			}
			case EHudOffsetType::eSafemode:
			{
				xr_strconcat(val_name, "safemode_hud_offset_rot", _prefix);
				rotation = pSettings->read_if_exists<Fvector3>(item->m_sect_name, val_name, zero_vel);
				break;
			}
			case EHudOffsetType::eCollision:
			{
				xr_strconcat(val_name, "collision_hud_offset_rot", _prefix);
				rotation = pSettings->read_if_exists<Fvector3>(item->m_sect_name, val_name, zero_vel);
				break;
			}
			default:
			{
				xr_strconcat(val_name, "hands_orientation", _prefix);
				rotation = pSettings->read_if_exists<Fvector3>(item->m_sect_name, val_name, zero_vel);
				break;
			}
			}
		}

		if (ImGui::BeginTable("Data##HUDR", 1))
		{
			ImGui::TableNextRow();

			ImGui::TableNextColumn();

			HudAdjustDragAngleDeg("X##HUDR", &rotation.x, _delta_rot, 360.f, "%.6f");
			HudAdjustDragAngleDeg("Y##HUDR", &rotation.y, _delta_rot, 360.f, "%.6f");
			HudAdjustDragAngleDeg("Z##HUDR", &rotation.z, _delta_rot, 360.f, "%.6f");

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

		ImGui::PopID();
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
		Fvector& position = item->m_measures.m_item_attach[EHudOffsetAxis::eAxisPos];

		string64 _prefix = {};
		xr_sprintf(_prefix, "%s", UI().is_widescreen() ? "_16x9" : "");
		string128 val_name = {};

		if (ImGui::Button("Reset##IPosition"))
		{
			position = pSettings->read_if_exists<Fvector3>(item->m_sect_name, "item_position", pSettings->r_fvector3(item->m_sect_name, "position"));
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
		Fvector& rotation = item->m_measures.m_item_attach[EHudOffsetAxis::eAxisRot];
		if (ImGui::Button("Reset##IRotation"))
		{
			rotation = pSettings->read_if_exists<Fvector3>(item->m_sect_name, "item_orientation", pSettings->r_fvector3(item->m_sect_name, "orientation"));
		}

		if (ImGui::BeginTable("Data##HUDR", 1))
		{
			ImGui::TableNextRow();

			ImGui::TableNextColumn();

			HudAdjustDragAngleDeg("X##HUDR", &rotation.x, _delta_rot, 360.f, "%.6f");
			HudAdjustDragAngleDeg("Y##HUDR", &rotation.y, _delta_rot, 360.f, "%.6f");
			HudAdjustDragAngleDeg("Z##HUDR", &rotation.z, _delta_rot, 360.f, "%.6f");

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
				position.x = pSettings->read_if_exists<float>(lt->Section, "torch_attach_offset_x", 0.0f);
				position.y = pSettings->read_if_exists<float>(lt->Section, "torch_attach_offset_y", 0.0f);
				position.z = pSettings->read_if_exists<float>(lt->Section, "torch_attach_offset_z", 0.0f);
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
				position.x = pSettings->read_if_exists<float>(ll->Section, "laserdot_attach_offset_x", 0.0f);
				position.y = pSettings->read_if_exists<float>(ll->Section, "laserdot_attach_offset_y", 0.0f);
				position.z = pSettings->read_if_exists<float>(ll->Section, "laserdot_attach_offset_z", 0.0f);
	
				position = pSettings->read_if_exists<Fvector3>(ll->Section, "laserdot_attach_offset", position);
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
				angle = pSettings->read_if_exists<Fvector2>(ll->Section, "laser_spot_angle", angle.set(2, 5));
				angle.mul(M_PI / 180);
			}
	
			if (ImGui::BeginTable("Data##TLAngle", 1))
			{
				ImGui::TableNextRow();
	
				ImGui::TableNextColumn();
	
				HudAdjustDragAngleRad("X##TLAngle", &angle.x, _delta_rot, PI_MUL_2, "%.6f");
				HudAdjustDragAngleRad("Y##TLAngle", &angle.y, _delta_rot, PI_MUL_2, "%.6f");
				ImGui::TableNextColumn();
	
				ImGui::EndTable();
			}
		}
	}

	ImGui::PopID();
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
		HudAdjustDrawHandsSettings();
		HudAdjustDrawItemSettings(g_player_hud->attached_item(0));
		HudAdjustDrawItemSettings(g_player_hud->attached_item(1));
	}

	ImGui::End();
	ImGui::EndDisabled();
	ImGui::PopStyleColor(1);

	HudAdjustRenderOverlays();
}

void RenderWatchDebugWindow()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_WatchDebug)])
	{
		return;
	}

	if (!g_pGameLevel || !g_player_hud)
	{
		return;
	}

	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.0f, 0.0f, 0.0f, kGeneralAlphaLevelForImGuiWindows));
	if (ImGui::Begin("Watch Debug", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_WatchDebug)]))
	{
		g_player_hud->WatchDevice().DrawImGui();
	}

	ImGui::End();
	ImGui::PopStyleColor(1);
}
