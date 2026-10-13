#include "StdAfx.h"
#include "ImUtils.h"

#include "../Actor.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../ui/UICellItem.h"
#include "../ui/UIDragDropListEx.h"

#include "../../xrCore/FS.h"
#include "../../xrCore/EngineExternal.h"
#include "../../xrUI/UI3dIconCache.h"

void Render3DIconLightingControls()
{
	if (!ImGui::CollapsingHeader("DX11 icon lighting"))
	{
		return;
	}
	ImGui::Text("Shared icon models: %zu", UI3dIconCache::Instance().GetModelCount());
	auto* Settings = Device.m_pRender->GetUIIconLighting();
	if (!Settings)
	{
		ImGui::TextDisabled("Lighting controls require renderer_r4.");
		return;
	}
	ImGui::ColorEdit3("Light color", &Settings->Color.x);
	ImGui::DragFloat("Intensity", &Settings->Intensity, 0.02f, 0.0f, 16.0f);
	ImGui::DragFloat("Ambient", &Settings->Ambient, 0.02f, 0.0f, 8.0f);
	ImGui::DragFloat3("Direction", &Settings->Direction.x, 0.01f, -1.0f, 1.0f);
	ImGui::SetItemTooltip("View-space direction. A zero vector uses the default (0, 0, 1).");
	if (ImGui::Button("Reset icon lighting"))
	{
		*Settings = UIIconLighting{};
	}
	ImGui::TextDisabled("Applies to all 3D icons immediately. Settings last until restart.");
}

extern bool g_Adjust3dIcon;
extern float g_Adjust3dIconValue;

namespace
{
	constexpr const char* k3dIconsOutputPath = "3d_icons\\adjusted.ltx";

	const char* safe_c_str(const shared_str& value)
	{
		return value.c_str() ? value.c_str() : "";
	}

	xr_string translate_to_utf8(const char* text)
	{
		if (!text || !text[0])
		{
			return {};
		}

		const shared_str translated = g_pStringTable->translate(text);
		const char* value = safe_c_str(translated);

		if (IsUTF8(value))
		{
			return value;
		}

		return Platform::ANSI_TO_UTF8(value);
	}

	shared_str item_name_key(const CInventoryItem* item)
	{
		const shared_str extended = item->GetExtendedUnionName();
		return extended.size() ? extended : item->m_name;
	}

	char lower_ascii(char value)
	{
		return (value >= 'A' && value <= 'Z') ? static_cast<char>(value - 'A' + 'a') : value;
	}

	bool contains_ci(const char* haystack, const char* needle)
	{
		if (!needle || !needle[0])
		{
			return true;
		}

		if (!haystack)
		{
			return false;
		}

		for (; *haystack; ++haystack)
		{
			const char* h = haystack;
			const char* n = needle;

			while (*h && *n && lower_ascii(*h) == lower_ascii(*n))
			{
				++h;
				++n;
			}

			if (!*n)
			{
				return true;
			}
		}

		return false;
	}

	struct S3dIconAdjustRecord
	{
		Fvector rotate_deg{};
		float scale = 1.0f;
		shared_str visual;
		Fvector2 Offset{};
		Ivector2 GridSize{1, 1};
	};

	class CImGui3dIconAdjust
	{
	public:
		void notify_hovered(CUICellItem* cell) { m_hovered_cell = cell; }

		void register_cell(CUICellItem* cell)
		{
			m_cells.push_back(cell);
		}

		void unregister_cell(CUICellItem* cell)
		{
			if (m_hovered_cell == cell)
			{
				m_hovered_cell = nullptr;
			}

			if (m_target_cell == cell)
			{
				m_target_cell = nullptr;
			}

			for (auto it = m_cells.begin(); it != m_cells.end(); ++it)
			{
				if (*it == cell)
				{
					m_cells.erase(it);
					break;
				}
			}
		}

		void consume_hovered()
		{
			if (m_follow_cursor && m_hovered_cell)
			{
				select(m_hovered_cell);
			}

			m_hovered_cell = nullptr;
		}

		void draw();

	private:
		void select(CUICellItem* cell)
		{
			if (!cell)
			{
				return;
			}

			set_target(static_cast<CInventoryItem*>(cell->m_pData), cell);
		}

		void select_item(CInventoryItem* item)
		{
			if (!item)
			{
				return;
			}

			set_target(item, nullptr);
			m_target_cell = find_cell_matching_target();
		}

		void set_target(CInventoryItem* item, CUICellItem* cell)
		{
			if (!item)
			{
				return;
			}

			const bool same = m_target_item && item_matches_target(item);

			m_target_item = item;
			m_target_cell = cell;
			m_target_section = item->m_section_id;

			if (!same)
			{
				reload();
			}
		}

		void release()
		{
			m_target_cell = nullptr;
			m_target_item = nullptr;
			m_target_section = nullptr;
		}

		bool item_matches_target(const CInventoryItem* item) const
		{
			return item && item->m_section_id == m_target_section;
		}

		CInventoryItem* find_item_matching_target() const
		{
			if (!g_actor || !m_target_section.size())
			{
				return nullptr;
			}

			for (CInventoryItem* item : g_actor->inventory().m_all)
			{
				if (item_matches_target(item))
				{
					return item;
				}
			}

			return nullptr;
		}

		CUICellItem* find_cell_matching_target() const
		{
			for (CUICellItem* cell : m_cells)
			{
				if (cell && item_matches_target(static_cast<CInventoryItem*>(cell->m_pData)))
				{
					return cell;
				}
			}

			return nullptr;
		}

		void collect_target_items()
		{
			m_target_items.clear();

			if (!m_target_item)
			{
				return;
			}

			if (g_actor)
			{
				for (CInventoryItem* item : g_actor->inventory().m_all)
				{
					if (item_matches_target(item))
					{
						m_target_items.push_back(item);
					}
				}
			}

			if (m_target_items.empty())
			{
				m_target_items.push_back(m_target_item);
			}
		}

		void update_cells_rotation_scale()
		{
			Fvector rotate = m_target_item->m_3d_static_rotate;

			for (CUICellItem* cell : m_cells)
			{
				if (cell && item_matches_target(static_cast<CInventoryItem*>(cell->m_pData)))
				{
					cell->SetXYZ(rotate);
					cell->SetScaleFactor(m_scale);
					cell->SetIconOffset(Offset);
				}
			}
		}

		void update_cells_visual()
		{
			for (CUICellItem* cell : m_cells)
			{
				if (cell && item_matches_target(static_cast<CInventoryItem*>(cell->m_pData)))
				{
					cell->SetIconVisual(m_visual_applied);
					auto* Item = static_cast<CInventoryItem*>(cell->m_pData);
					cell->SetBonesVisible(Item->object().Visual()->dcast_PKinematics());
				}
			}
		}

		void validate_target()
		{
			if (!m_target_item || !g_actor)
			{
				return;
			}

			if (CInventoryItem* item = find_item_matching_target())
			{
				m_target_item = item;
				return;
			}

			if (find_cell_matching_target())
			{
				return;
			}

			release();
		}

		void reload()
		{
			m_rotate_deg.set(
				rad2deg(m_target_item->m_3d_static_rotate.x),
				rad2deg(m_target_item->m_3d_static_rotate.y),
				rad2deg(m_target_item->m_3d_static_rotate.z));
			m_scale = m_target_item->m_3d_static_scale;
			Offset = m_target_item->IconOffset;
			GridSize = m_target_item->GetInvGridRect().rb;
			GridSizeBlocked = false;
			m_visual_applied = m_target_item->m_3d_static_visual_name;
			xr_strcpy(m_visual, sizeof(m_visual), safe_c_str(m_visual_applied));
		}

		void apply_rotation_scale()
		{
			if (!m_target_item)
			{
				return;
			}

			collect_target_items();

			for (CInventoryItem* item : m_target_items)
			{
				item->m_3d_static_rotate.set(deg2rad(m_rotate_deg.x), deg2rad(m_rotate_deg.y), deg2rad(m_rotate_deg.z));
				item->m_3d_static_scale = m_scale;
				item->IconOffset = Offset;
			}

			update_cells_rotation_scale();
			update_record();
		}

		bool ApplyGridSize()
		{
			const Ivector2 OldSize = m_target_item->GetInvGridRect().rb;
			if (OldSize.x == GridSize.x && OldSize.y == GridSize.y)
			{
				GridSizeBlocked = false;
				return true;
			}
			xr_vector<xr_pair<CUICellItem*, Ivector2>> Resized;
			for (CUICellItem* Cell : m_cells)
			{
				if (!Cell || !item_matches_target(static_cast<CInventoryItem*>(Cell->m_pData)))
				{
					continue;
				}
				auto* List = Cell->OwnerList();
				if (!Cell->IsOwnerListValid() || !List->IsOwner(Cell))
				{
					continue;
				}
				const Ivector2 PreviousSize = Cell->GetGridSize();
				if (!List->TryResizeItem(Cell, GridSize))
				{
					for (auto It = Resized.rbegin(); It != Resized.rend(); ++It)
					{
						It->first->OwnerList()->TryResizeItem(It->first, It->second);
					}
					GridSize = OldSize;
					GridSizeBlocked = true;
					return false;
				}
				Resized.push_back({Cell, PreviousSize});
			}
			collect_target_items();
			for (CInventoryItem* Item : m_target_items)
			{
				Irect Rect = Item->GetInvGridRect();
				Rect.rb = GridSize;
				Item->SetInvGridRect(Rect);
			}
			GridSizeBlocked = false;
			for (CUICellItem* Cell : m_cells)
			{
				if (Cell && !Cell->OwnerList() && item_matches_target(static_cast<CInventoryItem*>(Cell->m_pData)))
				{
					const Ivector2 Previous = Cell->GetGridSize();
					Cell->SetWndSize(Fvector2().set(Cell->GetWidth() * GridSize.x / Previous.x, Cell->GetHeight() * GridSize.y / Previous.y));
					Cell->SetGridSize(GridSize);
				}
			}
			update_record();
			return true;
		}

		void apply_visual()
		{
			if (!m_target_item)
			{
				return;
			}

			if (xr_strcmp(safe_c_str(m_visual_applied), m_visual) == 0)
			{
				return;
			}

			m_visual_applied = m_visual;
			collect_target_items();

			for (CInventoryItem* item : m_target_items)
			{
				item->m_3d_static_visual_name = safe_c_str(m_visual_applied);
			}

			update_cells_visual();
			update_record();
		}

		void reset_to_config()
		{
			if (!m_target_item)
			{
				return;
			}
			auto w = READ_IF_EXISTS(pSettings, r_u32, safe_c_str(m_target_section), "inv_grid_width", 1);
			auto h = READ_IF_EXISTS(pSettings, r_u32, safe_c_str(m_target_section), "inv_grid_height", 1);
			GridSize.set((int)w, (int)h);
			if (!ApplyGridSize())
			{
				return;
			}
			collect_target_items();

			for (CInventoryItem* item : m_target_items)
			{
				item->Read3dStaticsData(safe_c_str(item->m_section_id));
			}

			reload();

			update_cells_visual();
			apply_rotation_scale();

			m_adjusted.erase(safe_c_str(m_target_section));
		}

		void update_record()
		{
			if (!m_target_item)
			{
				return;
			}

			S3dIconAdjustRecord& record = m_adjusted[safe_c_str(m_target_section)];
			record.rotate_deg = m_rotate_deg;
			record.scale = m_scale;
			record.visual = m_visual_applied;
			record.Offset = Offset;
			record.GridSize = GridSize;
		}

		const char* get_place_text(const CInventory& inventory, const CInventoryItem* item)
		{
			for (u16 slot = 0; slot < static_cast<u16>(inventory.m_slots.size()); ++slot)
			{
				if (inventory.m_slots[slot].m_pIItem == item)
				{
					xr_sprintf(m_place_text, "Slot %u", slot);
					return m_place_text;
				}
			}

			if (inventory.InBelt(item))
			{
				return "Belt";
			}

			if (inventory.InRuck(item))
			{
				return "Ruck";
			}

			return "-";
		}

		void draw_inventory_list();
		void log_values() const;
		void copy_to_clipboard() const;
		void save() const;

		void draw_help() const;

		xr_vector<CUICellItem*> m_cells;
		CUICellItem* m_hovered_cell = nullptr;
		CUICellItem* m_target_cell = nullptr;
		CInventoryItem* m_target_item = nullptr;
		shared_str m_target_section;
		xr_vector<CInventoryItem*> m_target_items;
		bool m_follow_cursor = true;

		Fvector m_rotate_deg{};
		float m_scale = 1.0f;
		Fvector2 Offset{};
		Ivector2 GridSize{1, 1};
		bool GridSizeBlocked = false;
		char m_visual[256] = {};
		char m_filter[128] = {};
		char m_place_text[32] = {};
		shared_str m_visual_applied;

		xr_map<xr_string, S3dIconAdjustRecord> m_adjusted;
		mutable xr_string m_last_saved_path;
	};

	void CImGui3dIconAdjust::draw_inventory_list()
	{
		if (!g_actor)
		{
			ImGui::TextWrapped("Inventory is not available.");
			return;
		}

		struct SInventoryListEntry
		{
			shared_str section;
			CInventoryItem* item = nullptr;
			xr_string name;
			u32 count = 0;
		};

		CInventory& inventory = g_actor->inventory();

		ImGui::SetNextItemWidth(-1.0f);
		ImGui::InputTextWithHint("##3d_icons_filter", "Filter by section or name", m_filter, sizeof(m_filter));

		xr_vector<SInventoryListEntry> items;
		xr_map<xr_string, u32> entry_by_key;

		for (CInventoryItem* item : inventory.m_all)
		{
			if (!item)
			{
				continue;
			}

			const xr_string translated_name = translate_to_utf8(safe_c_str(item_name_key(item)));

			if (!contains_ci(safe_c_str(item->m_section_id), m_filter) && !contains_ci(translated_name.c_str(), m_filter))
			{
				continue;
			}

			const xr_string key = safe_c_str(item->m_section_id);
			const auto existing = entry_by_key.find(key);

			if (existing != entry_by_key.end())
			{
				++items[existing->second].count;
				continue;
			}

			entry_by_key[key] = static_cast<u32>(items.size());

			SInventoryListEntry entry;
			entry.section = item->m_section_id;
			entry.item = item;
			entry.name = translated_name;
			entry.count = 1;

			items.push_back(std::move(entry));
		}

		std::sort(items.begin(), items.end(), [](const SInventoryListEntry& left, const SInventoryListEntry& right)
		{
			const int section_cmp = xr_strcmp(safe_c_str(left.section), safe_c_str(right.section));

			if (section_cmp != 0)
			{
				return section_cmp < 0;
			}

			return xr_strcmp(left.name.c_str(), right.name.c_str()) < 0;
		});

		if (!m_adjusted.empty())
		{
			ImGui::TextDisabled("Highlighted rows are modified");
		}

		if (ImGui::BeginTable("##3d_icons_inventory", 4, ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg | ImGuiTableFlags_ScrollY, ImVec2(0.0f, 180.0f)))
		{
			ImGui::TableSetupColumn("Section");
			ImGui::TableSetupColumn("Name");
			ImGui::TableSetupColumn("Count", ImGuiTableColumnFlags_WidthFixed, 48.0f);
			ImGui::TableSetupColumn("Place", ImGuiTableColumnFlags_WidthFixed, 64.0f);
			ImGui::TableSetupScrollFreeze(0, 1);
			ImGui::TableHeadersRow();

			for (const SInventoryListEntry& entry : items)
			{
				CInventoryItem* item = entry.item;
				const bool is_modified = m_adjusted.find(safe_c_str(entry.section)) != m_adjusted.end();

				ImGui::PushID(static_cast<int>(item->object_id()));
				ImGui::TableNextRow();

				if (is_modified)
				{
					ImGui::TableSetBgColor(ImGuiTableBgTarget_RowBg0, ImGui::GetColorU32(ImVec4(0.55f, 0.38f, 0.05f, 0.55f)));
					ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.0f, 0.82f, 0.35f, 1.0f));
				}

				ImGui::TableSetColumnIndex(0);

				if (ImGui::Selectable(safe_c_str(entry.section), item_matches_target(item), ImGuiSelectableFlags_SpanAllColumns))
				{
					m_follow_cursor = false;
					select_item(item);
				}

				if (is_modified)
				{
					ImGui::PopStyleColor();
				}

				ImGui::TableSetColumnIndex(1);
				ImGui::TextUnformatted(entry.name.c_str());

				if (is_modified)
				{
					ImGui::SameLine();
					ImGui::TextColored(ImVec4(1.0f, 0.82f, 0.35f, 1.0f), "*");
				}

				ImGui::TableSetColumnIndex(2);

				if (entry.count > 1)
				{
					ImGui::Text("x%u", entry.count);
				}
				else
				{
					ImGui::TextDisabled("-");
				}

				ImGui::TableSetColumnIndex(3);
				ImGui::TextUnformatted(get_place_text(inventory, item));

				ImGui::PopID();
			}

			ImGui::EndTable();
		}

		if (items.empty())
		{
			ImGui::TextWrapped("No items found.");
		}
	}

	void CImGui3dIconAdjust::log_values() const
	{
		if (!m_target_item)
		{
			return;
		}

		Msg("[%s]", safe_c_str(m_target_item->m_section_id));

		string256 line{};
		xr_sprintf(line, "3d_static_visual_name\t\t= %s", m_visual);
		Log(line);
		xr_sprintf(line, "3d_static_rotate\t\t\t= %f,%f,%f", m_rotate_deg.x, m_rotate_deg.y, m_rotate_deg.z);
		Log(line);
		xr_sprintf(line, "3d_static_scale\t\t\t= %f", m_scale);
		Log(line);
		xr_sprintf(line, "3d_static_offset\t\t= %f,%f", Offset.x, Offset.y);
		Log(line);
		xr_sprintf(line, "inv_grid_width\t\t\t= %d", GridSize.x);
		Log(line);
		xr_sprintf(line, "inv_grid_height\t\t\t= %d", GridSize.y);
		Log(line);
	}

	void CImGui3dIconAdjust::copy_to_clipboard() const
	{
		if (!m_target_item)
		{
			return;
		}

		string4096 text{};
		xr_sprintf(text, "[%s]\n"
			"3d_static_visual_name = %s\n"
			"3d_static_rotate = %.3f, %.3f, %.3f\n"
			"3d_static_scale = %.3f\n"
			"3d_static_offset = %.3f, %.3f\n"
			"inv_grid_width = %d\n"
			"inv_grid_height = %d",
			safe_c_str(m_target_item->m_section_id), m_visual,
			m_rotate_deg.x, m_rotate_deg.y, m_rotate_deg.z, m_scale, Offset.x, Offset.y, GridSize.x, GridSize.y);

		ImGui::SetClipboardText(text);
	}

	void CImGui3dIconAdjust::save() const
	{
		if (m_adjusted.empty())
		{
			return;
		}

		string_path path{};
		FS.update_path(path, "$app_data_root$", k3dIconsOutputPath);

		CInifile file(path, false, true, true);
		file.set_override_names(true);

		for (const auto& [section, record] : m_adjusted)
		{
			file.w_string(section.c_str(), "3d_static_visual_name", safe_c_str(record.visual));
			file.w_fvector3(section.c_str(), "3d_static_rotate", record.rotate_deg);
			file.w_float(section.c_str(), "3d_static_scale", record.scale);
			file.w_fvector2(section.c_str(), "3d_static_offset", record.Offset);
			file.w_u32(section.c_str(), "inv_grid_width", record.GridSize.x);
			file.w_u32(section.c_str(), "inv_grid_height", record.GridSize.y);
		}

		m_last_saved_path = path;
	}

	void CImGui3dIconAdjust::draw_help() const
	{
		if (ImGui::CollapsingHeader("Config parameters"))
		{
			ImGuiEditorUI_HelpBullet("3d_static_visual_name - model for the 3D icon. If missing, the world model (visual) is used.");
			ImGuiEditorUI_HelpBullet("3d_static_rotate - rotation by X, Y, Z in degrees.");
			ImGuiEditorUI_HelpBullet("3d_static_scale - icon size in the inventory.");
			ImGuiEditorUI_HelpBullet("3d_static_offset - X/Y displacement relative to icon width/height. Positive X moves right, positive Y moves down.");
			ImGuiEditorUI_HelpBullet("inv_grid_width / inv_grid_height - occupied inventory cells. inv_grid_x / inv_grid_y remain atlas coordinates.");
		}

		if (ImGui::CollapsingHeader("Keyboard (Adjust mode)"))
		{
			ImGui::BulletText("Z / X / C - rotate by X / Y / Z");
			ImGui::BulletText("V - change scale");
			ImGui::BulletText("B - log the values");
			ImGui::BulletText("Left Ctrl (hold) - reverse direction");
		}
	}

	void CImGui3dIconAdjust::draw()
	{
		validate_target();
		Render3DIconLightingControls();

		if (!EngineExternal()[EEngineExternalGame::Enable3DIcons])
		{
			ImGui::TextColored(ImVec4(1.0f, 0.6f, 0.1f, 1.0f), "Enable3DIcons = false (engine_external.ltx)");
			ImGui::Separator();
		}

		ImGui::Checkbox("Adjust mode (3d_icons_adjust)", &g_Adjust3dIcon);
		ImGui::SameLine();
		ImGui::SetNextItemWidth(120.0f);
		ImGui::InputFloat("Step (3d_icons_adjust_value)", &g_Adjust3dIconValue, 0.0f, 0.0f, "%.3f");

		if (g_Adjust3dIconValue < 0.0f)
		{
			g_Adjust3dIconValue = 0.0f;
		}

		if (g_Adjust3dIconValue > 10.0f)
		{
			g_Adjust3dIconValue = 10.0f;
		}

		ImGui::SeparatorText("Target");

		ImGui::Checkbox("Follow cursor", &m_follow_cursor);
		ImGui::SetItemTooltip("Select the item under the cursor automatically");

		if (m_target_item)
		{
			ImGui::SameLine();

			if (ImGui::SmallButton("Release##3d_icons"))
			{
				release();
			}

			ImGui::Text("Section: %s", safe_c_str(m_target_item->m_section_id));

			const xr_string translated_name = translate_to_utf8(safe_c_str(item_name_key(m_target_item)));
			ImGui::Text("Name: %s", translated_name.c_str());
		}

		ImGui::SeparatorText("Inventory");
		draw_inventory_list();

		if (!m_target_item)
		{
			ImGui::SeparatorText("Help");
			draw_help();
			return;
		}

		const float rotate_step = std::max(g_Adjust3dIconValue * 10.0f, 0.001f);
		const float scale_step = std::max(g_Adjust3dIconValue, 0.0001f);

		bool changed = false;

		ImGui::SeparatorText("Static visual");

		ImGui::SetNextItemWidth(-1.0f);
		ImGui::InputTextWithHint("##3d_static_visual_name", "3d_static_visual_name", m_visual, sizeof(m_visual));
		ImGui::SetItemTooltip("3d_static_visual_name - model for the 3D icon (ogf path)");

		const bool visual_dirty = xr_strcmp(safe_c_str(m_visual_applied), m_visual) != 0;

		if (ImGui::Button("Apply visual"))
		{
			apply_visual();
		}

		ImGui::SameLine();

		if (ImGui::Button("Reset to config"))
		{
			reset_to_config();
			changed = false;
		}

		if (visual_dirty)
		{
			ImGui::SameLine();
			ImGui::TextColored(ImVec4(1.0f, 0.8f, 0.2f, 1.0f), "* not applied");
		}

		ImGui::SeparatorText("Rotation (degrees)");

		changed |= ImGui::DragFloat("X##3d_static_rotate", &m_rotate_deg.x, rotate_step, -360.0f, 360.0f, "%.3f");
		changed |= ImGui::DragFloat("Y##3d_static_rotate", &m_rotate_deg.y, rotate_step, -360.0f, 360.0f, "%.3f");
		changed |= ImGui::DragFloat("Z##3d_static_rotate", &m_rotate_deg.z, rotate_step, -360.0f, 360.0f, "%.3f");

		ImGui::SeparatorText("Scale");

		changed |= ImGui::DragFloat("Scale##3d_static_scale", &m_scale, scale_step, 0.01f, 100.0f, "%.4f");

		ImGui::SeparatorText("Position inside icon");
		changed |= ImGui::DragFloat("X##3d_static_offset", &Offset.x, 0.005f, -1.0f, 1.0f, "%.3f", ImGuiSliderFlags_AlwaysClamp);
		changed |= ImGui::DragFloat("Y##3d_static_offset", &Offset.y, 0.005f, -1.0f, 1.0f, "%.3f", ImGuiSliderFlags_AlwaysClamp);
		ImGui::TextDisabled("Relative to icon size: 0.1 = 10%%. Positive X/Y moves right/down.");

		ImGui::SeparatorText("Inventory cells");
		bool GridChanged = ImGui::DragInt("inv_grid_width", &GridSize.x, 1.0f, 1, 64, "%d", ImGuiSliderFlags_AlwaysClamp);
		GridChanged |= ImGui::DragInt("inv_grid_height", &GridSize.y, 1.0f, 1, 64, "%d", ImGuiSliderFlags_AlwaysClamp);
		if (GridChanged)
		{
			ApplyGridSize();
		}
		if (GridSizeBlocked)
		{
			ImGui::TextWrapped("Cannot resize: stop dragging and free neighbouring cells or move the item away from the grid edge.");
		}

		if (changed)
		{
			apply_rotation_scale();
		}

		if (g_Adjust3dIcon && !ImGui::GetIO().WantTextInput && ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows))
		{
			const float direction = ImGui::GetIO().KeyCtrl ? -1.0f : 1.0f;
			const float value = direction * g_Adjust3dIconValue;

			bool hotkey_changed = false;

			if (ImGui::IsKeyPressed(ImGuiKey_Z)) { m_rotate_deg.x += value * 10.0f; hotkey_changed = true; }
			if (ImGui::IsKeyPressed(ImGuiKey_X)) { m_rotate_deg.y += value * 10.0f; hotkey_changed = true; }
			if (ImGui::IsKeyPressed(ImGuiKey_C)) { m_rotate_deg.z += value * 10.0f; hotkey_changed = true; }
			if (ImGui::IsKeyPressed(ImGuiKey_V)) { m_scale += value; hotkey_changed = true; }
			if (ImGui::IsKeyPressed(ImGuiKey_B)) { log_values(); }

			if (hotkey_changed)
			{
				apply_rotation_scale();
			}
		}

		ImGui::SeparatorText("Output");

		if (ImGui::Button("Log values")) { log_values(); }
		ImGui::SameLine();
		if (ImGui::Button("Copy config")) { copy_to_clipboard(); }
		ImGui::SameLine();
		if (ImGui::Button("Save to file")) { save(); }

		if (!m_last_saved_path.empty())
		{
			ImGui::TextWrapped("Saved: %s", m_last_saved_path.c_str());
		}

		if (!m_adjusted.empty())
		{
			ImGui::SeparatorText("Adjusted sections");

			if (ImGui::BeginTable("##3d_icons_adjusted", 2, ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg | ImGuiTableFlags_ScrollY, ImVec2(0.0f, 140.0f)))
			{
				ImGui::TableSetupColumn("Section");
				ImGui::TableSetupColumn("Values");
				ImGui::TableSetupScrollFreeze(0, 1);
				ImGui::TableHeadersRow();

				for (auto it = m_adjusted.begin(); it != m_adjusted.end();)
				{
					ImGui::TableNextRow();
					ImGui::TableSetColumnIndex(0);
					ImGui::TextUnformatted(it->first.c_str());
					ImGui::TableSetColumnIndex(1);
					ImGui::Text("%.1f, %.1f, %.1f | %.3f | offset %.3f, %.3f | %dx%d cells", it->second.rotate_deg.x, it->second.rotate_deg.y, it->second.rotate_deg.z, it->second.scale, it->second.Offset.x, it->second.Offset.y, it->second.GridSize.x, it->second.GridSize.y);

					ImGui::PushID(it->first.c_str());
					ImGui::SameLine();

					if (ImGui::SmallButton("x"))
					{
						it = m_adjusted.erase(it);
					}
					else
					{
						++it;
					}

					ImGui::PopID();
				}

				ImGui::EndTable();
			}

			ImGui::TextWrapped("Output file: $app_data_root$\\%s", k3dIconsOutputPath);
		}

		ImGui::SeparatorText("Help");
		draw_help();
	}

	CImGui3dIconAdjust g_icon3d_adjust;
}

void Render3DIconAdjust()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_3DIconAdjust)])
	{
		return;
	}

	g_icon3d_adjust.consume_hovered();

	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.0f, 0.0f, 0.0f, kGeneralAlphaLevelForImGuiWindows));

	if (!ImGui::Begin("3D Icons Adjust", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_3DIconAdjust)]))
	{
		ImGui::End();
		ImGui::PopStyleColor(1);
		return;
	}

	g_icon3d_adjust.draw();

	ImGui::End();
	ImGui::PopStyleColor(1);
}

void Icon3dAdjust_NotifyHoveredCell(CUICellItem* cell)
{
	g_icon3d_adjust.notify_hovered(cell);
}

void Icon3dAdjust_RegisterCell(CUICellItem* cell)
{
	g_icon3d_adjust.register_cell(cell);
}

void Icon3dAdjust_UnregisterCell(CUICellItem* cell)
{
	g_icon3d_adjust.unregister_cell(cell);
}
