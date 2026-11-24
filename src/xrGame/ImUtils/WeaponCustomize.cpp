#include "StdAfx.h"
#include "ImUtils.h"
#include "../Level.h"
#include "../Actor.h"
#include "../Actor_Flags.h"
#include "../Inventory.h"
#include "../inventory_item.h"
#include "../PhysicsShellHolder.h"
#include "../ai_space.h"
#include "../alife_simulator.h"
#include "../ai_object_location.h"
#include "../ui/UIInventoryUtilities.h"
#include "../../xrEngine/string_table.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../Include/xrRender/RenderVisual.h"

extern CSE_Abstract* CALifeSimulator__spawn_item2(CALifeSimulator* self_, const char* section, const Fvector& position, u32 level_vertex_id, GameGraph::_GRAPH_ID game_vertex_id, ALife::_OBJECT_ID id_parent);

namespace
{
	constexpr u32 MaxPreviewVisuals = 64;

	struct SPreviewFrame
	{
		Fmatrix View;
		Fmatrix Project;
		UIIconVisual Visuals[MaxPreviewVisuals];
		u32 Count = 0;
	};

	struct SSlot
	{
		PIItem Owner;
		shared_str Bone;
		const RStringVec* Sections;
		PIItem Attached;
		ImVec2 Point;
	};

	ALife::_OBJECT_ID g_customize_id = ALife::_OBJECT_ID(-1);
	float g_customize_yaw = PI * 0.5f;
	float g_customize_pitch = 0.f;
	SPreviewFrame g_preview_frame;

	ImVec2 Project(const Fvector& point, const ImVec2& origin, const ImVec2& size)
	{
		Fvector ndc;
		g_preview_frame.Project.transform(ndc, point);
		return ImVec2(origin.x + (ndc.x + 1.f) * 0.5f * size.x, origin.y + (1.f - ndc.y) * 0.5f * size.y);
	}

	void CollectVisuals(PIItem item, const Fmatrix& xform, const ImVec2& origin, const ImVec2& size, xr_vector<SSlot>& slots)
	{
		IKinematics* model = PKinematics(item->object().Visual());
		model->CalculateBones_Invalidate();
		model->CalculateBones(true);

		if (g_preview_frame.Count < MaxPreviewVisuals)
		{
			g_preview_frame.Visuals[g_preview_frame.Count++] = { item->object().Visual(), xform };
		}

		for (const auto& [bone, sections] : item->ItemDesc().m_customize_bones)
		{
			const u16 bone_id = model->LL_BoneID(bone);
			if (bone_id == BI_NONE)
			{
				continue;
			}

			Fmatrix bone_xform;
			bone_xform.mul_43(xform, model->LL_GetTransform(bone_id));

			PIItem attached = nullptr;
			for (const auto& [id, child] : item->m_children)
			{
				if (child->m_attach_bone == bone)
				{
					attached = child;
					break;
				}
			}

			slots.push_back({ item, bone, &sections, attached, Project(bone_xform.c, origin, size) });

			if (attached)
			{
				Fmatrix attached_xform;
				attached_xform.mul_43(bone_xform, attached->m_attach_offset);
				CollectVisuals(attached, attached_xform, origin, size, slots);
			}
		}
	}

	// Drawn into the interface image's bottom right corner, away from the spawn menu's icons that fill it from the top, then shown from it
	void DrawPreview(const ImVec2& Min, const ImVec2& Max, const ImVec2& Corner, const ImVec2& Room)
	{
		const ImVec2 Scale(UI().ClientToScreenScaledX(1.0f), UI().ClientToScreenScaledY(1.0f));

		Frect Clip;
		Clip.set(Corner.x / Scale.x, Corner.y / Scale.y, Room.x / Scale.x, Room.y / Scale.y);

		const IRender_interface::SurfaceParams Image = ::Render->BeginUIImage();
		if (Image.Surface)
		{
			UI().PushScissor(Clip);

			Fcolor Color;
			Color.set(1.f, 1.f, 1.f, 1.f);
			::Render->RenderUI(xr_span<const UIIconVisual>(g_preview_frame.Visuals, g_preview_frame.Count), g_preview_frame.View, g_preview_frame.Project, &Color);

			UI().PopScissor();
		}
		::Render->EndUIImage();

		if (Image.Surface)
		{
			ImGui::GetWindowDrawList()->AddImage(Image.Surface, Min, Max, ImVec2(Corner.x / Image.w, Corner.y / Image.h), ImVec2(Room.x / Image.w, Room.y / Image.h));
		}
	}

	bool IconButton(const char* id, const shared_str& section, const ImVec2& size)
	{
		const auto surface = ::Render->getSurface(pSettings->read_if_exists<str_c>(section, "icons_texture", "ui\\ui_icon_equipment"));
		if (!surface.Surface || !pSettings->line_exist(section, "inv_grid_x"))
		{
			return ImGui::Button(id, size);
		}

		const float scale = pSettings->read_if_exists<float>(section, "inv_scale", 1.0f);
		const float x = pSettings->r_float(section, "inv_grid_x") * INV_GRID_WIDTH(scale);
		const float y = pSettings->r_float(section, "inv_grid_y") * INV_GRID_HEIGHT(scale);
		const float w = pSettings->r_float(section, "inv_grid_width") * INV_GRID_WIDTH(scale);
		const float h = pSettings->r_float(section, "inv_grid_height") * INV_GRID_HEIGHT(scale);
		const float fit = std::min(size.x / w, size.y / h);

		return ImGui::ImageButton(id, surface.Surface, ImVec2(w * fit, h * fit), ImVec2(x / surface.w, y / surface.h), ImVec2((x + w) / surface.w, (y + h) / surface.h));
	}

	const char* ItemName(const shared_str& section)
	{
		return g_pStringTable->translate(pSettings->read_if_exists<str_c>(section, "inv_name", section.c_str())).c_str();
	}

	void MoveItem(ALife::_OBJECT_ID from, ALife::_OBJECT_ID to, ALife::_OBJECT_ID what)
	{
		NET_Packet P;
		CGameObject::u_EventGen(P, GE_OWNERSHIP_REJECT, from);
		P << what;
		CGameObject::u_EventSend(P);

		CGameObject::u_EventGen(P, GE_OWNERSHIP_TAKE, to);
		P << what;
		CGameObject::u_EventSend(P);
	}

	PIItem FindInInventory(CActor* actor, const shared_str& section)
	{
		for (PIItem item : actor->inventory().m_all)
		{
			if (item->object().cNameSect() == section && item->object().H_Parent() == actor)
			{
				return item;
			}
		}
		return nullptr;
	}

	u32 CountInInventory(CActor* actor, const shared_str& section)
	{
		u32 count = 0;
		for (PIItem item : actor->inventory().m_all)
		{
			if (item->object().cNameSect() == section && item->object().H_Parent() == actor)
			{
				++count;
			}
		}
		return count;
	}

	void Install(CActor* actor, const SSlot& slot, const shared_str& section)
	{
		if (slot.Attached)
		{
			if (slot.Attached->object().cNameSect() == section)
			{
				return;
			}

			MoveItem(slot.Owner->object_id(), actor->ID(), slot.Attached->object_id());
		}

		if (PIItem item = FindInInventory(actor, section))
		{
			item->m_attach_request = slot.Bone;
			MoveItem(actor->ID(), slot.Owner->object_id(), item->object_id());
			return;
		}

		if (psActorFlags.test(AF_GOD_MODE))
		{
			CALifeSimulator__spawn_item2(ai().get_alife(), section.c_str(), actor->Position(), actor->ai_location().level_vertex_id(), actor->ai_location().game_vertex_id(), slot.Owner->object_id());
		}
	}

	void DrawSlot(CActor* actor, const SSlot& slot, const ImVec2& center, const ImVec2& min, const ImVec2& max)
	{
		constexpr float Distance = 110.f;
		const ImVec2 button_size(64.f, 40.f);

		ImVec2 dir(slot.Point.x - center.x, slot.Point.y - center.y);
		const float length = sqrtf(dir.x * dir.x + dir.y * dir.y);
		dir = length > 1.f ? ImVec2(dir.x / length, dir.y / length) : ImVec2(0.f, -1.f);

		ImVec2 pos(slot.Point.x + dir.x * Distance, slot.Point.y + dir.y * Distance);
		pos.x = std::clamp(pos.x, min.x + button_size.x * 0.5f, max.x - button_size.x * 0.5f);
		pos.y = std::clamp(pos.y, min.y + button_size.y * 0.5f, max.y - button_size.y * 0.5f);

		ImDrawList* draw_list = ImGui::GetWindowDrawList();
		const ImU32 color = slot.Attached ? IM_COL32(120, 220, 120, 255) : IM_COL32(200, 200, 200, 160);
		draw_list->AddLine(slot.Point, pos, color, 1.5f);
		draw_list->AddCircleFilled(slot.Point, 3.f, color);

		ImGui::PushID(slot.Owner);
		ImGui::PushID(slot.Bone.c_str());

		ImGui::SetCursorScreenPos(ImVec2(pos.x - button_size.x * 0.5f, pos.y - button_size.y * 0.5f));
		const bool clicked = slot.Attached ? IconButton("##slot", slot.Attached->object().cNameSect(), button_size) : ImGui::Button("+##slot", button_size);
		ImGui::SetItemTooltip("%s", slot.Bone.c_str());

		if (clicked)
		{
			ImGui::OpenPopup("slot_popup");
		}

		if (ImGui::BeginPopup("slot_popup"))
		{
			ImGui::TextDisabled("%s", slot.Bone.c_str());

			if (slot.Attached && ImGui::Selectable("Detach"))
			{
				MoveItem(slot.Owner->object_id(), actor->ID(), slot.Attached->object_id());
			}

			ImGui::Separator();

			const bool god = psActorFlags.test(AF_GOD_MODE);
			for (const shared_str& section : *slot.Sections)
			{
				const u32 count = CountInInventory(actor, section);
				ImGui::PushID(section.c_str());
				ImGui::BeginDisabled(!count && !god);

				if (IconButton("##icon", section, ImVec2(48.f, 32.f)))
				{
					Install(actor, slot, section);
					ImGui::CloseCurrentPopup();
				}
				ImGui::SameLine();
				ImGui::Text("%s (%u)", ItemName(section), count);

				ImGui::EndDisabled();
				ImGui::PopID();
			}

			ImGui::EndPopup();
		}

		ImGui::PopID();
		ImGui::PopID();
	}
}

void OpenWeaponCustomize(u32 id)
{
	g_customize_id = id;
	Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_WeaponCustomize)] = true;
}

void RenderWeaponCustomizeWindow()
{
	bool& opened = Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_WeaponCustomize)];
	if (!opened || !g_pGameLevel)
	{
		return;
	}

	CActor* actor = Actor();
	CObject* object = Level().Objects.net_Find(g_customize_id);
	PIItem weapon = object ? object->cast_inventory_item() : nullptr;
	if (!actor || !weapon)
	{
		opened = false;
		return;
	}

	ImGui::SetNextWindowSize(ImVec2(960.f, 640.f), ImGuiCond_FirstUseEver);
	if (!ImGui::Begin("Weapon Customize##InGame", &opened))
	{
		ImGui::End();
		return;
	}

	const ImVec2 min = ImGui::GetCursorScreenPos();
	const ImVec2 avail = ImGui::GetContentRegionAvail();
	const ImVec2 max(min.x + avail.x, min.y + avail.y);
	const ImVec2 center((min.x + max.x) * 0.5f, (min.y + max.y) * 0.5f);

	ImGui::SetNextItemAllowOverlap();
	ImGui::InvisibleButton("##viewport", avail);
	if (ImGui::IsItemActive())
	{
		const ImVec2 delta = ImGui::GetIO().MouseDelta;
		g_customize_yaw += delta.x * 0.01f;
		g_customize_pitch = std::clamp(g_customize_pitch + delta.y * 0.01f, -PI_DIV_2, PI_DIV_2);
	}

	const ImVec2 density = ImGui::GetWindowViewport()->FramebufferScale;
	const ImVec2 room(UI_BASE_WIDTH * UI().ClientToScreenScaledX(1.0f), UI_BASE_HEIGHT * UI().ClientToScreenScaledY(1.0f));
	const float fit = std::min({ 1.f, room.x / (avail.x * density.x), room.y / (avail.y * density.y) });
	const ImVec2 texels(density.x * fit, density.y * fit);
	const ImVec2 corner(room.x - avail.x * texels.x, room.y - avail.y * texels.y);
	const ImVec2 screen_origin(min.x - corner.x / texels.x, min.y - corner.y / texels.y);
	const ImVec2 screen_size(room.x / texels.x, room.y / texels.y);

	g_preview_frame.View.build_camera_dir(Fidentity.c, Fidentity.k, Fidentity.j);
	g_preview_frame.Project.build_projection(deg2rad(20.f), screen_size.y / screen_size.x, 0.1f, 50.f);
	g_preview_frame.Count = 0;

	constexpr float Distance = 2.f;
	Fvector unit;
	g_preview_frame.Project.transform(unit, Fvector().set(1.f, 1.f, Distance));

	const Fvector2 ndc_center = { (center.x - screen_origin.x) / screen_size.x * 2.f - 1.f, 1.f - (center.y - screen_origin.y) / screen_size.y * 2.f };
	const Fvector2 half_world = { avail.x / screen_size.x / unit.x, avail.y / screen_size.y / unit.y };

	const vis_data& vis = weapon->object().Visual()->getVisData();
	Fvector box_center;
	vis.box.getcenter(box_center);
	const float scale = 0.9f * std::min(half_world.x, half_world.y) / vis.sphere.R;

	Fmatrix xform, transform;
	xform.translate(-box_center.x, -box_center.y, -box_center.z);
	transform.scale(scale, scale, scale);
	xform.mulA_43(transform);
	transform.setHPB(g_customize_yaw, g_customize_pitch, 0.f);
	xform.mulA_43(transform);
	transform.translate(ndc_center.x / unit.x, ndc_center.y / unit.y, Distance);
	xform.mulA_43(transform);

	xr_vector<SSlot> slots;
	CollectVisuals(weapon, xform, screen_origin, screen_size, slots);

	DrawPreview(min, max, corner, room);

	for (const SSlot& slot : slots)
	{
		DrawSlot(actor, slot, center, min, max);
	}

	ImGui::End();
}
