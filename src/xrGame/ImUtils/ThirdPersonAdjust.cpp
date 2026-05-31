#include "StdAfx.h"
#include "../Actor.h"
#include "../Inventory.h"
#include "../InventoryWeaponSlotLayout.h"
#include "../Weapon.h"
#include "../../xrUI/ui_base.h"
#include "ImUtils.h"
#include "../game_news.h"

extern float ATT_ITEM_MOVE_STEP;
extern float ATT_ITEM_ROT_STEP;

bool enumBones(void* data, int idx, const char** item)
{
	xr_vector<shared_str>* bones = (xr_vector<shared_str>*)data;
	*item = (*bones)[idx].c_str();
	return true;
}

static void ThirdAdjustDrawSaveButton()
{
	if (!ImGui::Button("Save"))
	{
		return;
	}

	string_path fn = {};

	FS.update_path(fn, "$app_data_root$", "3rd_adjust\\saved.ltx");
	CInifile file(fn, false, true, true);
	file.set_override_names(true);

	auto writeAttachParams = [](const CAttachableItem* attached_item, CInifile& file) -> void
	{
		const char* sect = attached_item->item().m_section_id.c_str();

		file.w_fvector3(sect, "attach_position_offset", attached_item->m_offset_position);
		file.w_fvector3(sect, "attach_angle_offset", attached_item->m_offset_rotation);
		file.w_string(sect, "attach_bone_name", *attached_item->bone_name());
	};

	for (const auto& item : Actor()->attached_objects())
	{
		writeAttachParams(item, file);
	}

	auto writeStrappedParams = [](u16 slot, CInifile& file) -> void
	{
		if (PIItem item_from_slot = Actor()->inventory().ItemFromSlot(slot);
			CWeapon * wpn = item_from_slot ? item_from_slot->cast_weapon() : nullptr)
		{
			if (wpn->strapped_mode() || wpn->strapped_mode_rifle())
			{
				const char* sect = *wpn->cNameSect();

				bool use_alt_position = IsSidearmPhysicalSlot(wpn->CurrSlot());

				if (!use_alt_position)
				{
					file.w_fvector3(sect, "strap_position", wpn->m_StrapOffset.StrapPosition);
					file.w_fvector3(sect, "strap_orientation", wpn->m_StrapOffset.StrapRotation);
				}
				else
				{
					file.w_fvector3(sect, "strap_position_alt", wpn->m_StrapOffsetAlt.StrapPosition);
					file.w_fvector3(sect, "strap_orientation_alt", wpn->m_StrapOffsetAlt.StrapRotation);
				}
			}
		}
	};

	writeStrappedParams(INV_SLOT_3, file);
	writeStrappedParams(INV_SLOT_2, file);

	PIItem active_item = Actor()->inventory().ActiveItem();

	if (CWeapon* wpn = active_item ? active_item->cast_weapon() : nullptr)
	{
		const char* sect = *wpn->cNameSect();

		file.w_fvector3(sect, "position", wpn->m_ActiveOffset.StrapPosition);
		file.w_fvector3(sect, "orientation", wpn->m_ActiveOffset.StrapRotation);
	}

	GAME_NEWS_DATA news_data = {};
	news_data.m_type = GAME_NEWS_DATA::eNewsType::eNews;
	news_data.news_caption = "Saved result to:";
	news_data.news_text = fn;
	news_data.show_time = 5000;
	news_data.texture_name = "ui_iconsTotal_bar_darklab_documents2";
	Actor()->AddGameNews(news_data);
}

void Render3rdAdjust()
{
	if (!Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_3rdAdjust)])
	{
		return;
	}

	if (!g_pGameLevel)
	{
		return;
	}

	if (!ai().get_alife())
	{
		return;
	}

	if (!g_actor)
	{
		return;
	}

	if (load_screen_renderer.IsActive())
	{
		return;
	}

	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(0.0f, 0.0f, 0.0f, kGeneralAlphaLevelForImGuiWindows));

	if (!ImGui::Begin("3rd Adjust", &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_3rdAdjust)]))
	{
		ImGui::PopStyleColor(1);
		return;
	}

	ThirdAdjustDrawSaveButton();

	ImGui::SeparatorText("Adjust steps");

	ImGui::InputFloat("Pos Step", &ATT_ITEM_MOVE_STEP, 0.0f, 0.0f, "%.6f");
	ImGui::InputFloat("Rot Step", &ATT_ITEM_ROT_STEP, 0.0f, 0.0f, "%.6f");

	ImGui::SeparatorText("Attached Items List");

	const auto& ActorBones = *Actor()->Visual()->dcast_PKinematics()->LL_Bones();

	xr_string base_header_name = "";

	auto addItemInfo = [&base_header_name, &ActorBones](CAttachableItem* item) -> void
	{
		ImGui::PushID(item->item().object_id());

		base_header_name = "Attached Item: ";
		base_header_name += *item->item().m_section_id;

		if (ImGui::CollapsingHeader(base_header_name.c_str()))
		{
			int attached_bone = -1;
			RStringVec Bones = {};

			for (const auto& [name, id] : ActorBones)
			{
				Bones.push_back(name);

				if (name == item->bone_name())
				{
					attached_bone = (int)Bones.size() - 1;
				}
			}

			if (ImGui::Combo("Attach Bone", &attached_bone, enumBones, &Bones, (int)ActorBones.size()))
			{
				u16 BoneId = Actor()->Visual()->dcast_PKinematics()->LL_BoneID(Bones[attached_bone]);
				item->set_bone_name(Bones[attached_bone]);
				item->set_bone_id(BoneId);
			}

			ImGui::SeparatorText("Position");

			if (ImGui::Button("Reset##AP"))
			{
				item->m_offset_position = READ_IF_EXISTS(pSettings, r_fvector3, item->item().m_section_id, "attach_position_offset", zero_vel);
			}

			ImGui::DragFloat("X##AP", &item->m_offset_position.x, ATT_ITEM_MOVE_STEP, -100.0f, 100.0f, "%.6f");
			ImGui::DragFloat("Y##AP", &item->m_offset_position.y, ATT_ITEM_MOVE_STEP, -100.0f, 100.0f, "%.6f");
			ImGui::DragFloat("Z##AP", &item->m_offset_position.z, ATT_ITEM_MOVE_STEP, -100.0f, 100.0f, "%.6f");

			ImGui::SeparatorText("Rotation");

			if (ImGui::Button("Reset##AR"))
			{
				item->m_offset_rotation = READ_IF_EXISTS(pSettings, r_fvector3, item->item().m_section_id, "attach_angle_offset", zero_vel);
			}

			ImGui::DragFloat("X##AR", &item->m_offset_rotation.x, ATT_ITEM_ROT_STEP, -100.0f, 100.0f, "%.6f");
			ImGui::DragFloat("Y##AR", &item->m_offset_rotation.y, ATT_ITEM_ROT_STEP, -100.0f, 100.0f, "%.6f");
			ImGui::DragFloat("Z##AR", &item->m_offset_rotation.z, ATT_ITEM_ROT_STEP, -100.0f, 100.0f, "%.6f");
		}

		ImGui::PopID();
	};

	for (CAttachableItem* attach : Actor()->attached_objects())
	{
		addItemInfo(attach);
	}

	auto addStrapInfo = [&base_header_name](u16 slot) -> void
	{
		if (PIItem item_from_slot = Actor()->inventory().ItemFromSlot(slot);
			CWeapon * wpn = item_from_slot ? item_from_slot->cast_weapon() : nullptr)
		{
			if (wpn->strapped_mode() || wpn->strapped_mode_rifle())
			{
				ImGui::PushID(wpn->ID());

				base_header_name = "Strapped Item: ";
				base_header_name += *wpn->cNameSect();

				if (ImGui::CollapsingHeader(base_header_name.c_str()))
				{
					ImGui::SeparatorText("Position");

					bool use_alt_position = IsSidearmPhysicalSlot(wpn->CurrSlot());
					auto& strap_position = use_alt_position ? wpn->m_StrapOffsetAlt : wpn->m_StrapOffset;

					if (ImGui::Button("Reset##SP"))
					{
						Fvector _default = Fvector().set(-0.34f, (use_alt_position ? 0.20f : -0.20f), 0.15f);
						strap_position.StrapPosition = READ_IF_EXISTS(pSettings, r_fvector3, wpn->cNameSect(), use_alt_position ? "strap_position_alt" : "strap_position", _default);
					}

					ImGui::DragFloat("X##SP", &strap_position.StrapPosition.x, ATT_ITEM_MOVE_STEP, -100.0f, 100.0f, "%.6f");
					ImGui::DragFloat("Y##SP", &strap_position.StrapPosition.y, ATT_ITEM_MOVE_STEP, -100.0f, 100.0f, "%.6f");
					ImGui::DragFloat("Z##SP", &strap_position.StrapPosition.z, ATT_ITEM_MOVE_STEP, -100.0f, 100.0f, "%.6f");

					ImGui::SeparatorText("Rotation");

					if (ImGui::Button("Reset##SR"))
					{
						Fvector _default = Fvector().set(0.0f, 0.0f, (use_alt_position ? 94.0f : 84.0f));
						strap_position.StrapRotation = READ_IF_EXISTS(pSettings, r_fvector3, wpn->cNameSect(), use_alt_position ? "strap_orientation_alt" : "strap_orientation", _default);
					}

					ImGui::DragFloat("X##SR", &strap_position.StrapRotation.x, ATT_ITEM_ROT_STEP, -100.0f, 100.0f, "%.6f");
					ImGui::DragFloat("Y##SR", &strap_position.StrapRotation.y, ATT_ITEM_ROT_STEP, -100.0f, 100.0f, "%.6f");
					ImGui::DragFloat("Z##SR", &strap_position.StrapRotation.z, ATT_ITEM_ROT_STEP, -100.0f, 100.0f, "%.6f");
				}

				ImGui::PopID();
			}
		}
	};

	addStrapInfo(INV_SLOT_3);
	addStrapInfo(INV_SLOT_2);

	PIItem active_item = Actor()->inventory().ActiveItem();

	if (CWeapon* wpn = active_item ? active_item->cast_weapon() : nullptr)
	{
		ImGui::PushID(wpn->ID());

		base_header_name = "Active Item: ";
		base_header_name += *wpn->cNameSect();

		if (ImGui::CollapsingHeader(base_header_name.c_str()))
		{
			ImGui::SeparatorText("Position");

			if (ImGui::Button("Reset##CP"))
			{
				wpn->m_ActiveOffset.StrapPosition = READ_IF_EXISTS(pSettings, r_fvector3, wpn->cNameSect(), "position", zero_vel);
			}

			ImGui::DragFloat("X##CP", &wpn->m_ActiveOffset.StrapPosition.x, ATT_ITEM_MOVE_STEP, -100.0f, 100.0f, "%.6f");
			ImGui::DragFloat("Y##CP", &wpn->m_ActiveOffset.StrapPosition.y, ATT_ITEM_MOVE_STEP, -100.0f, 100.0f, "%.6f");
			ImGui::DragFloat("Z##CP", &wpn->m_ActiveOffset.StrapPosition.z, ATT_ITEM_MOVE_STEP, -100.0f, 100.0f, "%.6f");

			ImGui::SeparatorText("Rotation");

			if (ImGui::Button("Reset##CR"))
			{
				wpn->m_ActiveOffset.StrapRotation = READ_IF_EXISTS(pSettings, r_fvector3, wpn->cNameSect(), "orientation", zero_vel);
			}

			ImGui::DragFloat("X##CR", &wpn->m_ActiveOffset.StrapRotation.x, ATT_ITEM_ROT_STEP, -100.0f, 100.0f, "%.6f");
			ImGui::DragFloat("Y##CR", &wpn->m_ActiveOffset.StrapRotation.y, ATT_ITEM_ROT_STEP, -100.0f, 100.0f, "%.6f");
			ImGui::DragFloat("Z##CR", &wpn->m_ActiveOffset.StrapRotation.z, ATT_ITEM_ROT_STEP, -100.0f, 100.0f, "%.6f");
		}

		ImGui::PopID();
	}

	ImGui::End();
	ImGui::PopStyleColor(1);
}