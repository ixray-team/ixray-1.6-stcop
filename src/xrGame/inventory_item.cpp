////////////////////////////////////////////////////////////////////////////
//	Module 		: inventory_item.cpp
//	Created 	: 24.03.2003
//  Modified 	: 29.01.2004
//	Author		: Victor Reutsky, Yuri Dobronravin
//	Description : Inventory item
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "inventory_item.h"
#include "inventory_item_impl.h"
#include "PhysicsShellHolder.h"
#include "Level.h"
#include "Actor.h"
#include "../../xrUI/ui_base.h"
#include "ui/UIGameSP.h"
#include "ui/UIActorMenu.h"
#include "../xrEngine/string_table.h"
#include "../xrEngine/IGame_ObjectPool.h"
#include "ai_object_location.h"
#include "Artefact.h"
#include "object_broker.h"
#include "SaveObjectHelpers.h"
#include "../Include/xrRender/Kinematics.h"
#include "../Include/xrRender/KinematicsAnimated.h"
#include "../xrPhysics/PhysicsShell.h"
#include "HudItem.h"
#include "Weapons/Components/HudCartridges.h"

#ifdef DEBUG_DRAW
#	include "debug_renderer.h"
#endif

constexpr const u32 ITEM_REMOVE_TIME = 30000;

net_updateInvData* CInventoryItem::NetSync()
{
	if (m_net_updateData == nullptr)
	{
		m_net_updateData = new net_updateInvData();
	}

	return m_net_updateData;
}

CInventoryItem::CInventoryItem()
{
	CurrentItemDesc = &SInventoryItemDesc::Registry::Get(shared_str());

	m_flags.set(Fbelt, false);
	m_flags.set(Fruck, true);
	m_flags.set(FRuckDefault, true);

	SetDropManual(false);

	m_flags.set(FCanTake, true);
	m_flags.set(FCanTrade, true);
	m_flags.set(FUsingCondition, false);
	m_fCondition = 1.0f;

	m_ItemCurrPlace.value = 0;
	m_ItemCurrPlace.type = eItemPlaceUndefined;
	m_ItemCurrPlace.base_slot_id = NO_ACTIVE_SLOT;
	m_ItemCurrPlace.slot_id = NO_ACTIVE_SLOT;

	m_flags.set(FIsHelperItem, false);
	m_flags.set(FCanStack, true);

	m_custom_text_offset.set(0.0f, 0.0f);

	m_custom_mark_offset.set(0.0f, 0.0f);
	m_custom_mark_size.set(0.0f, 0.0f);
}

CInventoryItem::~CInventoryItem()
{
	delete_data(m_net_updateData);

	if (m_hud_model)
	{
		IRenderVisual* visual = m_hud_model->dcast_RenderVisual();
		::Render->model_Delete(visual);
		m_hud_model = nullptr;
	}
	ReleaseHudMirrors();

#ifndef MASTER_GOLD
	bool B_GOOD = (!m_pInventory || (std::find(m_pInventory->m_all.begin(), m_pInventory->m_all.end(), this) == m_pInventory->m_all.end()));
	if (!B_GOOD)
	{
		CObject* p = object().H_Parent();
		Msg("inventory ptr is [%s]", m_pInventory ? "not-null" : "null");

		if (p != nullptr)
		{
			Msg("parent name is [%s]", p->cName().c_str());
		}

		Msg("! ERROR item_id[%d] H_Parent=[%s][%d] [%d]", object().ID(), p != nullptr ? p->cName().c_str() : "none", p != nullptr ? p->ID() : -1, Device.dwFrame);
	}
#endif // #ifndef MASTER_GOLD
}

void CInventoryItem::Load(const char* section)
{
	CHitImmunity::LoadImmunities(pSettings->r_string(section, "immunities_sect"), pSettings);

	// FFx0001 ++ begin
	SetDrawCost(pSettings->read_if_exists<bool>(section, "is_draw_cost", true));

	if (pSettings->line_exist(section, "hud_cartridge_bones"))
	{
		object().CreateComponent<THudCartridges>();
	}

	// Highlight separated by delimeter ',' related item sections on mouseover from the actor's inventory
	CurrentItemDesc = &SInventoryItemDesc::Registry::Get(section);

	m_HiglightRelatedItemSections.clear();
	if (pSettings->line_exist(section, "highlight_related_sections"))
	{
		const char* separated_sections = pSettings->r_string(section, "highlight_related_sections");
		for (int it = 0, count = _GetItemCount(separated_sections); it < count; ++it)
		{
			string128 higlight_section;
			_GetItem(separated_sections, it, higlight_section);
			m_HiglightRelatedItemSections.push_back(higlight_section);
		}
	}
	// FFx0001 ++ end

	if (CGameObject* GO = cast_game_object())
	{
		cast_game_object()->SpatialComponent->type |= ESPATIAL_TYPE::VISIBLEFORAI;
	}

	m_section_id._set(section);
	m_name = g_pStringTable->translate(pSettings->r_string(section, "inv_name"));
	m_nameShort = g_pStringTable->translate(pSettings->r_string(section, "inv_name_short"));

	m_weight = pSettings->r_float(section, "inv_weight");
	R_ASSERT(m_weight >= 0.f);

	m_cost = pSettings->r_u32(section, "cost");
	u32 sl = pSettings->read_if_exists<u32>(section, "slot", -1);
	m_ItemCurrPlace.base_slot_id = (sl == -1) ? 0 : (sl + 1);

	m_Description = g_pStringTable->translate(pSettings->read_if_exists<str_c>(section, "description", ""));

	m_flags.set(Fbelt, pSettings->read_if_exists<bool>(section, "belt", false));
	m_flags.set(FCanTake, pSettings->read_if_exists<bool>(section, "can_take", true));
	m_flags.set(FCanTrade, ItemDesc().m_can_trade);
	m_flags.set(FCanStack, pSettings->read_if_exists<bool>(section, "can_stack", true));
	m_flags.set(FIsQuestItem, pSettings->read_if_exists<bool>(section, "quest_item", false));

	// Added by Axel, to enable optional condition use on any item
	m_flags.set(FUsingCondition, pSettings->read_if_exists<bool>(section, "use_condition", false));

	if (BaseSlot() != NO_ACTIVE_SLOT || Belt())
	{
		bool defaultRuck = (BaseSlot() != NO_ACTIVE_SLOT && !Belt() && cast_hud_item()) ? false : true;
		m_flags.set(FRuckDefault, pSettings->read_if_exists<bool>(section, "default_to_ruck", defaultRuck));
		m_flags.set(FAllowSprint, pSettings->read_if_exists<bool>(section, "sprint_allowed", true));
		m_fControlInertionFactor = pSettings->read_if_exists<float>(section, "control_inertion_factor", 1.0f);
	}

	u32 inv_grid_x = pSettings->r_u32(m_object->cNameSect(), "inv_grid_x");
	u32 inv_grid_y = pSettings->r_u32(m_object->cNameSect(), "inv_grid_y");
	u32 inv_grid_width = pSettings->read_if_exists<u32>(m_object->cNameSect(), "inv_grid_width", 1);
	u32 inv_grid_height = pSettings->read_if_exists<u32>(m_object->cNameSect(), "inv_grid_height", 1);
	ScaleIcon = pSettings->read_if_exists<float>(m_object->cNameSect(), "inv_scale", 1.0f);
	IconsTexture = pSettings->read_if_exists<str_c>(section, "icons_texture", nullptr);

	m_inv_rect.set(inv_grid_x, inv_grid_y, inv_grid_width, inv_grid_height);

	ReadCustomTextAndMarks(section);
	Read3dStaticsData(section);

}

void CInventoryItem::SetAdditionalName(const char* text)
{
	m_AdditionalName = text;
	m_IsUsedAdditionalName = xr_strcmp(m_AdditionalName, "") != 0;

	if (m_IsUsedAdditionalName)
	{
		shared_str translated = g_pStringTable->translate(m_AdditionalName);
		if (xr_strcmp(translated.c_str(), "") != 0)
		{
			m_AdditionalName = translated.c_str();
		}
	}

	BuildName();
}

void CInventoryItem::SetPrependName(const char* text)
{
	m_PrependName = text;
	m_IsUsedPrependName = xr_strcmp(m_PrependName, "") != 0;

	if (m_IsUsedPrependName)
	{
		shared_str translated = g_pStringTable->translate(m_PrependName);
		if (xr_strcmp(translated.c_str(), "") != 0)
		{
			m_PrependName = translated.c_str();
		}
	}

	BuildName();
}

void CInventoryItem::BuildName()
{
	if (m_IsUsedAdditionalName && m_IsUsedPrependName)
	{
		m_ExtendedUnionName = make_string<const char*>("%s%s%s", m_PrependName.c_str(), m_name.c_str(), m_AdditionalName.c_str());
		return;
	}

	if (m_IsUsedPrependName)
	{
		m_ExtendedUnionName = make_string<const char*>("%s%s", m_PrependName.c_str(), m_name.c_str());
	}

	if (m_IsUsedAdditionalName)
	{
		m_ExtendedUnionName = make_string<const char*>("%s%s", m_name.c_str(), m_AdditionalName.c_str());
	}
}

void CInventoryItem::SetAdditionalDescription(const char* text)
{
	m_AdditionalDescription = text;
	m_IsUsedAdditionalDescription = xr_strcmp(m_AdditionalDescription, "") != 0;
	
	if (m_IsUsedAdditionalDescription)
	{
		shared_str translated = g_pStringTable->translate(m_AdditionalDescription);
		if (xr_strcmp(translated.c_str(), "") != 0)
		{
			m_AdditionalDescription = translated.c_str();
		}
	}

	BuildDescription();
}

void CInventoryItem::SetPrependDescription(const char* text)
{
	m_PrependDescription = text;
	m_IsUsedPrependDescription = xr_strcmp(m_PrependDescription, "") != 0;
	
	if (m_IsUsedPrependDescription)
	{
		shared_str translated = g_pStringTable->translate(m_PrependDescription);
		if (xr_strcmp(translated.c_str(), "") != 0)
		{
			m_PrependDescription = translated.c_str();
		}
	}

	BuildDescription();
}

void CInventoryItem::BuildDescription()
{
	if (m_IsUsedAdditionalDescription && m_IsUsedPrependDescription)
	{
		m_ExtendedUnionDescription = make_string<const char*>("%s\\n\\n%s\\n\\n%s", m_PrependDescription.c_str(), m_Description.c_str(), m_AdditionalDescription.c_str());
		return;
	}

	if (m_IsUsedPrependDescription)
	{
		m_ExtendedUnionDescription = make_string<const char*>("%s\\n\\n%s", m_PrependDescription.c_str(), m_Description.c_str());
	}

	if (m_IsUsedAdditionalDescription)
	{
		m_ExtendedUnionDescription = make_string<const char*>("%s\\n\\n%s", m_Description.c_str(), m_AdditionalDescription.c_str());
	}
}

CInventoryItem::EInvCellAnchor CInventoryItem::ParseInvCellAnchor(const char* value)
{
	if (!value || !value[0])
	{
		return EInvCellAnchor::BottomRight;
	}

	if (!_stricmp(value, "top_left") || !_stricmp(value, "tl"))
	{
		return EInvCellAnchor::TopLeft;
	}
	if (!_stricmp(value, "top_right") || !_stricmp(value, "tr"))
	{
		return EInvCellAnchor::TopRight;
	}
	if (!_stricmp(value, "bottom_left") || !_stricmp(value, "bl"))
	{
		return EInvCellAnchor::BottomLeft;
	}

	return EInvCellAnchor::BottomRight;
}

void CInventoryItem::ReadCustomTextAndMarks(const char* section)
{
	m_custom_text = pSettings->read_if_exists<str_c>(section, "item_custom_text", nullptr);
	m_custom_text_offset = pSettings->read_if_exists<Fvector2>(section, "item_custom_text_offset", Fvector2().set(0.f, 0.f));

	m_custom_text_font = nullptr;
	if (pSettings->line_exist(section, "item_custom_text_font"))
	{
		shared_str font_str = pSettings->r_string(section, "item_custom_text_font");
		IGame_ObjectPool::RunOnPrimaryThread([this, font_str]() { m_custom_text_font = UI().Font().GetFont(font_str); });
	}

	if (pSettings->line_exist(section, "item_custom_text_clr_inv"))
	{
		m_custom_text_clr_inv = pSettings->r_color(section, "item_custom_text_clr_inv");
	}
	else {
		m_custom_text_clr_inv = 0;
	}

	m_custom_mark_texture = pSettings->read_if_exists<str_c>(section, "item_custom_mark_texture", nullptr);
	m_custom_mark = pSettings->read_if_exists<bool>(section, "item_custom_mark", false);
	m_custom_mark_offset = pSettings->read_if_exists<Fvector2>(section, "item_custom_mark_offset", Fvector2().set(0.f, 0.f));
	m_custom_mark_size = pSettings->read_if_exists<Fvector2>(section, "item_custom_mark_size", Fvector2().set(0.f, 0.f));
	m_custom_mark_clr = pSettings->read_if_exists<Fcolor>(section, "item_custom_mark_clr", Fcolor(0)).get();
}

void CInventoryItem::Read3dStaticsData(const char* section)
{
	m_3d_static_visual_name = pSettings->read_if_exists<str_c>(section, "3d_static_visual_name", *object().cNameVisual());

	m_3d_static_rotate = pSettings->read_if_exists<Fvector3>(section, "3d_static_rotate", m_3d_static_rotate.set(0,0,0));
	m_3d_static_rotate.mul(M_PI / 180.0f);

	m_3d_static_scale = pSettings->read_if_exists<float>(section, "3d_static_scale", 1.f);
	IconOffset = pSettings->read_if_exists<Fvector2>(section, "3d_static_offset", Fvector2().set(0.0f, 0.0f));
}

void CInventoryItem::RefreshTranslations()
{
	// Re-translate the cached strings after language change
	if (!m_section_id.size())
		return;

	const char* section = m_section_id.c_str();

	if (pSettings->line_exist(section, "inv_name"))
	{
		m_name = g_pStringTable->translate(pSettings->r_string(section, "inv_name"));
	}

	if (pSettings->line_exist(section, "inv_name_short"))
	{
		m_nameShort = g_pStringTable->translate(pSettings->r_string(section, "inv_name_short"));
	}

	if (pSettings->line_exist(section, "description"))
	{
		m_Description = g_pStringTable->translate(pSettings->r_string(section, "description"));
	}

	BuildName();
	BuildDescription();
}

void CInventoryItem::ChangeCondition(float fDeltaCondition)
{
	m_fCondition += fDeltaCondition;
	clamp(m_fCondition, 0.0f, 1.0f);
}

void CInventoryItem::Hit(SHit* pHDS)
{
	if (!IsUsingCondition())
	{
		return;
	}

	float hit_power = pHDS->damage();
	hit_power *= GetHitImmunity(pHDS->hit_type);

	ChangeCondition(-hit_power);
}

void CInventoryItem::OnH_B_Independent(bool just_before_destroy)
{
	UpdateXForm();
	m_ItemCurrPlace.type = eItemPlaceUndefined;
}

void CInventoryItem::OnH_A_Independent()
{
	m_dwItemIndependencyTime = Level().timeServer();
	m_ItemCurrPlace.type = eItemPlaceUndefined;
	inherited::OnH_A_Independent();
}

void CInventoryItem::OnH_B_Chield()
{
	Level().RemoveObject_From_4CrPr(m_object);
}

void CInventoryItem::OnH_A_Chield()
{
	inherited::OnH_A_Chield();
}

#ifdef DEBUG
extern	Flags32	dbg_net_Draw_Flags;
#endif

static void UpdateHudModel(IKinematics* model)
{
	if (!model)
	{
		return;
	}

	if (IKinematicsAnimated* ka = model->dcast_PKinematicsAnimated())
	{
		ka->UpdateTracks();
	}
	model->CalculateBones_Invalidate();
	model->CalculateBones(true);
}

void CInventoryItem::UpdateCL()
{
#ifdef DEBUG
	if (bDebug)
	{
		if (dbg_net_Draw_Flags.test(dbg_draw_invitem))
		{
			Device.seqRender.Remove(this);
			Device.seqRender.Add(this);
		}
		else
		{
			Device.seqRender.Remove(this);
		}
	}

#endif
	if (!IsGameTypeSingle())
	{
		Interpolate();
	}

	UpdateHudModel(m_hud_model);
	for (SHudMirror& mirror : m_hud_mirrors)
	{
		UpdateHudModel(mirror.model);
	}
}

void CInventoryItem::OnEvent(NET_Packet& P, u16 type)
{
	switch (type)
	{
	case GE_ADDON_ATTACH:
	{
		ALife::_OBJECT_ID ItemID;
		P >> ItemID;
		CObject* finded_addon = Level().Objects.net_Find(ItemID);
		PIItem ItemToAttach = finded_addon != nullptr ? finded_addon->cast_inventory_item() : nullptr;
		if (ItemToAttach == nullptr)
		{
			break;
		}
		Attach(ItemToAttach);
	}break;
	case GE_ADDON_DETACH:
	{
		string64 i_name = {};
		P.r_stringZ(i_name);
		Detach(i_name, true);
	}break;

	case GE_REPAIR_ITEM:
	{
		SetCondition(1.0f);
	}break;
	case GE_CHANGE_POS:
	{
		Fvector p;
		P.r_vec3(p);
		CPHSynchronize* pSyncObj = nullptr;
		pSyncObj = object().PHGetSyncItem(0);
		if (pSyncObj == nullptr)
		{
			return;
		}

		SPHNetState state;
		pSyncObj->get_State(state);
		state.position = p;
		state.previous_position = p;
		pSyncObj->set_State(state);

	}break;

	case GE_OWNERSHIP_TAKE:
	{
		ALife::_OBJECT_ID id;
		P >> id;
		CObject* O = Level().Objects.net_Find(id);
		O->H_SetParent(m_object);
		m_children[id] = O->cast_inventory_item();
		m_children[id]->m_attach_parent = this;
		AttachChild(m_children[id]);
		AttachRoot()->NotifyAttachment(m_children[id], true);
	}break;
	case GE_OWNERSHIP_REJECT:
	{
		ALife::_OBJECT_ID id;
		P >> id;
		CObject* O = Level().Objects.net_Find(id);
		const bool just_before_destroy = !P.r_eof() && P.r_u8();
		O->SetTmpPreDestroy(just_before_destroy);
		AttachRoot()->NotifyAttachment(m_children[id], false);
		m_children[id]->m_attach_parent = nullptr;
		m_children[id]->m_attach_bone = nullptr;
		m_children[id]->m_attach_bone_id = BI_NONE;
		m_children[id]->ReleaseHudMirrors();
		m_children.erase(id);
		O->H_SetParent(nullptr, just_before_destroy);
	}break;
	}
}

CInventoryItem* CInventoryItem::GetChild(ALife::_OBJECT_ID id) const
{
	const auto it = m_children.find(id);
	if (it != m_children.end())
	{
		return it->second;
	}

	for (const auto& [child_id, child] : m_children)
	{
		if (CInventoryItem* nested = child->GetChild(id))
		{
			return nested;
		}
	}
	return nullptr;
}

bool CInventoryItem::CanAttachChild(PIItem child, const shared_str& bone) const
{
	for (const auto& [id, other] : m_children)
	{
		if (other != child && other->m_attach_bone == bone)
		{
			return false;
		}
	}
	return true;
}

void CInventoryItem::CollectConflicts(PIItem item, xr_vector<PIItem>& conflicts) const
{
	const RStringVec& item_conflicts = item->ItemDesc().m_conflicts;
	for (const auto& [id, child] : m_children)
	{
		if (child == item || !child->m_attach_bone.size())
		{
			continue;
		}

		const RStringVec& child_conflicts = child->ItemDesc().m_conflicts;
		if (std::find(child_conflicts.begin(), child_conflicts.end(), item->object().cNameSect()) != child_conflicts.end() ||
			std::find(item_conflicts.begin(), item_conflicts.end(), child->object().cNameSect()) != item_conflicts.end())
		{
			if (std::find(conflicts.begin(), conflicts.end(), child) == conflicts.end())
			{
				conflicts.push_back(child);
			}
			continue;
		}

		child->CollectConflicts(item, conflicts);
	}

	for (const auto& [id, child] : item->m_children)
	{
		if (child->m_attach_bone.size())
		{
			CollectConflicts(child, conflicts);
		}
	}
}

void CInventoryItem::DetachConflicts(PIItem item)
{
	xr_vector<PIItem> conflicts = {};
	CollectConflicts(item, conflicts);

	for (PIItem conflict : conflicts)
	{
		bool ancestor = false;
		for (PIItem parent = item->m_attach_parent; parent; parent = parent->m_attach_parent)
		{
			ancestor |= parent == conflict;
		}

		if (ancestor)
		{
			continue;
		}

		NotifyAttachment(conflict, false);
		conflict->m_attach_bone = nullptr;

		if (!OnServer())
		{
			continue;
		}

		NET_Packet P;
		CGameObject::u_EventGen(P, GE_OWNERSHIP_REJECT, conflict->parent_id());
		P << conflict->object_id();
		CGameObject::u_EventSend(P);

		if (CObject* owner = object().H_Parent())
		{
			CGameObject::u_EventGen(P, GE_OWNERSHIP_TAKE, owner->ID());
			P << conflict->object_id();
			CGameObject::u_EventSend(P);
		}
	}
}

void CInventoryItem::AttachChild(PIItem child)
{
	const shared_str& section = child->object().cNameSect();

	child->m_attach_bone = nullptr;
	child->m_attach_bone_id = BI_NONE;
	child->m_attach_hud_bone_id = BI_NONE;
	child->ReleaseHudMirrors();

	CInventoryItem* root = this;
	while (root->m_attach_parent)
	{
		root = root->m_attach_parent;
	}

	root->DetachConflicts(child);

	const auto& bones = ItemDesc().m_customize_bones;
	const auto request = child->m_attach_request.size() ? bones.find(child->m_attach_request) : bones.end();
	const bool use_request = request != bones.end() && std::find(request->second.begin(), request->second.end(), section) != request->second.end() && CanAttachChild(child, request->first);
	child->m_attach_request = nullptr;

	for (const auto& [bone, sections] : bones)
	{
		if (std::find(sections.begin(), sections.end(), section) == sections.end())
		{
			continue;
		}

		if (!CanAttachChild(child, bone) || (use_request && bone != request->first))
		{
			continue;
		}

		child->m_attach_bone = bone;
		child->m_attach_bone_id = PKinematics(object().Visual())->LL_BoneID(bone);

		const auto placement = ItemDesc().m_attach_placements.find(section);
		child->m_attach_offset = placement != ItemDesc().m_attach_placements.end() ? placement->second.m_world : Fidentity;
		child->m_attach_hud_offset = placement != ItemDesc().m_attach_placements.end() ? placement->second.m_hud : Fidentity;
		break;
	}

	if (!child->m_attach_bone.size())
	{
		Msg("! [%s]: no free slot for [%s]", object().cNameSect_str(), section.c_str());
	}
}

void CInventoryItem::ReleaseHudMirrors()
{
	for (SHudMirror& mirror : m_hud_mirrors)
	{
		if (mirror.model)
		{
			IRenderVisual* visual = mirror.model->dcast_RenderVisual();
			::Render->model_Delete(visual);
		}
	}
	m_hud_mirrors.clear();
}

IKinematics* CInventoryItem::HudModel()
{
	if (!m_hud_model && ItemDesc().m_attach_hud_visual.size())
	{
		m_hud_model = PKinematics(::Render->model_Create(ItemDesc().m_attach_hud_visual.c_str()));
	}

	return m_hud_model;
}

void CInventoryItem::RenderHud(const Fmatrix& xform, IKinematics* model, IDSGraphManager* DM)
{
	model->CalculateBones(true);

	if (THudCartridges* cartridges = object().GetComponent<THudCartridges>())
	{
		cartridges->Render(xform, model, DM);
	}

	for (const auto& [id, child] : m_children)
	{
		if (!child->m_attach_bone.size())
		{
			continue;
		}

		if (child->m_attach_hud_bone_id == BI_NONE)
		{
			child->m_attach_hud_bone_id = model->LL_BoneID(child->m_attach_bone);
			R_ASSERT3(child->m_attach_hud_bone_id != BI_NONE, "attachment bone not found in hud model", child->m_attach_bone.c_str());
		}

		IKinematics* child_model = child->HudModel();

		child->m_attach_hud_transform.mul_43(xform, model->LL_GetTransform(child->m_attach_hud_bone_id));
		child->m_attach_hud_transform.mulB_43(child->m_attach_hud_offset);
		DM->add_Dynamic(child_model->dcast_RenderVisual(), &child->m_attach_hud_transform, true);

		child->RenderHud(child->m_attach_hud_transform, child_model, DM);

		const auto mirrors = ItemDesc().m_attach_hud_mirrors.find(child->m_attach_bone);
		if (mirrors == ItemDesc().m_attach_hud_mirrors.end())
		{
			continue;
		}

		if (child->m_hud_mirrors.size() < mirrors->second.size())
		{
			child->m_hud_mirrors.resize(mirrors->second.size());
		}

		for (u32 i = 0; i < mirrors->second.size(); ++i)
		{
			SHudMirror& mirror = child->m_hud_mirrors[i];
			if (mirror.bone == BI_NONE)
			{
				mirror.bone = model->LL_BoneID(mirrors->second[i]);
				R_ASSERT3(mirror.bone != BI_NONE, "mirror bone not found in hud model", mirrors->second[i].c_str());
			}

			if (!model->LL_GetBoneVisible(mirror.bone))
			{
				continue;
			}

			if (!mirror.model)
			{
				mirror.model = PKinematics(::Render->model_Create(child->ItemDesc().m_attach_hud_visual.c_str()));
			}

			mirror.transform.mul_43(xform, model->LL_GetTransform(mirror.bone));
			mirror.transform.mulB_43(child->m_attach_hud_offset);
			DM->add_Dynamic(mirror.model->dcast_RenderVisual(), &mirror.transform, true);

			if (THudCartridges* cartridges = child->object().GetComponent<THudCartridges>())
			{
				cartridges->Render(mirror.transform, mirror.model, DM, i + 1);
			}
		}
	}
}

void CInventoryItem::RenderIcon(const Fmatrix& xform, IKinematics* model, IDSGraphManager* DM, xr_deque<Fmatrix>& xforms)
{
	for (const auto& [id, child] : m_children)
	{
		const u16 bone = child->m_attach_bone.size() ? model->LL_BoneID(child->m_attach_bone) : BI_NONE;
		if (bone == BI_NONE)
		{
			continue;
		}

		IKinematics* child_model = PKinematics(child->object().Visual());
		child_model->CalculateBones(true);

		Fmatrix& child_xform = xforms.emplace_back();
		child_xform.mul_43(xform, model->LL_GetTransform(bone));
		child_xform.mulB_43(child->m_attach_offset);
		DM->add_Dynamic(child_model->dcast_RenderVisual(), &child_xform, true);

		child->RenderIcon(child_xform, child_model, DM, xforms);
	}
}

void CInventoryItem::AddAttachmentShapes(CPhysicsElement* element, const Fmatrix& xform)
{
	IKinematics* model = PKinematics(object().Visual());
	for (const auto& [id, child] : m_children)
	{
		if (child->m_attach_bone_id == BI_NONE)
		{
			continue;
		}

		IKinematics* child_model = PKinematics(child->object().Visual());
		child_model->CalculateBones_Invalidate();
		child_model->CalculateBones(true);

		Fmatrix child_xform;
		child_xform.mul_43(xform, model->LL_GetTransform(child->m_attach_bone_id));
		child_xform.mulB_43(child->m_attach_offset);

		for (u16 bone = 0, count = child_model->LL_BoneCount(); bone < count; ++bone)
		{
			SBoneShape& shape = child_model->LL_GetData(bone).shape;
			if (shape.type == SBoneShape::stNone || !shape.Valid())
			{
				continue;
			}

			Fmatrix offset = Fidentity;
			offset.mul_43(child_xform, child_model->LL_GetTransform(bone));
			element->add_Shape(shape, offset);
		}

		child->AddAttachmentShapes(element, child_xform);
	}
}

static void PlayHudModelMotion(IKinematics* model, const shared_str& name, bool mix, float speed)
{
	IKinematicsAnimated* ka = model ? model->dcast_PKinematicsAnimated() : nullptr;
	if (!ka)
	{
		return;
	}

	MotionID motion = ka->ID_Cycle_Safe(name);
	if (!motion.valid())
	{
		motion = ka->ID_Cycle_Safe("idle");
	}

	if (!motion.valid())
	{
		return;
	}

	for (u16 pid = 0, count = ka->partitions().count(); pid < count; ++pid)
	{
		if (CBlend* blend = ka->PlayCycle(pid, motion, mix))
		{
			blend->speed *= speed;
		}
	}
}

void CInventoryItem::PlayHudMotion(const shared_str& name, bool mix, float speed)
{
	for (const auto& [id, child] : m_children)
	{
		if (!child->m_attach_bone.size())
		{
			continue;
		}

		PlayHudModelMotion(child->HudModel(), name, mix, speed);
		for (SHudMirror& mirror : child->m_hud_mirrors)
		{
			PlayHudModelMotion(mirror.model, name, mix, speed);
		}

		child->PlayHudMotion(name, mix, speed);
	}
}

bool CInventoryItem::AttachTransform(bool hud, IKinematics*& kin, Fmatrix& xform)
{
	if (!m_attach_parent)
	{
		if (hud)
		{
			kin = cast_hud_item()->HudItemData()->m_model;
			xform = cast_hud_item()->HudItemData()->m_item_transform;
		}
		else
		{
			kin = PKinematics(object().Visual());
			xform = object().XFORM();
		}
		return true;
	}

	if (!m_attach_parent->AttachTransform(hud, kin, xform))
	{
		return false;
	}

	const u16 bone = kin->LL_BoneID(m_attach_bone);
	if (bone == BI_NONE)
	{
		return false;
	}

	xform.mulB_43(kin->LL_GetTransform(bone));
	xform.mulB_43(hud ? m_attach_hud_offset : m_attach_offset);
	kin = hud ? HudModel() : PKinematics(object().Visual());
	return true;
}

CInventoryItem* CInventoryItem::AttachRoot()
{
	CInventoryItem* root = this;
	while (root->m_attach_parent)
	{
		root = root->m_attach_parent;
	}
	return root;
}

void CInventoryItem::NotifyAttachment(PIItem item, bool attached)
{
	if (!item->m_attach_bone.size())
	{
		return;
	}

	if (attached)
	{
		OnAttachmentInc(item);
	}
	else
	{
		OnAttachmentDec(item);
	}

	for (const auto& [id, child] : item->m_children)
	{
		NotifyAttachment(child, attached);
	}
}

void CInventoryItem::FindAttachPose(shared_str& pose, shared_str& ik_mark, shared_str& fingers_mark) const
{
	for (const auto& [id, child] : m_children)
	{
		if (!child->m_attach_bone.size())
		{
			continue;
		}

		const SInventoryItemDesc& desc = child->ItemDesc();
		if (desc.m_anim_pose.size())
		{
			pose = desc.m_anim_pose;
			ik_mark = desc.m_ik_mark.size() ? desc.m_ik_mark : ik_mark;
			fingers_mark = desc.m_fingers_mark.size() ? desc.m_fingers_mark : fingers_mark;
		}

		child->FindAttachPose(pose, ik_mark, fingers_mark);
	}
}

void CInventoryItem::renderable_Render(IDSGraphManager* DM)
{
	inherited::renderable_Render(DM);

	if (m_children.empty())
	{
		return;
	}

	IKinematics* model = PKinematics(object().Visual());
	model->CalculateBones(true);

	for (const auto& [id, child] : m_children)
	{
		if (child->m_attach_bone_id == BI_NONE)
		{
			continue;
		}

		child->object().XFORM().mul_43(object().XFORM(), model->LL_GetTransform(child->m_attach_bone_id));
		child->object().XFORM().mulB_43(child->m_attach_offset);
		child->object().renderable_Render(DM);
	}
}

bool CInventoryItem::Detach(const char* item_section_name, bool b_spawn_item)
{
	if (OnClient())
	{
		return true;
	}

	if (b_spawn_item)
	{
		Level().spawn_item(item_section_name, object().Position(), object().ai_location().level_vertex_id(), object().H_Parent() ? object().H_Parent()->ID() : ALife::INVALID_OBJECT_ID);
	}
	return true;
}

/////////// network ///////////////////////////////
bool CInventoryItem::net_Spawn(CSE_Abstract* DC)
{
	PROF_EVENT("CInventoryItem::net_Spawn");
	VERIFY(!m_pInventory);

	m_flags.set(FInInterpolation, false);
	m_flags.set(FInInterpolate, false);

	m_flags.set(Fuseful_for_NPC, true);
	CSE_Abstract* e = (CSE_Abstract*)(DC);
	CSE_ALifeObject* alife_object = e != nullptr ? e->cast_alife_object() : nullptr;
	if (alife_object != nullptr)
	{
		m_flags.set(Fuseful_for_NPC, alife_object->m_flags.test(CSE_ALifeObject::flUsefulForAI));
	}

	object().SpatialComponent->type |= ESPATIAL_TYPE::ITEM;

	CSE_ALifeInventoryItem* pSE_InventoryItem = e != nullptr ? e->cast_inventory_item() : nullptr;
	if (pSE_InventoryItem == nullptr)
	{
		return true;
	}

	//!!!
	m_fCondition = pSE_InventoryItem->m_fCondition;

	if (IsGameTypeSingle())
	{
		net_Spawn_install_upgrades(pSE_InventoryItem->m_upgrades);
	}

	if (!IsGameTypeSingle())
		object().processing_activate();

	m_dwItemIndependencyTime = 0;

	m_just_after_spawn = true;
	m_activated = false;
	return true;
}

void CInventoryItem::net_Destroy()
{
	if (m_pInventory)
	{
		VERIFY(std::find(m_pInventory->m_all.begin(), m_pInventory->m_all.end(), this) == m_pInventory->m_all.end());
	}
}

void CInventoryItem::net_Import(NET_Packet& P)
{
	u8 NumItems = 0;
	NumItems = P.r_u8();
	if (!NumItems)
	{
		return;
	}

	mask_inv_num_items num_items;
	num_items.common = NumItems;
	NumItems = num_items.num_items;

	net_update_IItem N;
	N.dwTimeStamp = Device.dwTimeGlobal;

	net_Import_PH_Params(P, N, num_items);
	////////////////////////////////////////////
	P.r_u8();	//active (not freezed ot not)

	if (this->cast_game_object()->Local())
	{
		return;
	}

	net_updateInvData* p = NetSync();

	Level().AddObject_To_Objects4CrPr(m_object);

	p->NET_IItem.push_back(N);

	while (p->NET_IItem.size() > 2)
	{
		p->NET_IItem.pop_front();
	}

	if (!m_activated)
	{
#ifdef DEBUG
		Msg("Activating object [%d] before interpolation starts", object().ID());
#endif // #ifdef DEBUG
		object().processing_activate();
		m_activated = true;
	}
};

void CInventoryItem::net_Import_PH_Params(NET_Packet& P, net_update_IItem& N, mask_inv_num_items& num_items)
{
	P.r_vec3(N.State.force);
	P.r_vec3(N.State.torque);

	P.r_vec3(N.State.position);

	P.r_float(N.State.quaternion.x);
	P.r_float(N.State.quaternion.y);
	P.r_float(N.State.quaternion.z);
	P.r_float(N.State.quaternion.w);

	N.State.enabled = num_items.mask & CSE_ALifeInventoryItem::inventory_item_state_enabled;
	if (!(num_items.mask & CSE_ALifeInventoryItem::inventory_item_angular_null))
	{
		N.State.angular_vel.x = P.r_float();
		N.State.angular_vel.y = P.r_float();
		N.State.angular_vel.z = P.r_float();
	}
	else
	{
		N.State.angular_vel.set(0.0f, 0.0f, 0.0f);
	}

	if (!(num_items.mask & CSE_ALifeInventoryItem::inventory_item_linear_null))
	{
		N.State.linear_vel.x = P.r_float();
		N.State.linear_vel.y = P.r_float();
		N.State.linear_vel.z = P.r_float();
	}
	else
	{
		N.State.linear_vel.set(0.f, 0.f, 0.f);
	}

	N.State.previous_position = N.State.position;
	N.State.previous_quaternion = N.State.quaternion;
}

void CInventoryItem::net_Export_PH_Params(NET_Packet& P, SPHNetState& State, mask_inv_num_items& num_items)
{
	P.w_vec3(State.force);
	P.w_vec3(State.torque);
	P.w_vec3(State.position);

	float magnitude = _sqrt(State.quaternion.magnitude());
	if (fis_zero(magnitude))
	{
		magnitude = 1.0f;
		State.quaternion.x = 0.0f;
		State.quaternion.y = 0.0f;
		State.quaternion.z = 1.0f;
		State.quaternion.w = 0.0f;
	}

	P.w_float(State.quaternion.x);
	P.w_float(State.quaternion.y);
	P.w_float(State.quaternion.z);
	P.w_float(State.quaternion.w);

	if (!(num_items.mask & CSE_ALifeInventoryItem::inventory_item_angular_null))
	{
		P.w_float(State.angular_vel.x);
		P.w_float(State.angular_vel.y);
		P.w_float(State.angular_vel.z);
	}

	if (!(num_items.mask & CSE_ALifeInventoryItem::inventory_item_linear_null))
	{
		P.w_float(State.linear_vel.x);
		P.w_float(State.linear_vel.y);
		P.w_float(State.linear_vel.z);
	}
}

void CInventoryItem::net_Export(NET_Packet& P)
{
	//copy from CPhysicObject
	if (object().H_Parent() || IsGameTypeSingle())
	{
		P.w_u8(0);

		//To fix condition not persisting offline for items except weapons
		//Optimization, as I can't think of very many cases where we need update condition change when item is not actor's
		if (g_actor && this->parent_id() == g_actor->ID())
		{
			CGameObject* obj = cast_game_object();
			NET_Packet stpk;
			obj->u_EventGen(stpk, GE_SYNC_ALIFEITEM, obj->ID());
			stpk.w_float(m_fCondition);
			obj->u_EventSend(stpk, net_flags(false));
		}

		return;
	}

	CPHSynchronize* pSyncObj = nullptr;
	SPHNetState	State;
	pSyncObj = object().PHGetSyncItem(0);

	if (pSyncObj != nullptr && !object().H_Parent())
	{
		pSyncObj->get_State(State);
	}
	else
	{
		State.position.set(object().Position());
	}

	mask_inv_num_items num_items;
	num_items.mask = 0;
	u16	temp = object().PHGetSyncItemsNumber();
	R_ASSERT(temp < (u16(1) << 5));
	num_items.num_items = u8(temp);

	if (State.enabled)
	{
		num_items.mask |= CSE_ALifeInventoryItem::inventory_item_state_enabled;
	}

	if (fis_zero(State.angular_vel.square_magnitude()))
	{
		num_items.mask |= CSE_ALifeInventoryItem::inventory_item_angular_null;
	}

	if (fis_zero(State.linear_vel.square_magnitude()))
	{
		num_items.mask |= CSE_ALifeInventoryItem::inventory_item_linear_null;
	}

	P.w_u8(num_items.common);
	if (!num_items.common)
	{
#ifdef DEBUG
		Msg("--- Number of sync items of inv item object is 0");
#endif // #ifdef DEBUG
		return;
	}

	net_Export_PH_Params(P, State, num_items);

	P.w_u8(!!object().PPhysicsShell() && object().PPhysicsShell()->isEnabled());	//not freezed
};

void CInventoryItem::save(NET_Packet& packet)
{
	packet.w_u16(m_ItemCurrPlace.value);
	packet.w_float(m_fCondition);
	packet.w_u8(IsDrawCost() ? 1 : 0);

	packet.w_stringZ(m_PrependName);
	packet.w_stringZ(m_AdditionalName);
	packet.w_stringZ(m_PrependDescription);
	packet.w_stringZ(m_AdditionalDescription);

	if (object().H_Parent())
	{
		packet.w_u8(0);
		return;
	}

	u8 _num_items = (u8)object().PHGetSyncItemsNumber();
	packet.w_u8(_num_items);
	object().PHSaveState(packet);
}

void CInventoryItem::load(IReader& packet)
{
	m_ItemCurrPlace.value = packet.r_u16();
	m_fCondition = packet.r_float();
	SetDrawCost(packet.r_u8() == 1);

	packet.r_stringZ(m_PrependName);
	SetPrependName(m_PrependName.c_str());
	packet.r_stringZ(m_AdditionalName);
	SetAdditionalName(m_AdditionalName.c_str());
	packet.r_stringZ(m_PrependDescription);
	SetPrependDescription(m_PrependDescription.c_str());
	packet.r_stringZ(m_AdditionalDescription);
	SetAdditionalDescription(m_AdditionalDescription.c_str());

	u8 tmp = packet.r_u8();
	if (!tmp)
	{
		return;
	}

	if (!object().PPhysicsShell())
	{
		object().setup_physic_shell();
		object().PPhysicsShell()->Disable();
	}

	object().PHLoadState(packet);
	object().PPhysicsShell()->Disable();
}

void CInventoryItem::Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CInventoryItem")
	{
		Object << m_ItemCurrPlace.value << m_fCondition << m_AdditionalDescription << m_IsUsedAdditionalDescription;

		if (!Object.IsSave() && m_IsUsedAdditionalDescription)
		{
			SetAdditionalDescription(m_AdditionalDescription.c_str());
		}

		u8 num_items;
		if (Object.IsSave()) {
			num_items = (u8)object().PHGetSyncItemsNumber();
		}
		Object << num_items;

		if (!num_items) {
			return;
		}

		if (!Object.IsSave()&&!object().PPhysicsShell()) {
			object().setup_physic_shell();
			object().PPhysicsShell()->Disable();
		}

		object().PHSerializeState(Object);
		if (!Object.IsSave()) {
			object().PPhysicsShell()->Disable();
		}
	}
}

///////////////////////////////////////////////
void CInventoryItem::PH_B_CrPr()
{};

void CInventoryItem::PH_I_CrPr()		// actions & operations between two phisic prediction steps
{};

#ifdef DEBUG
void CInventoryItem::PH_Ch_CrPr()
{};
#endif

void CInventoryItem::PH_A_CrPr()
{
	if (m_just_after_spawn)
	{
		VERIFY(object().Visual());
		IKinematics* K = object().Visual()->dcast_PKinematics();
		VERIFY(K);

		if (!object().PPhysicsShell())
		{
			Msg("! ERROR: PhysicsShell is nullptr, object [%s][%d]", object().cName().c_str(), object().ID());
			VERIFY2(0, "physical shell is nullptr");
			return;
		}

		if (!object().PPhysicsShell()->isFullActive())
		{
			K->CalculateBones_Invalidate();
			K->CalculateBones(true);
		}

		object().PPhysicsShell()->GetGlobalTransformDynamic(&object().XFORM());
		K->CalculateBones_Invalidate();
		K->CalculateBones(true);
#if	0
		Fbox bb = BoundingBox();
		DBG_OpenCashedDraw();
		Fvector c, r, p;
		bb.get_CD(c, r);
		XFORM().transform_tiny(p, c);
		DBG_DrawAABB(p, r, color_xrgb(255, 0, 0));
		//PPhysicsShell()->XFORM().transform_tiny(c);
		Fmatrix mm;
		PPhysicsShell()->GetGlobalTransformDynamic(&mm);
		mm.transform_tiny(p, c);
		DBG_DrawAABB(p, r, color_xrgb(0, 255, 0));
		DBG_ClosedCashedDraw(50000);
#endif
		object().spatial_move();
		m_just_after_spawn = false;

		VERIFY(!OnServer());

		object().PPhysicsShell()->get_ElementByStoreOrder(0)->Fix();
		object().PPhysicsShell()->SetIgnoreStatic();
	}
};


void CInventoryItem::Interpolate()
{
	net_updateInvData* p = NetSync();
	CPHSynchronize* pSyncObj = object().PHGetSyncItem(0);

	//simple linear interpolation...
	if (!object().H_Parent() && object().getVisible() && object().m_pPhysicsShell && !OnServer() && p->NET_IItem.size())
	{
		SPHNetState newState = p->NET_IItem.front().State;

		if (p->NET_IItem.size() >= 2)
		{
			float ret_interpolate = interpolate_states(p->NET_IItem.front(), p->NET_IItem.back(), newState);

			if (ret_interpolate >= 1.0f)
			{
				p->NET_IItem.pop_front();
				if (m_activated)
				{
#ifdef DEBUG
					Msg("Deactivating object [%d] after interpolation finish", object().ID());
#endif // #ifdef DEBUG
					object().processing_deactivate();
					m_activated = false;
				}
			}
		}

		pSyncObj->set_State(newState);
	}
}

float CInventoryItem::interpolate_states(net_update_IItem const& first, net_update_IItem const& last, SPHNetState& current)
{
	float ret_val = 0.0f;
	u32 CurTime = Device.dwTimeGlobal;

	if (CurTime == last.dwTimeStamp)
	{
		return 0.0f;
	}

	float factor = float(CurTime - last.dwTimeStamp) / float(last.dwTimeStamp - first.dwTimeStamp);

	ret_val = factor;
	if (factor > 1.0f)
	{
		factor = 1.0f;
	}
	else if (factor < 0.0f)
	{
		factor = 0.0f;
	}

	current.position.x = first.State.position.x + (factor * (last.State.position.x - first.State.position.x));
	current.position.y = first.State.position.y + (factor * (last.State.position.y - first.State.position.y));
	current.position.z = first.State.position.z + (factor * (last.State.position.z - first.State.position.z));
	current.previous_position = current.position;

	current.quaternion.slerp(first.State.quaternion, last.State.quaternion, factor);
	current.previous_quaternion = current.quaternion;
	return ret_val;
}

void CInventoryItem::reload(const char* section)
{
	inherited::reload(section);
}

void CInventoryItem::reinit()
{
	m_pInventory = nullptr;
	m_ItemCurrPlace.type = eItemPlaceUndefined;
}

bool CInventoryItem::can_kill() const
{
	return false;
}

CInventoryItem* CInventoryItem::can_kill(CInventory* inventory) const
{
	return nullptr;
}

const CInventoryItem* CInventoryItem::can_kill(const xr_vector<const CGameObject*>& items) const
{
	return nullptr;
}

CInventoryItem* CInventoryItem::can_make_killing(const CInventory* inventory) const
{
	return nullptr;
}

bool CInventoryItem::ready_to_kill() const
{
	return false;
}

void CInventoryItem::activate_physic_shell()
{
	CEntityAlive* E = object().H_Parent() != nullptr ? object().H_Parent()->cast_entity_alive() : nullptr;
	if (E == nullptr)
	{
		on_activate_physic_shell();
		return;
	};

	UpdateXForm();

	object().CPhysicsShellHolder::activate_physic_shell();
}

void CInventoryItem::setControlInertionFactor(float value)
{
	m_fControlInertionFactor = value;
}

bool CInventoryItem::GetHandsBasis(const Fmatrix& left, const Fmatrix& right, Fmatrix& result)
{
	Fvector D = Fvector().sub(left.c, right.c);
	const float magnitude = D.magnitude();
	const bool valid = !fis_zero(magnitude);

	if (valid)
	{
		D.div(magnitude);
	}
	else
	{
		D.set(0.0f, 0.0f, 1.0f);
	}

	Fvector R = Fvector().crossproduct(right.j, D);
	R.normalize();
	Fvector N = Fvector().crossproduct(D, R);
	N.normalize();

	result.set(R, N, D, right.c);
	return valid;
}

void CInventoryItem::UpdateXForm()
{
	CObject* obj_parent = object().H_Parent();
	if (obj_parent == nullptr)
	{
		return;
	}

	CEntityAlive* E = obj_parent->cast_entity_alive();
	if (E == nullptr)
	{
		return;
	}

	if (E->cast_base_monster())
	{
		return;
	}

	const CInventoryOwner* parent = E->cast_inventory_owner();

	if (parent == nullptr)
	{
		return;
	}

	if (parent->attached(this))
	{
		return;
	}

	R_ASSERT(E);
	IKinematics* V = PKinematics(E->Visual());
	VERIFY(V);

	u16 boneL = BI_NONE, boneR = BI_NONE, boneR2 = BI_NONE;
	E->g_WeaponBones(boneL, boneR, boneR2);
	if (boneR == BI_NONE)
	{
		return;
	}

	V->CalculateBones();
	E->XFORM().transform_tiny(object().Position(), V->LL_GetTransform(boneR).c);
}

#ifdef DEBUG_DRAW
void CInventoryItem::OnRender()
{
	if (bDebug && object().Visual())
	{
		if (!(dbg_net_Draw_Flags.is_any(dbg_draw_invitem)))
		{
			return;
		}

		Fvector bc = zero_vel, bd = zero_vel;
		object().Visual()->getVisData().box.get_CD(bc, bd);
		Fmatrix	M = object().XFORM();
		M.c.add(bc);
		Level().debug_renderer().draw_obb(M, bd, color_rgba(0, 0, 255, 255));
	};
}
#endif

DLL_Pure* CInventoryItem::_construct()
{
	m_object = smart_cast<CPhysicsShellHolder*>(this);
	VERIFY(m_object);

	return inherited::_construct();
}

void CInventoryItem::modify_holder_params(float& range, float& fov) const
{
	range *= ItemDesc().m_holder_range_modifier;
	fov *= ItemDesc().m_holder_fov_modifier;
}

bool CInventoryItem::NeedToDestroyObject() const
{
	if (IsGameTypeSingle())
	{
		return false;
	}

	if (GameID() & eGameIDCaptureTheArtefact)
	{
		return false;
	}

	if (object().Remote())
	{
		return false;
	}

	if (TimePassedAfterIndependant() > ITEM_REMOVE_TIME)
	{
		return true;
	}

	return false;
}

ALife::_TIME_ID	 CInventoryItem::TimePassedAfterIndependant() const
{
	if (!object().H_Parent() && m_dwItemIndependencyTime != 0)
	{
		return Level().timeServer() - m_dwItemIndependencyTime;
	}
	else
	{
		return 0;
	}
}

bool CInventoryItem::CanTrade() const
{
	bool res = true;
#pragma todo("Dima to Andy : why CInventoryItem::CanTrade can be called for the item, which doesn't have owner?")
	if (m_pInventory != nullptr)
	{
		res = inventory_owner().AllowItemToTrade(this, m_ItemCurrPlace);
	}

	return (res && m_flags.test(FCanTrade) && !IsQuestItem());
}

Frect CInventoryItem::GetKillMsgRect() const
{
	float x, y, w, h;

	x = pSettings->read_if_exists<float>(m_object->cNameSect(), "kill_msg_x", 0.0f);
	y = pSettings->read_if_exists<float>(m_object->cNameSect(), "kill_msg_y", 0.0f);
	w = pSettings->read_if_exists<float>(m_object->cNameSect(), "kill_msg_width", 0.0f);
	h = pSettings->read_if_exists<float>(m_object->cNameSect(), "kill_msg_height", 0.0f);

	return Frect().set(x, y, w, h);
}

Irect CInventoryItem::GetInvGridRect() const
{
	return m_inv_rect;
}

void CInventoryItem::SetInvGridRect(const Irect& rect)
{
	m_inv_rect.set(rect);
}

void CInventoryItem::SetInvGridRect(u32 x, u32 y, u32 w, u32 h)
{
	SetInvGridRect(Irect().set(x, y, w, h));
}

Irect CInventoryItem::GetUpgrIconRect() const
{
	u32 x = pSettings->read_if_exists<u32>(m_object->cNameSect(), "upgr_icon_x", 0);
	u32 y = pSettings->read_if_exists<u32>(m_object->cNameSect(), "upgr_icon_y", 0);
	u32 w = pSettings->read_if_exists<u32>(m_object->cNameSect(), "upgr_icon_width", 0);
	u32 h = pSettings->read_if_exists<u32>(m_object->cNameSect(), "upgr_icon_height", 0);

	return Irect().set(x, y, w, h);
}

bool CInventoryItem::IsNecessaryItem(CInventoryItem* item)
{
	return IsNecessaryItem(item->object().cNameSect());
};

bool CInventoryItem::IsInvalid() const
{
	return object().getDestroy() || GetDropManual();
}

void CInventoryItem::setCost(u32 nValue)
{
	m_cost = nValue;
}

void CInventoryItem::setWeight(float value)
{
	m_weight = value;
}

ALife::_OBJECT_ID CInventoryItem::object_id() const
{
	return object().ID();
}

ALife::_OBJECT_ID CInventoryItem::parent_id() const
{
	return (object().H_Parent()) ? object().H_Parent()->ID() : ALife::INVALID_OBJECT_ID;
}

void CInventoryItem::SetDropManual(bool val)
{
	m_flags.set(FdropManual, val);

#ifdef DEBUG
	if (!IsGameTypeSingle())
	{
		if (!!m_name)
		{
			Msg("! WARNING: trying to set drop manual flag to item [%d][%s] to %d", object_id(), m_name.c_str(), val);
		}
	}
#endif // #ifdef DEBUG
	if (!IsGameTypeSingle())
	{
		if (val == true)
		{
			DenyTrade();
		}
		else
		{
			AllowTrade();
		}
	}
}

bool CInventoryItem::has_network_synchronization() const
{
	return false;
}

u32 CInventoryItem::Cost() const
{
	u32 AllCosts = m_cost;

	for (const auto& [id, item] : m_children)
	{
		AllCosts += item->Cost();
	}

	return AllCosts;
}

float CInventoryItem::Weight() const
{
	float AllWeight = m_weight;

	for (const auto& [id, item] : m_children)
	{
		AllWeight += item->Weight();
	}

	return AllWeight;
}