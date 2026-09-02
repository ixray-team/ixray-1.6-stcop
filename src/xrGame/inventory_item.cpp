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
#include "ai_object_location.h"
#include "object_broker.h"

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
	m_flags.set(Fbelt, false);
	m_flags.set(Fruck, true);
	m_flags.set(FRuckDefault, true);

	SetDropManual(false);

	m_flags.set(FCanTake, true);
	m_can_trade = true;
	m_flags.set(FCanTrade, m_can_trade);
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
	SetDrawCost(READ_IF_EXISTS(pSettings, r_bool, section, "is_draw_cost", true));

	// Highlight separated by delimeter ',' related item sections on mouseover from the actor's inventory
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

	m_parse_params.m_chances.clear();
	m_parse_params.m_items.clear();

	if (pSettings->line_exist(section, "parse_spawn_items") && pSettings->line_exist(section, "parse_spawn_chances"))
	{
		shared_str SpawnList = pSettings->r_string(section, "parse_spawn_items");
		shared_str ChanceList = pSettings->r_string(section, "parse_spawn_chances");

		int Count = _GetItemCount(SpawnList.c_str());
		int Count2 = _GetItemCount(ChanceList.c_str());

		string256 sItem = {};

		for (int i = 0; i < Count; ++i)
		{
			m_parse_params.m_items.push_back(_GetItem(SpawnList.c_str(), i, sItem));
		}

		for (int i = 0; i < Count2; ++i)
		{
			m_parse_params.m_chances.push_back(atof(_GetItem(ChanceList.c_str(), i, sItem)));
		}
	}

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
	u32 sl = READ_IF_EXISTS(pSettings, r_u32, section, "slot", -1);
	m_ItemCurrPlace.base_slot_id = (sl == -1) ? 0 : (sl + 1);

	m_Description = g_pStringTable->translate(READ_IF_EXISTS(pSettings, r_string, section, "description", ""));

	m_flags.set(Fbelt, READ_IF_EXISTS(pSettings, r_bool, section, "belt", false));
	m_can_trade = READ_IF_EXISTS(pSettings, r_bool, section, "can_trade", true);
	m_flags.set(FCanTake, READ_IF_EXISTS(pSettings, r_bool, section, "can_take", true));
	m_flags.set(FCanTrade, m_can_trade);
	m_flags.set(FCanStack, READ_IF_EXISTS(pSettings, r_bool, section, "can_stack", true));
	m_flags.set(FIsQuestItem, READ_IF_EXISTS(pSettings, r_bool, section, "quest_item", false));

	// Added by Axel, to enable optional condition use on any item
	m_flags.set(FUsingCondition, READ_IF_EXISTS(pSettings, r_bool, section, "use_condition", false));

	m_highlight_equipped = !!READ_IF_EXISTS(pSettings, r_bool, section, "highlight_equipped", false);

	if (BaseSlot() != NO_ACTIVE_SLOT || Belt())
	{
		bool defaultRuck = (BaseSlot() != NO_ACTIVE_SLOT && !Belt() && cast_hud_item()) ? false : true;
		m_flags.set(FRuckDefault, READ_IF_EXISTS(pSettings, r_bool, section, "default_to_ruck", defaultRuck));
		m_flags.set(FAllowSprint, READ_IF_EXISTS(pSettings, r_bool, section, "sprint_allowed", true));
		m_fControlInertionFactor = READ_IF_EXISTS(pSettings, r_float, section, "control_inertion_factor", 1.0f);
	}
	m_icon_name = READ_IF_EXISTS(pSettings, r_string, section, "icon_name", nullptr);

	u32 inv_grid_x = pSettings->r_u32(m_object->cNameSect(), "inv_grid_x");
	u32 inv_grid_y = pSettings->r_u32(m_object->cNameSect(), "inv_grid_y");
	u32 inv_grid_width = pSettings->r_u32(m_object->cNameSect(), "inv_grid_width");
	u32 inv_grid_height = pSettings->r_u32(m_object->cNameSect(), "inv_grid_height");
	ScaleIcon = READ_IF_EXISTS(pSettings, r_float, m_object->cNameSect(), "inv_scale", 1.0f);
	IconsTexture = READ_IF_EXISTS(pSettings, r_string, section, "icons_texture", nullptr);

	m_inv_rect.set(inv_grid_x, inv_grid_y, inv_grid_width, inv_grid_height);

	ReadCustomTextAndMarks(section);
	Read3dStaticsData(section);
}

void CInventoryItem::SetAdditionalDescription(const char* additionalDescription)
{
	m_AdditionalDescription = additionalDescription;
	m_IsUsedAdditionalDescription = xr_strcmp(m_AdditionalDescription, "") != 0;
	RebuildExtendedDescription();
}

void CInventoryItem::SetPrependDescription(const char* prependDescription)
{
	m_PrependDescription = prependDescription ? prependDescription : "";
	m_IsUsedPrependDescription = prependDescription && prependDescription[0];
	RebuildExtendedDescription();
}

void CInventoryItem::RebuildExtendedDescription()
{
	xr_string description;

	auto appendDescriptionBlock = [&description](const char* text)
	{
		if (!text || !text[0])
			return;

		if (!description.empty())
			description += "\\n\\n";

		description += text;
	};

	if (m_IsUsedPrependDescription)
		appendDescriptionBlock(m_PrependDescription.c_str());

	appendDescriptionBlock(m_Description.c_str());

	if (m_IsUsedAdditionalDescription)
		appendDescriptionBlock(m_AdditionalDescription.c_str());

	m_ExtendedUnionDescription = description.c_str();
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
	m_custom_text = READ_IF_EXISTS(pSettings, r_string, section, "item_custom_text", nullptr);
	m_custom_text_offset = READ_IF_EXISTS(pSettings, r_fvector2, section, "item_custom_text_offset", Fvector2().set(0.f, 0.f));
	m_custom_text_auto_uses = READ_IF_EXISTS(pSettings, r_bool, section, "item_custom_text_auto_uses", false);
	m_custom_text_anchor = ParseInvCellAnchor(
		READ_IF_EXISTS(pSettings, r_string, section, "item_custom_text_anchor", "bottom_right"));

	if (pSettings->line_exist(section, "item_custom_text_font"))
	{
		shared_str font_str = pSettings->r_string(section, "item_custom_text_font");
		m_custom_text_font = UI().Font().GetFont(font_str);
		m_custom_text_font = nullptr;
	}

	if (pSettings->line_exist(section, "item_custom_text_clr_inv"))
	{
		m_custom_text_clr_inv = pSettings->r_color(section, "item_custom_text_clr_inv");
	}
	else {
		m_custom_text_clr_inv = 0;
	}

	m_custom_mark_texture = READ_IF_EXISTS(pSettings, r_string, section, "item_custom_mark_texture", nullptr);
	m_custom_mark = READ_IF_EXISTS(pSettings, r_bool, section, "item_custom_mark", false);
	m_custom_mark_offset = READ_IF_EXISTS(pSettings, r_fvector2, section, "item_custom_mark_offset", Fvector2().set(0.f, 0.f));
	m_custom_mark_size = READ_IF_EXISTS(pSettings, r_fvector2, section, "item_custom_mark_size", Fvector2().set(0.f, 0.f));
	m_custom_mark_clr = READ_IF_EXISTS(pSettings, r_color, section, "item_custom_mark_clr", 0);
	m_custom_mark_anchor = ParseInvCellAnchor(
		READ_IF_EXISTS(pSettings, r_string, section, "item_custom_mark_anchor", "bottom_right"));
}

void CInventoryItem::Read3dStaticsData(const char* section)
{
	m_3d_static_visual_name = READ_IF_EXISTS(pSettings, r_string, section, "3d_static_visual_name", *object().cNameVisual());

	m_3d_static_rotate = READ_IF_EXISTS(pSettings, r_fvector3, section, "3d_static_rotate", m_3d_static_rotate.set(0,0,0));
	m_3d_static_rotate.mul(M_PI / 180.0f);

	m_3d_static_scale = READ_IF_EXISTS(pSettings, r_float, section, "3d_static_scale", 1.f);
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

	if (IsUsedExtendedDescription())
		RebuildExtendedDescription();
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
}

void CInventoryItem::OnEvent(NET_Packet& P, u16 type)
{
	switch (type)
	{
	case GE_ADDON_ATTACH:
	{
		u16 ItemID;
		P.r_u16(ItemID);
		CObject* finded = Level().Objects.net_Find(ItemID);
		PIItem ItemToAttach = finded != nullptr ? finded->cast_inventory_item() : nullptr;
		if (ItemToAttach == nullptr)
		{
			break;
		}

		Attach(ItemToAttach, true);
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
	}
}

//процесс отсоединения вещи заключается в спауне новой вещи 
//в инвентаре и установке соответствующих флагов в родительском
//объекте, поэтому функция должна быть переопределена
bool CInventoryItem::Detach(const char* item_section_name, bool b_spawn_item)
{
	if (OnClient())
	{
		return true;
	}

	if (b_spawn_item)
	{
		CSE_Abstract* D = F_entity_Create(item_section_name);
		R_ASSERT(D);
		CSE_ALifeDynamicObject* l_tpALifeDynamicObject = D->cast_alife_dynamic_object();
		R_ASSERT(l_tpALifeDynamicObject);

		l_tpALifeDynamicObject->m_tNodeID = (g_dedicated_server) ? u32(-1) : object().ai_location().level_vertex_id();

		// Fill
		D->s_name = item_section_name;
		D->set_name_replace("");

		D->s_RP = 0xff;
		D->ID = 0xffff;
		if (IsGameTypeSingle())
		{
			D->ID_Parent = u16(object().H_Parent()->ID());
		}
		else	// i'm not sure this is right
		{		// but it is simpliest way to avoid exception in MP BuyWnd... [Satan]
			if (object().H_Parent())
			{
				D->ID_Parent = u16(object().H_Parent()->ID());
			}
			else
			{
				D->ID_Parent = 0;
			}
		}

		D->ID_Phantom = 0xffff;
		D->o_Position = object().Position();
		D->s_flags.assign(M_SPAWN_OBJECT_LOCAL);
		D->RespawnTime = 0;

		// Send
		NET_Packet P;
		D->Spawn_Write(P, true);
		Level().Send(P, net_flags(true));
		// Destroy
		F_entity_Destroy(D);
	}
	return true;
}

/////////// network ///////////////////////////////
bool CInventoryItem::net_Spawn(CSE_Abstract* DC)
{
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

void CInventoryItem::save(NET_Packet& packet)
{
	packet.w_u16(m_ItemCurrPlace.value);
	packet.w_float(m_fCondition);

	packet.w_stringZ(m_AdditionalDescription);
	packet.w_u8(m_IsUsedAdditionalDescription ? 1 : 0);

	if (object().H_Parent())
	{
		packet.w_u8(0);
		return;
	}

	u8 _num_items = (u8)object().PHGetSyncItemsNumber();
	packet.w_u8(_num_items);
	object().PHSaveState(packet);

	packet.w_u8(IsDrawCost() ? 1 : 0);
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
			stpk.w_stringZ(m_AdditionalDescription);
			stpk.w_u8(m_IsUsedAdditionalDescription ? 1 : 0);
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

void CInventoryItem::load(IReader& packet)
{
	m_ItemCurrPlace.value = packet.r_u16();
	m_fCondition = packet.r_float();
	packet.r_stringZ(m_AdditionalDescription);
	m_IsUsedAdditionalDescription = packet.r_u8() == 1 ? true : false;

	if (m_IsUsedAdditionalDescription)
	{
		SetAdditionalDescription(m_AdditionalDescription.c_str());
	}

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

	SetDrawCost(packet.r_u8() == 1);
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

	m_holder_range_modifier = READ_IF_EXISTS(pSettings, r_float, section, "holder_range_modifier", 1.f);
	m_holder_fov_modifier = READ_IF_EXISTS(pSettings, r_float, section, "holder_fov_modifier", 1.f);
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

void CInventoryItem::UpdateXForm()
{
	CObject* obj_parent = object().H_Parent();
	if (obj_parent == nullptr)
	{
		return;
	}

	// Get access to entity and its visual
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

	if (parent->use_simplified_visual())
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

	// Get matrices
	u16	boneL = BI_NONE, boneR = BI_NONE, boneR2 = BI_NONE;
	E->g_WeaponBones(boneL, boneR, boneR2);
	if (boneR == BI_NONE)	return;
	//	if ((HandDependence() == hd1Hand) || (STATE == eReload) || (!E->g_Alive()))
	//		boneL = boneR2;
#pragma todo("TO ALL: serious performance problem")
	V->CalculateBones();
	Fmatrix& mL = V->LL_GetTransform(boneL);
	Fmatrix& mR = V->LL_GetTransform(boneR);
	// Calculate
	Fmatrix			mRes;
	Fvector			R, D, N;
	D.sub(mL.c, mR.c);	D.normalize_safe();

	if (fis_zero(D.magnitude()))
	{
		mRes.set(E->XFORM());
		mRes.c.set(mR.c);
	}
	else
	{
		D.normalize();
		R.crossproduct(mR.j, D);

		N.crossproduct(D, R);
		N.normalize();

		mRes.set(R, N, D, mR.c);
		mRes.mulA_43(E->XFORM());
	}

	//	UpdatePosition	(mRes);
	object().Position().set(mRes.c);
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
	range *= m_holder_range_modifier;
	fov *= m_holder_fov_modifier;
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

	x = READ_IF_EXISTS(pSettings, r_float, m_object->cNameSect(), "kill_msg_x", 0.0f);
	y = READ_IF_EXISTS(pSettings, r_float, m_object->cNameSect(), "kill_msg_y", 0.0f);
	w = READ_IF_EXISTS(pSettings, r_float, m_object->cNameSect(), "kill_msg_width", 0.0f);
	h = READ_IF_EXISTS(pSettings, r_float, m_object->cNameSect(), "kill_msg_height", 0.0f);

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
	u32 x = READ_IF_EXISTS(pSettings, r_u32, m_object->cNameSect(), "upgr_icon_x", 0);
	u32 y = READ_IF_EXISTS(pSettings, r_u32, m_object->cNameSect(), "upgr_icon_y", 0);
	u32 w = READ_IF_EXISTS(pSettings, r_u32, m_object->cNameSect(), "upgr_icon_width", 0);
	u32 h = READ_IF_EXISTS(pSettings, r_u32, m_object->cNameSect(), "upgr_icon_height", 0);

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

u16 CInventoryItem::object_id()const
{
	return object().ID();
}

u16 CInventoryItem::parent_id() const
{
	return (object().H_Parent()) ? object().H_Parent()->ID() : u16(-1);
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
