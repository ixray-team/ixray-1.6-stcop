////////////////////////////////////////////////////////////////////////////
//	Module 		: eatable_item.cpp
//	Created 	: 24.03.2003
//  Modified 	: 29.01.2004
//	Author		: Yuri Dobronravin
//	Description : Eatable item
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "eatable_item.h"
#include "xrMessages.h"
#include "physic_item.h"
#include "Level.h"
#include "entity_alive.h"
#include "EntityCondition.h"
#include "InventoryOwner.h"
#include "UIGameCustom.h"
#include "ui/UIActorMenu.h"
#include "HUDAnimItem.h"
#include "Actor.h"
#include "ActorCondition.h"
#include "Inventory.h"
#include "CustomOutfit.h"
#include "ai_object_location.h"

CEatableItem::CEatableItem()
{
	m_physic_item = nullptr;
	m_fWeightFull = 0;
	m_fWeightEmpty = 0;

	m_iMaxUses = 1;
	m_bRemoveAfterUse = true;
	m_bConsumeChargeOnUse = true;
}

CEatableItem::~CEatableItem()
{
}

DLL_Pure* CEatableItem::_construct()
{
	m_physic_item = smart_cast<CPhysicItem*>(this);
	return			(inherited::_construct());
}

void CEatableItem::Load(LPCSTR section)
{
	inherited::Load(section);

	m_bUseHUDAnim = pSettings->line_exist(section, "hud");

	m_iMaxUses = READ_IF_EXISTS(pSettings, r_u8, section, "eat_portions_num", 1);

	UseText = READ_IF_EXISTS(pSettings, r_string, section, "use_text", "st_use");

	m_iPortionsMarker = m_iMaxUses;

	m_bRemoveAfterUse = READ_IF_EXISTS( pSettings, r_bool, section, "remove_after_use", TRUE );
	m_eat_condition = READ_IF_EXISTS(pSettings, r_float, section, "eat_condition", 1);
	m_bConsumeChargeOnUse = READ_IF_EXISTS(pSettings, r_bool, section, "consume_charge_on_use", TRUE);
	m_fWeightFull = m_weight;
	m_fWeightEmpty = READ_IF_EXISTS(pSettings, r_float, section, "empty_weight", 0.0f);

	if (m_bUseHUDAnim)
	{
		HudSect = pSettings->r_string(section, "hud");

		if (pSettings->line_exist(HudSect, "timing"))
			m_siTiming = pSettings->r_s32(HudSect, "timing");

		m_siTimingEarly = m_siTiming;

		if (pSettings->line_exist(HudSect, "timing_early"))
			m_siTimingEarly = pSettings->r_s32(HudSect, "timing_early");

		if (pSettings->line_exist(HudSect, "timing_trash"))
			m_siTimingTrash = pSettings->r_s32(HudSect, "timing_trash");

		if (pSettings->line_exist(HudSect, "no_trash"))
			m_bNoTrash = pSettings->r_bool(HudSect, "no_trash");
	}

	if (pSettings->line_exist(section, "trash_object"))
		m_sTrashSect = pSettings->r_string(section, "trash_object");
}

void CEatableItem::load(IReader& packet)
{
	inherited::load(packet);
	m_iPortionsMarker = packet.r_u8();
}

void CEatableItem::save(NET_Packet& packet)
{
	inherited::save(packet);
	packet.w_u8((u8)m_iPortionsMarker);
}

BOOL CEatableItem::net_Spawn(CSE_Abstract* DC)
{
	if (!inherited::net_Spawn(DC)) return FALSE;
	return TRUE;
}

bool CEatableItem::Useful() const
{
	if (!inherited::Useful()) return false;

	//проверить не все ли еще съедено
	if (GetRemainingUses() == 0 && CanDelete())
		return false;

	return true;
}

void CEatableItem::OnH_A_Independent()
{
	inherited::OnH_A_Independent();
	if (!Useful()) {
		if (object().Local() && OnServer())	object().DestroyObject();
	}
}

void CEatableItem::OnH_B_Independent(bool just_before_destroy)
{
	if (!Useful())
	{
		object().setVisible(FALSE);
		object().setEnabled(FALSE);
		if (m_physic_item)
			m_physic_item->m_ready_to_destroy = true;
	}
	inherited::OnH_B_Independent(just_before_destroy);
}

bool CEatableItem::UseBy(CEntityAlive* entity_alive)
{
	if (entity_alive->cast_actor() == nullptr || entity_alive->cast_actor() != nullptr && m_siTimingEarly == 0)
	{
		SMedicineInfluenceValues	V;
		V.Load(m_physic_item->cNameSect());

		CInventoryOwner* IO = smart_cast<CInventoryOwner*>(entity_alive);
		R_ASSERT(IO);
		R_ASSERT(m_pInventory == IO->m_inventory);
		R_ASSERT(object().H_Parent()->ID() == entity_alive->ID());

		entity_alive->conditions().ApplyInfluence(V, m_physic_item->cNameSect());

		for (u8 i = 0; i < (u8)eBoostMaxCount; i++)
		{
			if (pSettings->line_exist(m_physic_item->cNameSect().c_str(), ef_boosters_section_names[i]))
			{
				SBooster B;
				B.Load(m_physic_item->cNameSect(), (EBoostParams)i);
				entity_alive->conditions().ApplyBooster(B, m_physic_item->cNameSect());
			}
		}

		if (m_iPortionsMarker > 0)
		{
			--m_iPortionsMarker;
			bNeedUpdateIcon = true;
		}
		else
			m_iPortionsMarker = 0;

		if (m_iMaxUses > 1)
			SetRemainingUses(m_iPortionsMarker);

		if (READ_IF_EXISTS(pSettings, r_bool, m_section_id, "anabiotic", false))
		{
			luabind::functor<void> funct;
			if (ai().script_engine().functor("anabiotic.anabiotic", funct))
				funct("");
		}
	}

	if (m_bUseHUDAnim && entity_alive->cast_actor() != nullptr)
	{
		string64 string = {};
		const char* base_name = "hud";

		if (Actor()->GetOutfit() != nullptr)
		{
			const char* outfit_name = entity_alive->cast_inventory_owner()->inventory().ItemFromSlot(OUTFIT_SLOT)->m_section_id.c_str();
			xr_sprintf(string, "hud_%s", outfit_name);
			if (pSettings->line_exist(m_section_id, string))
				base_name = string;
		}

		if (m_iPortionsMarker == 1 && m_iMaxUses > 1)
			xr_sprintf(string, "%s_last", base_name);
		else
			xr_sprintf(string, "%s", base_name);

		HudSect = READ_IF_EXISTS(pSettings, r_string, m_section_id, string, HudSect);

		if (pSettings->line_exist(HudSect, "timing"))
			m_siTiming = pSettings->r_s32(HudSect, "timing");

		m_siTimingEarly = m_siTiming;

		if (pSettings->line_exist(HudSect, "timing_early"))
			m_siTimingEarly = pSettings->r_s32(HudSect, "timing_early");

		if (pSettings->line_exist(HudSect, "timing_trash"))
			m_siTimingTrash = pSettings->r_s32(HudSect, "timing_trash");

		if (pSettings->line_exist(HudSect, "no_trash"))
			m_bNoTrash = pSettings->r_bool(HudSect, "no_trash");

		CurrentGameUI()->HideActorMenu();
		Actor()->set_inventory_disabled(true);
		Actor()->eateable_to_delete = this;
		m_bIsEated = false;
		m_siStartTime = 0;
		CHUDAnimItem::PlayHudAnim(HudSect, "anm_use", "snd_using");
	}

	return true;
}

void CEatableItem::UpdateEatable()
{
	if (smart_cast<CHUDAnimItem*>(Actor()->inventory().ActiveItem()) == nullptr)
		return;

	if (m_bIsEated)
		return;

	auto PrepareEffects = [&]()
	{
		SMedicineInfluenceValues V;
		V.Load(m_physic_item->cNameSect());

		Actor()->conditions().ApplyInfluence(V, m_physic_item->cNameSect());

		for (u8 i = 0; i < (u8)eBoostMaxCount; i++)
		{
			if (pSettings->line_exist(m_physic_item->cNameSect().c_str(), ef_boosters_section_names[i]))
			{
				SBooster B;
				B.Load(m_physic_item->cNameSect(), (EBoostParams)i);
				Actor()->conditions().ApplyBooster(B, m_physic_item->cNameSect());
			}
		}

		if (m_iPortionsMarker > 0)
		{
			--m_iPortionsMarker;
			bNeedUpdateIcon = true;
		}
		else
			m_iPortionsMarker = 0;

		if (m_iMaxUses > 1)
			SetRemainingUses(m_iPortionsMarker);

		if (READ_IF_EXISTS(pSettings, r_bool, m_section_id, "anabiotic", false))
		{
			g_player_hud->detach_item(smart_cast<CHUDAnimItem*>(Actor()->inventory().ActiveItem()));
			g_player_hud->RemoveHudItem(HudSect);
			Actor()->inventory().Activate(NO_ACTIVE_SLOT, true);

			luabind::functor<void> funct;
			if (ai().script_engine().functor("anabiotic.anabiotic", funct))
				funct("");
		}

		Actor()->set_inventory_disabled(false);
		m_bIsEated = true;
		Actor()->eateable_to_delete = nullptr;

		if (m_iPortionsMarker == 0)
			object().DestroyObject();
	};

	if (m_siStartTime == 0)
		m_siStartTime = Device.dwTimeGlobal;

	if (!m_bTrashSpawned && m_siTimingTrash > 0 && !m_bNoTrash)
	{
		m_bTrashSpawned = true;
		Actor()->eater_manager.trash_sect = m_sTrashSect;
		Actor()->eater_manager.start_time = m_siStartTime;
		Actor()->eater_manager.trash_time = m_siTimingTrash;
	}

	if (!m_bIsEated)
	{
		if (Device.dwTimeGlobal - m_siStartTime > m_siTiming)
			PrepareEffects();
		else if (Device.dwTimeGlobal - m_siStartTime > m_siTimingEarly)
			PrepareEffects();
	}
}

float CEatableItem::Weight() const
{
	float res = inherited::Weight();

	if (IsUsingCondition())
	{
		float net_weight = m_fWeightFull - m_fWeightEmpty;
		float use_weight = m_iMaxUses > 0 ? (net_weight / m_iMaxUses) : 0.f;

		res = m_fWeightEmpty + (GetRemainingUses() * use_weight);
	}

	return res;
}

Irect CEatableItem::GetInvGridRect() const
{
	Irect rect = inherited::GetInvGridRect();

	if (m_iMaxUses > 1 && m_iPortionsMarker < m_iMaxUses)
	{
		u32 x = rect.x1;
		u32 y = rect.y1;
		u32 w = rect.x2;
		u32 h = rect.y2;
		string32 name = {};

		xr_sprintf(name, "inv_grid_x_%d", m_iPortionsMarker);
		if (pSettings->line_exist(m_section_id, name))
			x = pSettings->r_u32(m_section_id, name);

		xr_sprintf(name, "inv_grid_y_%d", m_iPortionsMarker);
		if (pSettings->line_exist(m_section_id, name))
			y = pSettings->r_u32(m_section_id, name);

		xr_sprintf(name, "inv_grid_width_%d", m_iPortionsMarker);
		if (pSettings->line_exist(m_section_id, name))
			w = pSettings->r_u32(m_section_id, name);

		xr_sprintf(name, "inv_grid_height_%d", m_iPortionsMarker);
		if (pSettings->line_exist(m_section_id, name))
			h = pSettings->r_u32(m_section_id, name);

		rect.set(x, y, w, h);
	}

	return rect;
}

using namespace luabind;

#pragma optimize("s",on)
void CEatableItem::script_register(lua_State* L)
{
	module(L)
		[
			class_<CEatableItem>("CEatableItem")
				.def("Empty", &CEatableItem::Empty)
				.def("CanDelete", &CEatableItem::CanDelete)
				.def("GetMaxUses", &CEatableItem::GetMaxUses)
				.def("GetRemainingUses", &CEatableItem::GetRemainingUses)
				.def("SetRemainingUses", &CEatableItem::SetRemainingUses)

				.def_readwrite("m_bRemoveAfterUse", &CEatableItem::m_bRemoveAfterUse)
				.def_readwrite("m_fWeightFull", &CEatableItem::m_fWeightFull)
				.def_readwrite("m_fWeightEmpty", &CEatableItem::m_fWeightEmpty)

				.def("Weight", &CEatableItem::Weight)
				.def("Cost", &CEatableItem::Cost)
		];
}