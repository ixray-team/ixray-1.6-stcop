////////////////////////////////////////////////////////////////////////////
//	Module 		: eatable_item.cpp
//	Created 	: 24.03.2003
//  Modified 	: 29.01.2004
//	Author		: Yuri Dobronravin
//	Description : Eatable item
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
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

DLL_Pure *CEatableItem::_construct	()
{
	m_physic_item	= smart_cast<CPhysicItem*>(this);
	return			(inherited::_construct());
}

void CEatableItem::Load(LPCSTR section)
{
	inherited::Load(section);

	bUseHUDAnim = (pSettings->line_exist(section, "anm_use"));

	if (pSettings->line_exist(section, "eat_portions_num"))
	{
		m_iMaxUses = pSettings->r_s32(section, "eat_portions_num");
	}
	else
	{
		m_iMaxUses = READ_IF_EXISTS(pSettings, r_u8, section, "max_uses", 1);
	}

	UseText = READ_IF_EXISTS(pSettings, r_string, section, "use_text", "st_use");

	if (m_iMaxUses < 1)
		m_iMaxUses = 1;

	m_iPortionsMarker = m_iMaxUses;

	m_bRemoveAfterUse = READ_IF_EXISTS( pSettings, r_bool, section, "remove_after_use", TRUE );
	m_bConsumeChargeOnUse = READ_IF_EXISTS(pSettings, r_bool, section, "consume_charge_on_use", TRUE);
	m_fWeightFull = m_weight;
	m_fWeightEmpty = READ_IF_EXISTS(pSettings, r_float, section, "empty_weight", 0.0f);
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
	if(!inherited::Useful()) return false;

	//проверить не все ли еще съедено
	if (GetRemainingUses() == 0 && CanDelete()) return false;

	return true;
}

void CEatableItem::OnH_A_Independent() 
{
	inherited::OnH_A_Independent();
	if(!Useful()) {
		if (object().Local() && OnServer())	object().DestroyObject	();
	}	
}

void CEatableItem::OnH_B_Independent(bool just_before_destroy)
{
	if(!Useful()) 
	{
		object().setVisible(FALSE);
		object().setEnabled(FALSE);
		if (m_physic_item)
			m_physic_item->m_ready_to_destroy	= true;
	}
	inherited::OnH_B_Independent(just_before_destroy);
}

bool CEatableItem::UseBy (CEntityAlive* entity_alive)
{
	SMedicineInfluenceValues	V;
	V.Load						(m_physic_item->cNameSect());

	CInventoryOwner* IO	= smart_cast<CInventoryOwner*>(entity_alive);
	R_ASSERT		(IO);
	R_ASSERT		(m_pInventory==IO->m_inventory);
	R_ASSERT		(object().H_Parent()->ID()==entity_alive->ID());

	entity_alive->conditions().ApplyInfluence(V, m_physic_item->cNameSect());

	for(u8 i = 0; i<(u8)eBoostMaxCount; i++)
	{
		if(pSettings->line_exist(m_physic_item->cNameSect().c_str(), ef_boosters_section_names[i]))
		{
			SBooster B;
			B.Load(m_physic_item->cNameSect(), (EBoostParams)i);
			entity_alive->conditions().ApplyBooster(B, m_physic_item->cNameSect());
		}
	}

	if (m_iPortionsMarker > 0)
		--m_iPortionsMarker;
	else
		m_iPortionsMarker = 0;

	if (bUseHUDAnim)
	{
		CHUDAnimItem::PlayHudAnim(m_section_id.c_str(), "anm_use");
	}

	return true;
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

using namespace luabind;

#pragma optimize("s",on)
void CEatableItem::script_register(lua_State *L)
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