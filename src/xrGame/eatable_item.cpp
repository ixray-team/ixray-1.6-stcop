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
#include "Inventory.h"
#include "Actor.h"
#include "ActorCondition.h"
#include "Descs/EatableEffectsDesc.h"

DLL_Pure* CEatableItem::_construct()
{
	m_physic_item = smart_cast<CPhysicItem*>(this);
	return inherited::_construct();
}

void CEatableItem::Load(const char* section)
{
	inherited::Load(section);
	CurrentEatableDesc = &SEatableItemDesc::Registry::Get(section);

	m_iRemainingUses = GetMaxUses();

	m_bRemoveAfterUse = READ_IF_EXISTS(pSettings, r_bool, section, "remove_after_use", true);
	m_fWeightFull = m_weight;
	m_fWeightEmpty = READ_IF_EXISTS(pSettings, r_float, section, "empty_weight", 0.0f);

	if (IsUsingCondition())
	{
		if (GetMaxUses() > 0)
		{
			SetCondition((float)(m_iRemainingUses / GetMaxUses()));
		}
		else
		{
			SetCondition(0.0f);
		}
	}
}

void CEatableItem::load(IReader& packet)
{
	inherited::load(packet);
	m_iRemainingUses = packet.r_u8();
}

void CEatableItem::save(NET_Packet& packet)
{
	inherited::save(packet);
	packet.w_u8(m_iRemainingUses);
}

void CEatableItem::Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CEatableItem")
	{
		inherited::Serialize(Object);
		Object << m_iRemainingUses;
	}
}

bool CEatableItem::net_Spawn(CSE_Abstract* DC)
{
	if (!inherited::net_Spawn(DC))
	{
		return false;
	}

	if (IsUsingCondition())
	{
		if (GetMaxUses() > 0)
		{
			SetCondition((float)(m_iRemainingUses / GetMaxUses()));
		}
		else
		{
			SetCondition(0.0f);
		}
	}

	return true;
};

bool CEatableItem::Useful() const
{
	if (!inherited::Useful())
	{
		return false;
	}

	//проверить не все ли еще съедено
	if (m_iRemainingUses == 0 && CanDelete())
	{
		return false;
	}

	return true;
}

void CEatableItem::OnH_A_Independent()
{
	inherited::OnH_A_Independent();
	if (!Useful())
	{
		if (object().Local() && OnServer())
		{
			object().DestroyObject();
		}
	}
}

void CEatableItem::OnH_B_Independent(bool just_before_destroy)
{
	if (!Useful())
	{
		object().setVisible(false);
		object().setEnabled(false);
		if (m_physic_item != nullptr)
		{
			m_physic_item->m_ready_to_destroy = true;
		}
	}

	inherited::OnH_B_Independent(just_before_destroy);
}

bool CEatableItem::UseBy(CEntityAlive* entity_alive)
{
	if (object().object_removed())
	{
		return false;
	}

	CInventoryOwner* IO = entity_alive != nullptr ? entity_alive->cast_inventory_owner() : nullptr;
	R_ASSERT(IO);
	R_ASSERT(m_pInventory == IO->m_inventory);
	R_ASSERT(object().H_Parent()->ID() == entity_alive->ID());

	CActor* actor = IO->cast_actor();

	const SEatableItemDesc& Desc = EatableDesc();
	bool use_animator = Desc.m_sUseAnimator.size() > 0;

	if (!use_animator || use_animator && !actor)
	{
		const SEatableEffectsDesc& Effects = SEatableEffectsDesc::Registry::Get(m_section_id);

		entity_alive->conditions().ApplyInfluence(Effects.Influence, m_section_id, !use_animator);

		for (const SBooster& Booster : Effects.Boosters)
		{
			entity_alive->conditions().ApplyBooster(Booster, m_section_id, !use_animator);
		}
	}

	if (!g_dedicated_server)
	{
		if (use_animator)
		{
			if (actor && actor->HudAnimator())
			{
				actor->StartAnimator(Desc.m_iMaxUses > 1 && m_iRemainingUses == 1 && Desc.m_sLastUseAnimator.size() > 0 ? Desc.m_sLastUseAnimator : Desc.m_sUseAnimator);
				actor->HudAnimator()->ItemAnimator()->SetLeftCallback({ this, &CEatableItem::EatableEffects });
			}
		}
	}

	if (!IsGameTypeSingle() && OnServer())
	{
		NET_Packet tmp_packet;
		CGameObject::u_EventGen(tmp_packet, GEG_PLAYER_USE_BOOSTER, entity_alive->ID());
		tmp_packet << object_id();
		Level().Send(tmp_packet);
	}

	if (!use_animator || use_animator && !actor)
	{
		// If uses 255, then skip the decrement for infinite usages
		if (m_iRemainingUses != u8(-1))
		{
			if (m_iRemainingUses > 0)
			{
				--m_iRemainingUses;
			}
			else
			{
				m_iRemainingUses = 0;
			}
		}

		if (IsUsingCondition())
		{
			if (GetMaxUses() > 0)
			{
				SetCondition((float)(m_iRemainingUses / GetMaxUses()));
			}
			else
			{
				SetCondition(0.0f);
			}
		}

		if (CurrentGameUI()->GetActiveInventoryWindow() && GetMaxUses() > 1)
		{
			CurrentGameUI()->GetActiveInventoryWindow()->RefreshCurrentItemCell();
		}
	}

	return true;
}

void CEatableItem::EatableEffects()
{
	CActor* actor = Level().CurrentControlEntity() ? Level().CurrentControlEntity()->cast_actor() : nullptr;

	if (actor == nullptr)
	{
		return;
	}

	const SEatableEffectsDesc& Effects = SEatableEffectsDesc::Registry::Get(m_section_id);

	actor->conditions().ApplyInfluence(Effects.Influence, m_section_id, false);

	for (const SBooster& Booster : Effects.Boosters)
	{
		actor->conditions().ApplyBooster(Booster, m_section_id, false);
	}

	if (m_iRemainingUses != (-1))
	{
		if (m_iRemainingUses > 0)
		{
			--m_iRemainingUses;
		}
		else
		{
			m_iRemainingUses = 0;
		}
	}

	if (IsUsingCondition())
	{
		if (GetMaxUses() > 0)
		{
			SetCondition((float)(m_iRemainingUses / GetMaxUses()));
		}
		else
		{
			SetCondition(0.0f);
		}
	}

	if (CurrentGameUI() && CurrentGameUI()->GetActiveInventoryWindow() && GetMaxUses() > 1)
	{
		CurrentGameUI()->GetActiveInventoryWindow()->RefreshCurrentItemCell();
	}

	if (Empty() && CanDelete())
	{
		object().DestroyObject();
	}
}

float CEatableItem::Weight() const
{
	float res = inherited::Weight();

	if (IsUsingCondition())
	{
		float net_weight = m_fWeightFull - m_fWeightEmpty;
		float use_weight = GetMaxUses() > 0 ? (net_weight / GetMaxUses()) : 0.0f;

		res = m_fWeightEmpty + (m_iRemainingUses * use_weight);
	}

	return res;
}

void CEatableItem::Hit(SHit* pHDS)
{
	//Предмет получает урон и не стакается по использованиям, поэтому функция пустая
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