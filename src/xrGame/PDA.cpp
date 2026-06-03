#include "stdafx.h"
#include "pch_script.h"
#include "PDA.h"
#include "../xrPhysics/PhysicsShell.h"
#include "Entity.h"
#include "Actor.h"

#include "xrServer.h"
#include "xrServer_Objects_ALife_Items.h"
#include "Level.h"

#include "specific_character.h"
#include "alife_registry_wrappers.h"
#include "../xrScripts/script_engine.h"
#include "../ui/UIGameCustom.h"
#include "../ui/UIPdaWnd.h"

bool CPda::net_Spawn(CSE_Abstract* DC)
{
	inherited::net_Spawn(DC);

	CSE_ALifeItemPDA* pda = DC->cast_item_pda();
	R_ASSERT(pda);

	m_idOriginalOwner = pda->m_original_owner;
	m_SpecificChracterOwner = pda->m_specific_character;

	return true;
}

void CPda::net_Destroy()
{
	inherited::net_Destroy();
	TurnOff();
	feel_touch.clear();
	UpdateActiveContacts();
}

void CPda::Load(const char* section)
{
	inherited::Load(section);

	m_fRadius = pSettings->r_float(section, "radius");
	m_functor_str = pSettings->read_if_exists<str_c>(section,"play_function","");

	IPowerManager::SetSelfObject(cast_inventory_item(), H_Parent());
	IPowerManager::Load(section, cast_inventory_item());
}

void CPda::shedule_Update(u32 dt)
{
	inherited::shedule_Update(dt);

	if (!H_Parent() || H_Parent()->getDestroy())
	{
		return;
	}

	if (IPowerManager::IsAllow() && strcmp(m_section_id.c_str(), "device_pda") == 0)
	{
		if (CUIGameCustom* uic = CurrentGameUI())
		{
			bool is_shown_pda = uic->PdaMenu()->IsShown();
			float left_power = IPowerManager::GetLeftPowerValue();
			IPowerManager::SetEnabled(is_shown_pda);
			if (is_shown_pda && left_power <= 0)
			{
				uic->HidePdaMenu();
			}

			Actor()->set_pda_disabled(left_power <= 0);
		}
	}


	Position().set(H_Parent()->Position());

	if (IsOn() && Level().CurrentEntity() && Level().CurrentEntity()->ID() == H_Parent()->ID())
	{
		CEntityAlive* EA = H_Parent()->cast_entity_alive();
		if (!EA || !EA->g_Alive())
		{
			TurnOff();
			return;
		}

		feel_touch_update(Position(), m_fRadius);
		UpdateActiveContacts();
	}
}

void CPda::UpdateActiveContacts()
{
	m_active_contacts.resize(0);

	for (const auto& feel_object : feel_touch)
	{
		if (!feel_object || feel_object->getDestroy() || feel_object == H_Parent())
		{
			continue;
		}

		CEntityAlive* entityAlive = feel_object->cast_entity_alive();
		CInventoryOwner* inventoryOwner = feel_object->cast_inventory_owner();
		if (!entityAlive || !inventoryOwner)
		{
			continue;
		}

		if (!entityAlive->g_Alive() || entityAlive->cast_base_monster() != nullptr || entityAlive->cast_car() != nullptr)
		{
			continue;
		}

		m_active_contacts.push_back(feel_object);
	}
}

void CPda::feel_touch_new(CObject* O)
{
	if (O == nullptr || O->getDestroy())
	{
		return;
	}

	if (H_Parent() == nullptr || H_Parent()->getDestroy())
	{
		return;
	}

	CEntityAlive* entity_alive = O->cast_entity_alive();
	CInventoryOwner* pNewContactInvOwner = O->cast_inventory_owner();

	if (entity_alive->cast_base_monster() == nullptr && entity_alive->cast_car() == nullptr && pNewContactInvOwner != nullptr)
	{
		CInventoryOwner* pOwner = H_Parent()->cast_inventory_owner();
		VERIFY(pOwner);
		pOwner->NewPdaContact(pNewContactInvOwner);
	}
}

void CPda::feel_touch_delete(CObject* O)
{
	if (O == nullptr || O->getDestroy())
	{
		return;
	}

	if (H_Parent() == nullptr || H_Parent()->getDestroy())
	{
		return;
	}

	CEntityAlive* entity_alive = O->cast_entity_alive();
	CInventoryOwner* pLostContactInvOwner = O->cast_inventory_owner();

	if (entity_alive->cast_base_monster() == nullptr && entity_alive->cast_car() == nullptr && pLostContactInvOwner != nullptr)
	{
		CInventoryOwner* pOwner = H_Parent()->cast_inventory_owner();
		VERIFY(pOwner);
		pOwner->LostPdaContact(pLostContactInvOwner);
	}
}

bool CPda::feel_touch_contact(CObject* O)
{
	if (O == nullptr || O->getDestroy())
	{
		return false;
	}

	CEntityAlive* entity_alive = O->cast_entity_alive();

	if (entity_alive != nullptr && (entity_alive->cast_base_monster() || entity_alive->cast_car()))
	{
		return true;
	}
	else if (CInventoryOwner* pInvOwner = O->cast_inventory_owner())
	{
		if (this != pInvOwner->GetPDA())
		{
			if (CEntityAlive* pEntityAlive = O->cast_entity_alive())
			{
				return true;
			}
		}
	}

	return false;
}

void CPda::OnH_A_Chield()
{
	VERIFY(IsOff());

	if (H_Parent()->ID() == m_idOriginalOwner)
	{
		TurnOn();
		if (m_sFullName.empty())
		{
			m_sFullName.assign(NameItem());
			m_sFullName += " ";
			m_sFullName += H_Parent()->cast_inventory_owner()->Name();
		}
	};

	inherited::OnH_A_Chield();
}

void CPda::OnH_B_Independent(bool just_before_destroy)
{
	inherited::OnH_B_Independent(just_before_destroy);
	TurnOff();
}

CInventoryOwner* CPda::GetOriginalOwner()
{
	CObject* pObject = Level().Objects.net_Find(GetOriginalOwnerID());
	CInventoryOwner* pInvOwner = pObject != nullptr ? pObject->cast_inventory_owner() : nullptr;

	return pInvOwner;
}

void CPda::ActivePDAContacts(xr_vector<CInventoryOwner*>& res)
{
	res.resize(0);

	for (const auto& active_contact : m_active_contacts)
	{
		if (CInventoryOwner* p = GetOwner(active_contact))
		{
			res.push_back(p);
		}
	}
}

void CPda::save(NET_Packet& output_packet)
{
	inherited::save(output_packet);
	save_data(m_sFullName, output_packet);
	IPowerManager::net_save(output_packet);
}

void CPda::load(IReader& input_packet)
{
	inherited::load(input_packet);
	load_data(m_sFullName, input_packet);
	IPowerManager::net_load(input_packet);
}

void CPda::Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CPda")
	{
		inherited::Serialize(Object);
		Object << m_sFullName;
	}
}

CObject* CPda::GetOwnerObject()
{
	return Level().Objects.net_Find(GetOriginalOwnerID());
}

CInventoryOwner* CPda::GetOwner(CObject* owner)
{
	return (owner != nullptr && !owner->getDestroy() && owner->cast_inventory_owner()) ? owner->cast_inventory_owner() : nullptr;
}

void CPda::PlayScriptFunction()
{
	if (xr_strcmp(m_functor_str, ""))
	{
		luabind::functor<void> m_functor;
		R_ASSERT(ai().script_engine().functor(m_functor_str.c_str(), m_functor));
		m_functor();
	}
}
