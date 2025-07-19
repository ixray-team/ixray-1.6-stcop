#include "StdAfx.h"
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


CPda::CPda(void)						
{										
	m_idOriginalOwner		= u16(-1);
	m_SpecificChracterOwner = nullptr;
	TurnOff					();
}

CPda::~CPda() 
{}

BOOL CPda::net_Spawn(CSE_Abstract* DC) 
{
	inherited::net_Spawn		(DC);
	CSE_Abstract				*abstract = (CSE_Abstract*)(DC);
	CSE_ALifeItemPDA			*pda = smart_cast<CSE_ALifeItemPDA*>(abstract);
	R_ASSERT					(pda);
	m_idOriginalOwner			= pda->m_original_owner;
	m_SpecificChracterOwner		= pda->m_specific_character;

	return						(TRUE);
}

void CPda::net_Destroy() 
{
	inherited::net_Destroy		();
	TurnOff						();
	feel_touch.clear			();
	UpdateActiveContacts		();
}

void CPda::Load(LPCSTR section) 
{
	inherited::Load(section);

	m_fRadius = pSettings->r_float(section,"radius");
	m_functor_str = READ_IF_EXISTS(pSettings,r_string,section,"play_function",""); 
}

void CPda::shedule_Update(u32 dt)	
{
	inherited::shedule_Update	(dt);

	if(!H_Parent() || H_Parent()->getDestroy()) return;

	if(!CAttachableItem::enabled())
		Position().set(H_Parent()->Position());

	if( IsOn() && Level().CurrentEntity() && Level().CurrentEntity()->ID()==H_Parent()->ID() )
	{
		CEntityAlive* EA = H_Parent()->cast_entity_alive();
		if(!EA || !EA->g_Alive())
		{
			TurnOff();
			return;
		}

		feel_touch_update(Position(),m_fRadius);
		UpdateActiveContacts	();
	}
}

void CPda::UpdateActiveContacts	()
{
	m_active_contacts.resize(0);
	xr_vector<CObject*>::iterator it= feel_touch.begin();
	for(;it!=feel_touch.end();++it){
		CEntityAlive* pEA = (*it)->cast_entity_alive();
		if(!!pEA->g_Alive() && !pEA->cast_base_monster() && !pEA->cast_car())
		{
			m_active_contacts.push_back(*it);
		}
	}
}

void CPda::feel_touch_new(CObject* O) 
{
	if (!O || O->getDestroy()) return;
	if (!H_Parent() || H_Parent()->getDestroy()) return;

	CEntityAlive* entity_alive = O->cast_entity_alive();
	CInventoryOwner* pNewContactInvOwner = O->cast_inventory_owner();

	if (!entity_alive->cast_base_monster() && !entity_alive->cast_car() && pNewContactInvOwner)
	{
		CInventoryOwner* pOwner = H_Parent()->cast_inventory_owner(); VERIFY(pOwner);
		pOwner->NewPdaContact(pNewContactInvOwner);
	}
}

void CPda::feel_touch_delete(CObject* O) 
{
	if (!O || O->getDestroy()) return;
	if (!H_Parent() || H_Parent()->getDestroy()) return;

	CEntityAlive* entity_alive = O->cast_entity_alive();
	CInventoryOwner* pLostContactInvOwner = O->cast_inventory_owner();
	
	if (!entity_alive->cast_base_monster() && !entity_alive->cast_car() && pLostContactInvOwner)
	{
		CInventoryOwner* pOwner = H_Parent()->cast_inventory_owner(); VERIFY(pOwner);
		pOwner->LostPdaContact(pLostContactInvOwner);
	}
}

BOOL CPda::feel_touch_contact(CObject* O)
{
	if (!O || O->getDestroy()) return FALSE;

	CEntityAlive* entity_alive = O->cast_entity_alive();

	if (entity_alive && (entity_alive->cast_base_monster() || entity_alive->cast_car()))
	{
		return TRUE;
	}
	else if (CInventoryOwner* pInvOwner = O->cast_inventory_owner())
	{
		if (this != pInvOwner->GetPDA())
		{
			CEntityAlive* pEntityAlive = O->cast_entity_alive();
			if (pEntityAlive)
				return TRUE;
		}
		else
			return FALSE;
	}

	return FALSE;
}

void CPda::OnH_A_Chield() 
{
	VERIFY(IsOff());

	//âêëþ÷èòü PDA òîëüêî åñëè îíî íàõîäèòñÿ ó ïåðâîãî âëàäåëüöà
	if(H_Parent()->ID() == m_idOriginalOwner){
		TurnOn					();
		if(m_sFullName.empty()){
			m_sFullName.assign( NameItem() );
			m_sFullName += " ";
			m_sFullName += H_Parent()->cast_inventory_owner()->Name();
		}
	};
	inherited::OnH_A_Chield		();
}

void CPda::OnH_B_Independent(bool just_before_destroy) 
{
	inherited::OnH_B_Independent(just_before_destroy);
	
	//âûêëþ÷èòü
	TurnOff();
}


CInventoryOwner* CPda::GetOriginalOwner()
{
	CObject* pObject =  Level().Objects.net_Find(GetOriginalOwnerID());
	CInventoryOwner* pInvOwner = pObject ? pObject->cast_inventory_owner() : NULL;

	return pInvOwner;
}



void CPda::ActivePDAContacts(xr_vector<CInventoryOwner*>& res)
{
	res.resize(0);
	xr_vector<CObject*>::iterator it		= m_active_contacts.begin();
	xr_vector<CObject*>::iterator it_e		= m_active_contacts.end();

	for(;it!=it_e;++it)
	{
		CInventoryOwner* p = GetOwner(*it);
		if(p)
			res.push_back(p);
	}
}

void CPda::save(NET_Packet &output_packet)
{
	inherited::save	(output_packet);
	save_data		(m_sFullName, output_packet);
}

void CPda::load(IReader &input_packet)
{
	inherited::load	(input_packet);
	load_data		(m_sFullName, input_packet);
}

CObject* CPda::GetOwnerObject()
{
	return				Level().Objects.net_Find(GetOriginalOwnerID());
}
/* remove must
LPCSTR		CPda::Name				()
{
	if( !m_SpecificChracterOwner.size() )
		return inherited::Name();

	if(m_sFullName.empty())
	{
		m_sFullName.assign(inherited::Name());
		
		CSpecificCharacter spec_char;
		spec_char.Load(m_SpecificChracterOwner);
		m_sFullName += " ";
		m_sFullName += xr_string(spec_char.Name());
	}
	
	return m_sFullName.c_str();
}
*/

CInventoryOwner* CPda::GetOwner(CObject* owner)
{
	return (owner&&!owner->getDestroy()&&owner->cast_inventory_owner()) ? owner->cast_inventory_owner() : NULL;
}

void CPda::PlayScriptFunction()
{
	if(xr_strcmp(m_functor_str, ""))
	{
		luabind::functor<void> m_functor;
		R_ASSERT(ai().script_engine().functor(m_functor_str.c_str(), m_functor));
		m_functor();
	}
}
