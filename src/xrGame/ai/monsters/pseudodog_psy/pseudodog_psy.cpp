#include "stdafx.h"
#include "../pseudodog/pseudodog.h"
#include "pseudodog_psy.h"
#include "../pseudodog_phantom/pseudodog_phantom.h"
#include "../../../level_graph.h"
#include "../../../ai_space.h"
#include "../../../alife_simulator.h"
#include "../../../../xrServerEntities/xrServer_Object_Base.h"
#include "../../../xrserver.h"
#include "../../../ai_object_location.h"
#include "../../../level.h"
#include "../control_movement_base.h"
#include "../monster_velocity_space.h"
#include "../../../restricted_object.h"
#include "../../../actor.h"
#include "../ai_monster_effector.h"
#include "../../../ActorEffector.h"
#include "pseudodog_psy_aura.h"
#include "pseudodog_psy_aura_effector.h"
#include "pseudodog_psy_state_manager.h"
#include "../../../alife_object_registry.h"
#include "../../../../xrServerEntities/xrserver_objects_alife_monsters.h"

CPseudoPsyDogBase::CPseudoPsyDogBase()
{
	m_enemy = nullptr;

	m_aura						=	new CPsyDogAura(this);
	m_max_phantoms_count		=	0;
	m_time_phantom_respawn = {};
	m_phantoms_die_time			=	nullptr;
}

CPseudoPsyDogBase::~CPseudoPsyDogBase()
{
	xr_delete						(m_aura);
	xr_free							(m_phantoms_die_time);
}

void CPseudoPsyDogBase::Load(LPCSTR section)
{
	inherited::Load					(section);
	
	m_aura->load					(pSettings->r_string(section,"aura_effector"));
	m_max_phantoms_count		=	pSettings->r_u8(section,"Phantoms_Count");
	
	xr_free							(m_phantoms_die_time);
	m_phantoms_die_time			=	xr_alloc<TTime>(m_max_phantoms_count);

	for ( int i=0; i<m_max_phantoms_count; ++i )
		m_phantoms_die_time[i]	= EntityDefinitions::CPseudoDogBase::CPseudoPsyDogBase::s_phantom_immediate_respawn_flag;

	m_time_phantom_respawn		=	pSettings->r_u32(section,"Time_Phantom_Respawn");
}

BOOL CPseudoPsyDogBase::net_Spawn(CSE_Abstract *dc)
{
	if (!inherited::net_Spawn(dc)) return FALSE;

	return TRUE;
}

void CPseudoPsyDogBase::reinit()
{
	inherited::reinit	();
	m_aura->reinit		();
}

void CPseudoPsyDogBase::reload(LPCSTR section)
{
	inherited::reload(section);
}

void CPseudoPsyDogBase::register_phantom(CPseudoPsyDogPhantomBase*phantom)
{
 	m_storage.push_back(phantom);
}

void CPseudoPsyDogBase::unregister_phantom(CPseudoPsyDogPhantomBase*phantom)
{
	xr_vector<CPseudoPsyDogPhantomBase*>::iterator it = std::find(m_storage.begin(),m_storage.end(), phantom);

	for ( int i=0; i<m_max_phantoms_count; ++i )
	{
		if ( m_phantoms_die_time[i] == EntityDefinitions::CPseudoDogBase::CPseudoPsyDogBase::s_phantom_alive_flag )
		{
			m_phantoms_die_time[i]	=	time();
			break;
		}
	}

	VERIFY(it != m_storage.end());
	m_storage.erase(it);
}

bool CPseudoPsyDogBase::spawn_phantom()
{
	u32 node{};
	if (!control().path_builder().get_node_in_radius(ai_location().level_vertex_id(), 4,8,5,node)) return false;
	
	LPCSTR phantomSection = READ_IF_EXISTS(pSettings, r_string, get_section(), "phantom_section", "psy_dog_phantom");
	CSE_Abstract* phantom = Level().spawn_item(phantomSection, ai().level_graph().vertex_position(node), node, 0xffff, true);
	CSE_ALifeMonsterBase	*pSE_Monster = smart_cast<CSE_ALifeMonsterBase*>(phantom);
	VERIFY(pSE_Monster);

	pSE_Monster->m_spec_object_id = ID();
	
	NET_Packet					P{};
	phantom->Spawn_Write		(P,TRUE);
	Level().Send				(P,net_flags(TRUE));
	F_entity_Destroy			(phantom);

	return true;
}

void CPseudoPsyDogBase::delete_all_phantoms()
{
	for (xr_vector<CPseudoPsyDogPhantomBase*>::iterator it = m_storage.begin(); it != m_storage.end(); it++)
		(*it)->destroy_from_parent();

	m_storage.clear();
}

void CPseudoPsyDogBase::Think()
{
	inherited::Think();
	if (!g_Alive()) return;
	
	m_aura->update_schedule();

	if ( EnemyMan.get_enemy() && get_phantoms_count() < m_max_phantoms_count )
	{
		for ( int i=0; i<m_max_phantoms_count; ++i )
		{
			if ( m_phantoms_die_time[i] != EntityDefinitions::CPseudoDogBase::CPseudoPsyDogBase::s_phantom_alive_flag &&
				 time() > m_phantoms_die_time[i] + m_time_phantom_respawn )
			{
				if ( spawn_phantom() )
					m_phantoms_die_time[i]	= EntityDefinitions::CPseudoDogBase::CPseudoPsyDogBase::s_phantom_alive_flag;
			}
		}
	}
	else 
	{
		if (!EnemyMan.get_enemy() && !m_storage.empty()) {
			delete_all_phantoms();
		}
	}
}

void CPseudoPsyDogBase::net_Destroy()
{
	m_aura->on_death();

	delete_all_phantoms	();
	inherited::net_Destroy();
}

void CPseudoPsyDogBase::Die(CObject* who)
{
	inherited::Die		(who);
	m_aura->on_death	();
	delete_all_phantoms	();
}

IStateManagerBase *CPseudoPsyDogBase::create_state_manager()
{
	return new CPseudoPsyDogBaseStateManager(this);
}

u8 CPseudoPsyDogBase::get_phantoms_count()
{
	return u8(m_storage.size());
}
