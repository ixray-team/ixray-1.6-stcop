#include "stdafx.h"
#include "../pseudodog/pseudodog.h"
#include "../pseudodog_psy/pseudodog_psy.h"
#include "pseudodog_phantom.h"
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
#include "../pseudodog_psy/pseudodog_psy_aura.h"
#include "../pseudodog_psy/pseudodog_psy_state_manager.h"
#include "../../../alife_object_registry.h"
#include "../../../../xrServerEntities/xrserver_objects_alife_monsters.h"

CPseudoPsyDogPhantomBase::CPseudoPsyDogPhantomBase()
{
	m_parent = nullptr;

	m_appear_effector = {};

	m_state = {};

	m_particles_appear = {};
	m_particles_disappear = {};

	m_parent_id = {};

	m_time_spawned = {};
}

CPseudoPsyDogPhantomBase::~CPseudoPsyDogPhantomBase()
{

}

BOOL CPseudoPsyDogPhantomBase::net_Spawn(CSE_Abstract *dc)
{
	if (!inherited::net_Spawn(dc)) 
		return FALSE;

	CSE_ALifeMonsterBase *se_monster	= smart_cast<CSE_ALifeMonsterBase*>(dc);
	m_parent_id = se_monster->m_spec_object_id;
	m_parent	= 0;
	VERIFY		(m_parent_id != 0xffff);
	
	try_to_register_to_parent();

	setVisible	(FALSE);
	setEnabled	(FALSE);

	load_effector(*cNameSect(), "appear_effector",m_appear_effector);
	
	m_particles_appear		= pSettings->r_string(*cNameSect(), "particles_appear");
	m_particles_disappear	= pSettings->r_string(*cNameSect(), "particles_disappear");

	m_time_spawned			= time();

	return (TRUE);
}

void CPseudoPsyDogPhantomBase::Think()
{
	if (is_wait_to_destroy_object()) 
		return;

	inherited::Think();

	try_to_register_to_parent();

	if (m_parent && m_parent->Position().distance_to(Position()) > 30 )
	{
		destroy_me();
		return;
	}

	if (!m_parent) 
	{
		if (m_time_spawned +
			EntityDefinitions::CPseudoDogBase::CPseudoPsyDogPhantomBase::PMT_TIME_WAIT_PARENT > time())
		{
			destroy_me();
		}

		return;
	}

	if (m_state != eWaitToAppear) 
		return;

	EnemyMan.transfer_enemy(m_parent);
	
	if ( EnemyMan.get_enemy() )
		if ( !control().direction().is_face_target(EnemyMan.get_enemy(), PI_DIV_6) ) return;

	Fvector target;
	target.mad(Position(),Direction(), 10.f);
	
	control().path_builder().restrictions().add_border(Position(), target);
	u32 node = ai().level_graph().check_position_in_direction(ai_location().level_vertex_id(),Position(),target);
	control().path_builder().restrictions().remove_border();
	
	if ( ai().level_graph().valid_vertex_id(node) && control().path_builder().accessible(node) )
	{
		target.y += 1.f;
		com_man().jump	(target);
	}

	m_state			= eAttack;
	
	setVisible		(TRUE);
	setEnabled		(TRUE);

	CParticlesPlayer::StartParticles(m_particles_appear,Fvector().set(0.0f,0.1f,0.0f),ID());

	if (EnemyMan.get_enemy() != Actor()) 
		return;

	Actor()->Cameras().AddCamEffector(new CMonsterEffectorHit(m_appear_effector.ce_time,m_appear_effector.ce_amplitude,m_appear_effector.ce_period_number,m_appear_effector.ce_power));
	Actor()->Cameras().AddPPEffector(new CMonsterEffector(m_appear_effector.ppi, m_appear_effector.time, m_appear_effector.time_attack, m_appear_effector.time_release));
}

void	CPseudoPsyDogPhantomBase::Hit					(SHit* pHDS)
{
	if (is_wait_to_destroy_object()) return;
	if ((pHDS->who == EnemyMan.get_enemy())  && (pHDS->who != 0)) destroy_me();
}

void CPseudoPsyDogPhantomBase::net_Destroy()
{
	Fvector center{};
	Center(center);
	PlayParticles(m_particles_disappear,center,Fvector().set(0.f,1.f,0.f));
	
	if (m_parent && !is_wait_to_destroy_object()) {
		m_parent->unregister_phantom	(this);
		m_parent						= 0;
		m_parent_id						= 0xffff;
	}

	inherited::net_Destroy();
}

void CPseudoPsyDogPhantomBase::Die(CObject* who)
{
	inherited::Die	(who);
	destroy_me		();
}

void CPseudoPsyDogPhantomBase::try_to_register_to_parent()
{
	if(m_parent) return;
	
	CObject	*obj = Level().Objects.net_Find(m_parent_id);

	if (obj) 
	{
		CPseudoPsyDogBase *dog = smart_cast<CPseudoPsyDogBase *>(obj);
		VERIFY(dog);
		
		m_parent = dog;
		m_parent->register_phantom	(this);

		movement().restrictions().add_restrictions( m_parent->movement().restrictions().out_restrictions(), 
													m_parent->movement().restrictions().in_restrictions() );

		m_state						= eWaitToAppear;
	}
}

void CPseudoPsyDogPhantomBase::destroy_me()
{
	VERIFY(!is_wait_to_destroy_object());

	if (m_parent) 
	{
		m_parent->unregister_phantom	(this);
		m_parent						= 0;
		m_parent_id						= 0xffff;
	}

	NET_Packet		P{};
	u_EventGen		(P,GE_DESTROY,ID());
	u_EventSend		(P);
}

void CPseudoPsyDogPhantomBase::destroy_from_parent()
{
	m_parent_id		= 0xffff;

	NET_Packet		P{};
	u_EventGen		(P,GE_DESTROY,ID());
	u_EventSend		(P);
}
