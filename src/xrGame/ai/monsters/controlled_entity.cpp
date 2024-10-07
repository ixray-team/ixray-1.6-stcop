#include "StdAfx.h"
#include "controlled_entity.h"

void CControlledEntity::on_reinit()
{
	m_data.m_object = 0;
	m_controller	= 0; 
}


void CControlledEntity::set_task_follow(const CEntity *e)
{
	m_data.m_object = e;
	m_data.m_task	= eTaskFollow;
}

void CControlledEntity::set_task_attack(const CEntity *e)
{
	m_data.m_object = e;
	m_data.m_task	= eTaskAttack;
}


void CControlledEntity::set_under_control(CControllerBase *controller)
{
	m_controller		= controller;
	
	saved_id.team_id	= m_object->g_Team	();
	saved_id.squad_id	= m_object->g_Squad	();
	saved_id.group_id	= m_object->g_Group	();

	m_object->ChangeTeam(m_controller->g_Team(), m_controller->g_Squad(), m_controller->g_Group());
}


void CControlledEntity::free_from_control()
{
	m_object->ChangeTeam			(saved_id.team_id, saved_id.squad_id, saved_id.group_id);
	m_controller					= 0;
}	


void CControlledEntity::on_die()
{
	if (!is_under_control())			return;

	m_controller->OnFreedFromControl	(m_object);
	m_controller						= 0;
}

void CControlledEntity::on_destroy()
{
	if (!is_under_control())			return;

	m_object->ChangeTeam				(saved_id.team_id, saved_id.squad_id, saved_id.group_id);

	m_controller->OnFreedFromControl	(m_object);
	m_controller						= 0;
}
