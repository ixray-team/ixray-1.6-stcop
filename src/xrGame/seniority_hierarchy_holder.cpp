////////////////////////////////////////////////////////////////////////////
//	Module 		: seniority_hierarchy_holder.cpp
//	Created 	: 12.11.2001
//  Modified 	: 03.09.2004
//	Author		: Dmitriy Iassenev, Oles Shishkovtsov, Aleksandr Maksimchuk
//	Description : Seniority hierarchy holder
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "seniority_hierarchy_holder.h"
#include "team_hierarchy_holder.h"
#include "object_broker.h"

CSeniorityHierarchyHolder::~CSeniorityHierarchyHolder	()
{
	delete_data				(m_teams);
}

CTeamHierarchyHolder &CSeniorityHierarchyHolder::team	(u32 team_id)
{
	CHECK_OR_EXIT(team_id < max_team_count, make_string<const char*>("Invalid hierarchy team id: %u (expected < %u)", team_id, u32(max_team_count)));
	if (!m_teams[team_id])
		m_teams[team_id]	= new CTeamHierarchyHolder(this);
	return					(*m_teams[team_id]);
}
