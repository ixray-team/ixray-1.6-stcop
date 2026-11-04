////////////////////////////////////////////////////////////////////////////
//	Module 		: team_hierarchy_holder.cpp
//	Created 	: 12.11.2001
//  Modified 	: 03.09.2004
//	Author		: Dmitriy Iassenev, Oles Shishkovtsov, Aleksandr Maksimchuk
//	Description : Team hierarchy holder
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "team_hierarchy_holder.h"
#include "squad_hierarchy_holder.h"
#include "object_broker.h"
#include "seniority_hierarchy_space.h"

CTeamHierarchyHolder::~CTeamHierarchyHolder			()
{
	delete_data				(m_squads);
}

CSquadHierarchyHolder &CTeamHierarchyHolder::squad	(u32 squad_id) const
{
	CHECK_OR_EXIT(squad_id < max_squad_count, make_string<const char*>("Invalid hierarchy squad id: %u (expected < %u)", squad_id, u32(max_squad_count)));
	if (!m_squads[squad_id])
		m_squads[squad_id]	= new CSquadHierarchyHolder(const_cast<CTeamHierarchyHolder*>(this));
	return					(*m_squads[squad_id]);
}
