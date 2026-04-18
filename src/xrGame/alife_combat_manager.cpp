////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_combat_manager.h
//	Created 	: 12.08.2003
//  Modified 	: 14.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife combat manager
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "alife_combat_manager.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "alife_graph_registry.h"
#include "alife_schedule_registry.h"

CALifeCombatManager::CALifeCombatManager	(xrServer *server, const char* section) :
	CALifeSimulatorBase	(server,section)
{
}

void CALifeCombatManager::kill_entity	(CSE_ALifeMonsterAbstract *l_tpALifeMonsterAbstract, const GameGraph::_GRAPH_ID &l_tGraphID, CSE_ALifeSchedulable *schedulable)
{
	VERIFY									(l_tpALifeMonsterAbstract->g_Alive());
	append_item_vector						(l_tpALifeMonsterAbstract->children,m_temp_item_vector);
	GameGraph::_GRAPH_ID					l_tGraphID1 = l_tpALifeMonsterAbstract->m_tGraphID;
	assign_death_position					(l_tpALifeMonsterAbstract, l_tGraphID, schedulable);
	l_tpALifeMonsterAbstract->vfDetachAll	();
	R_ASSERT								(l_tpALifeMonsterAbstract->children.empty());
	scheduled().remove						(l_tpALifeMonsterAbstract);
	if (l_tpALifeMonsterAbstract->m_tGraphID != l_tGraphID1) {
		graph().remove						(l_tpALifeMonsterAbstract,l_tGraphID1);
		graph().add							(l_tpALifeMonsterAbstract,l_tpALifeMonsterAbstract->m_tGraphID);
	}
	CSE_ALifeInventoryItem *l_tpALifeInventoryItem = smart_cast<CSE_ALifeInventoryItem*>(l_tpALifeMonsterAbstract);
	if (l_tpALifeInventoryItem)
		m_temp_item_vector.push_back		(l_tpALifeInventoryItem);
}
