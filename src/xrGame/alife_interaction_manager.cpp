////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_communication_manager.cpp
//	Created 	: 03.09.2003
//  Modified 	: 14.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife communication manager
////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"
#include "alife_interaction_manager.h"

CALifeInteractionManager::CALifeInteractionManager	(xrServer*server, const char* section) : 
	CALifeCombatManager			(server,section),
	CALifeCommunicationManager	(server,section),
	CALifeSimulatorBase			(server,section)
{
}