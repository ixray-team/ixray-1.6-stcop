////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_communication_manager.h
//	Created 	: 03.09.2003
//  Modified 	: 14.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife communication manager
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../xrServerEntities/xrServer_Space.h"
#include "alife_combat_manager.h"
#include "alife_communication_manager.h"

class CALifeInteractionManager : 
	public CALifeCombatManager,
	public CALifeCommunicationManager
{
public:
	CALifeInteractionManager(xrServer *server, LPCSTR section);
};