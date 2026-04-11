////////////////////////////////////////////////////////////////////////////
//	Module 		: agent_manager.h
//	Created 	: 24.05.2004
//  Modified 	: 24.05.2004
//	Author		: Dmitriy Iassenev
//	Description : Agent manager
////////////////////////////////////////////////////////////////////////////

#pragma once

class CAgentCorpseManager;
class CAgentEnemyManager;
class CAgentExplosiveManager;
class CAgentLocationManager;
class CAgentMemberManager;
class CAgentMemoryManager;
class FRbmkAgentManagerPlanner;

class CAgentManager final
{
	CAgentCorpseManager* m_corpse;
	CAgentEnemyManager* m_enemy;
	CAgentExplosiveManager* m_explosive;
	CAgentLocationManager* m_location;
	CAgentMemberManager* m_member;
	CAgentMemoryManager* m_memory;
	FRbmkAgentManagerPlanner* m_brain;

private:
	u32 m_last_update_time;
	u32 m_update_rate;

private:
	void init_scheduler();
	void init_components();
	void remove_components();
	void update_impl();

public:
	CAgentManager();
	~CAgentManager();

	void update();
	shared_str cName() const { return "agent_manager"; };
	void remove_links(CObject* object);

public:
	IC CAgentCorpseManager& corpse() const { return *m_corpse; };
	IC CAgentEnemyManager& enemy() const { return *m_enemy; };
	IC CAgentExplosiveManager& explosive() const { return *m_explosive; };
	IC CAgentLocationManager& location() const { return *m_location; };
	IC CAgentMemberManager& member() const{ return *m_member; };
	IC CAgentMemoryManager& memory() const{ return *m_memory; };
};