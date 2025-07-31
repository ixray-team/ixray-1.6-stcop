#include "StdAfx.h"
#include "pch_script.h"
#include "script_game_object.h"
#include "UsableScriptObject.h"
#include "GameObject.h"
#include "../xrScripts/script_storage_space.h"
#include "../xrScripts/script_engine.h"
#include "ai/stalker/ai_stalker.h"
#include "searchlight.h"
#include "game_object_space.h"
#include "memory_manager.h"
#include "enemy_manager.h"
#include "movement_manager.h"
#include "patrol_path_manager.h"
#include "PHCommander.h"
#include "PHScriptCall.h"
#include "PHSimpleCalls.h"
#include "../xrPhysics/IPHWorld.h"
#include "doors_manager.h"
#include "Legacy/StalkerPlanner/stalker_planner.h"

void CScriptGameObject::SetTipText(LPCSTR tip_text)
{
	if (CUsableScriptObject* l_tpUseableScriptObject = object().cast_usable_script_object())
	{
		l_tpUseableScriptObject->set_tip_text(tip_text);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "SetTipText. Reason: the object is not usable");
	}
}

void CScriptGameObject::SetTipTextDefault ()
{
	if (CUsableScriptObject* l_tpUseableScriptObject = object().cast_usable_script_object())
	{
		l_tpUseableScriptObject->set_tip_text_default();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "SetTipTextDefault . Reason: the object is not usable");
	}
}

void CScriptGameObject::SetNonscriptUsable(bool nonscript_usable)
{
	if (CUsableScriptObject* l_tpUseableScriptObject = object().cast_usable_script_object())
	{
		l_tpUseableScriptObject->set_nonscript_usable(nonscript_usable);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "SetNonscriptUsable . Reason: the object is not usable");
	}
}

Fvector CScriptGameObject::GetCurrentDirection()
{
	if (CProjector* obj = object().cast_projector())
	{
		return obj->GetCurrentDirection();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "Script Object : cannot access class member GetCurrentDirection!");
	return zero_vel;
}

CScriptGameObject::CScriptGameObject(CGameObject *game_object) : m_game_object(game_object), m_door(0)
{
	R_ASSERT2(m_game_object, "Null actual object passed!");
}

CScriptGameObject::~CScriptGameObject()
{
	if (m_door == nullptr)
	{
		return;
	}

	unregister_door();
}

CScriptGameObject *CScriptGameObject::Parent() const
{
	if (CGameObject* l_tpGameObject = object().H_Parent()->cast_game_object())
	{
		return l_tpGameObject->lua_game_object();
	}

	return 0;
}

int	CScriptGameObject::clsid() const
{
	return object().clsid();
}

LPCSTR CScriptGameObject::Name() const
{
	return *object().cName();
}

shared_str CScriptGameObject::cName() const
{
	return object().cName();
}

LPCSTR CScriptGameObject::Section() const
{
	if (m_game_object == nullptr)
	{
		return 0;
	}

	return *object().cNameSect();
}

void CScriptGameObject::Kill(CScriptGameObject* who, bool bypass_actor_check /*AVO: added for actor before death callback*/)
{
	if (CEntity* l_tpEntity = object().cast_entity())
	{
		if (!l_tpEntity->AlreadyDie())
		{
			l_tpEntity->KillEntity(who ? who->object().ID() : object().ID(), bypass_actor_check);
		}
		else
		{
			ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "attempt to kill dead object %s", *object().cName());
		}
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "%s cannot access class member Kill!", *object().cName());
	}
}

void CScriptGameObject::KillNotBypassActorCheck(CScriptGameObject* who)
{
	Kill(who, false);
}

bool CScriptGameObject::Alive() const
{
	if (CEntity* entity = object().cast_entity())
	{
		return entity->g_Alive();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CSciptEntity : cannot access class member Alive!");
	return false;
}

ALife::ERelationType CScriptGameObject::GetRelationType(CScriptGameObject* who)
{
	CEntityAlive* l_tpEntityAlive1 = object().cast_entity_alive();
	if (l_tpEntityAlive1 == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"%s cannot access class member GetRelationType!",*object().cName());
		return ALife::eRelationTypeDummy;
	}
	
	CEntityAlive* l_tpEntityAlive2 = who->object().cast_entity_alive();
	if (l_tpEntityAlive2 == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"%s cannot apply GetRelationType method for non-alive object!",*who->object().cName());
		return ALife::eRelationTypeDummy;
	}
	
	return l_tpEntityAlive1->tfGetRelationType(l_tpEntityAlive2);
}

template <typename T>
IC T* CScriptGameObject::action_planner()
{
	if (CAI_Stalker* manager = object().cast_stalker())
	{
		return &manager->brain();
	}

	ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CAI_Stalker : cannot access class member action_planner!");
	return 0;
}

CScriptActionPlanner* script_action_planner(CScriptGameObject *obj)
{
	return obj->action_planner<CScriptActionPlanner>();
}

void CScriptGameObject::set_enemy_callback(const luabind::functor<bool> &functor)
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->memory().enemy().useful_callback().set(functor);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member set_enemy_callback!");
	}
}

void CScriptGameObject::set_enemy_callback(const luabind::functor<bool> &functor, const luabind::object &object)
{
	if (CCustomMonster* monster = this->object().cast_custom_monster())
	{
		monster->memory().enemy().useful_callback().set(functor, object);
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member set_enemy_callback!");
	}
}

void CScriptGameObject::set_enemy_callback()
{
	if (CCustomMonster* monster = object().cast_custom_monster())
	{
		monster->memory().enemy().useful_callback().clear();
	}
	else
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "CCustomMonster : cannot access class member set_enemy_callback!");
	}
}

void CScriptGameObject::SetCallback(GameObject::ECallbackType type, const luabind::functor<void> &functor)
{
	object().callback(type).set(functor);
}

void CScriptGameObject::SetCallback(GameObject::ECallbackType type, const luabind::functor<void> &functor, const luabind::object &object)
{
	this->object().callback(type).set(functor, object);
}

void CScriptGameObject::SetCallback(GameObject::ECallbackType type)
{
	object().callback(type).clear();
}

void CScriptGameObject::set_fastcall(const luabind::functor<bool> &functor, const luabind::object &object)
{
	CPHScriptGameObjectCondition* c = new CPHScriptGameObjectCondition(object,functor,m_game_object);
	CPHDummiAction* a = new CPHDummiAction();
	CPHSriptReqGObjComparer cmpr(m_game_object);
	Level().ph_commander_scripts().remove_calls(&cmpr);
	Level().ph_commander_scripts().add_call(c, a);
}

void CScriptGameObject::set_const_force(const Fvector &dir,float value,u32 time_interval)
{
	CPhysicsShell* shell = object().cast_physics_shell_holder()->PPhysicsShell();

	if (physics_world() == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"set_const_force : ph_world do not exist!");
		return;
	}

	if (shell == nullptr)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError,"set_const_force : object %s has no physics shell!",*object().cName());
		return;
	}

	Fvector force;
	force.set(dir);
	force.mul(value);

	CPHConstForceAction* a = new CPHConstForceAction(shell,force);
	CPHExpireOnStepCondition* cn = new CPHExpireOnStepCondition();
	cn->set_time_interval(time_interval);
	Level().ph_commander_physics_worldstep().add_call(cn, a);
	
}
