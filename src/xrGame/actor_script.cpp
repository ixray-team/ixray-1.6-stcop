////////////////////////////////////////////////////////////////////////////
//	Module 		: actor_script.cpp
//	Created 	: 17.01.2008
//  Modified 	: 17.01.2008
//	Author		: Dmitriy Iassenev
//	Description : actor script export
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "Actor.h"
#include "level_changer.h"
#include "ActorCondition.h"

XREFLECT(LuaExport, Name="is_first_person")
bool isFirstPerson() { return Actor()->active_cam() == eacFirstEye; }
XREFLECT(LuaExport, Name="set_first_person")
void setFirstPerson() { Actor()->cam_Set(eacFirstEye); }
XREFLECT(LuaExport, Name="set_third_person")
void setThirdPerson() { Actor()->cam_Set(eacLookAt); }

XREFLECT(LuaExport, Name="is_god_mode")
bool isGodMode() { return psActorFlags.test(AF_GOD_MODE); }
XREFLECT(LuaExport, Name="is_actor_shadow")
bool isActorShadow() { return psGameFlags.test(rsActorShadow); }
XREFLECT(LuaExport, Name="set_actor_shadow")
void setActorShadow(bool state) { psGameFlags.set(rsActorShadow, state); }

#include "actor_script.reflect.inl"
