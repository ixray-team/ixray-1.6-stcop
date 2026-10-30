////////////////////////////////////////////////////////////////////////////
//	Module 		: script_action_condition.h
//	Created 	: 30.09.2003
//  Modified 	: 29.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Script action condition class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../xrScripts/script_export_space.h"
#include "alife_space.h"

XREFLECT(LuaExport, Name="cond")
class CScriptActionCondition {
public:
	XPROPERTY(LuaEnum, Name="cond", Names="move_end=MOVEMENT_FLAG,look_end=WATCH_FLAG,anim_end=ANIMATION_FLAG,sound_end=SOUND_FLAG,object_end=OBJECT_FLAG,time_end=TIME_FLAG,act_end=ACT_FLAG")
	enum EActionFlags {
		MOVEMENT_FLAG	= u32(1 << 0),
		WATCH_FLAG		= u32(1 << 1),
		ANIMATION_FLAG	= u32(1 << 2),
		SOUND_FLAG		= u32(1 << 3),
		PARTICLE_FLAG	= u32(1 << 4),
		OBJECT_FLAG		= u32(1 << 5),
		TIME_FLAG		= u32(1 << 6),
		ACT_FLAG		= u32(1 << 7)
	};

public:
	u32					m_dwFlags;
	ALife::_TIME_ID		m_tLifeTime;
	ALife::_TIME_ID		m_tStartTime;

public:
	XPROPERTY(LuaConstructor)
	IC					CScriptActionCondition	();
	XPROPERTY(LuaConstructor)
	IC					CScriptActionCondition	(u32 dwFlags, double dTime = -1);
	virtual				~CScriptActionCondition	();
	IC		void		initialize				();

};

#include "script_action_condition_inline.h"