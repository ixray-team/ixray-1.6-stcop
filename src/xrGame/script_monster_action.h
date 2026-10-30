////////////////////////////////////////////////////////////////////////////
//	Module 		: script_monster_action.h
//	Created 	: 30.09.2003
//  Modified 	: 29.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Script monster action class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "script_abstract_action.h"
#include "../xrScripts/script_export_space.h"
#include "ai_monster_space.h"

class CScriptGameObject;

XREFLECT(LuaExport, Name="act")
class CScriptMonsterAction :
	public CScriptAbstractAction 
{
public: 
	MonsterSpace::EScriptMonsterGlobalAction	m_tAction;
	CObject										*m_tObject;

public:
	XPROPERTY(LuaConstructor)
	IC				CScriptMonsterAction	();
	XPROPERTY(LuaConstructor)
	IC				CScriptMonsterAction	(MonsterSpace::EScriptMonsterGlobalAction action);
	XPROPERTY(LuaConstructor)
	IC				CScriptMonsterAction	(MonsterSpace::EScriptMonsterGlobalAction action, CScriptGameObject *tObj);
	virtual ~CScriptMonsterAction	() override;
			void	SetObject				(CScriptGameObject *tObj);

};

#include "script_monster_action_inline.h"