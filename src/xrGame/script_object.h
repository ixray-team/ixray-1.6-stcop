////////////////////////////////////////////////////////////////////////////
//	Module 		: script_object.h
//	Created 	: 06.10.2003
//  Modified 	: 14.12.2004
//	Author		: Dmitriy Iassenev
//	Description : Script object class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "GameObject.h"
#include "script_entity.h"

class CScriptObject : 
	public CGameObject,
	public CScriptEntity
{
public:
								CScriptObject			();
	virtual ~CScriptObject			() override;
	virtual DLL_Pure			*_construct				() override;
	virtual void				reinit					() override;
	virtual bool				net_Spawn				(CSE_Abstract* DC) override;
	virtual void				net_Destroy				() override;
	virtual bool				UsedAI_Locations		() override;
	virtual void				shedule_Update			(u32 DT) override;
	virtual void				UpdateCL				() override;
	virtual CScriptEntity*		cast_script_entity		() override {return this;}
};
