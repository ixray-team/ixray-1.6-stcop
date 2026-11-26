#pragma once
#include "../basemonster/base_monster.h"
#include "../../../../xrScripts/script_export_space.h"

class CStateManagerFracture;

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CFracture final : public CBaseMonster {
	typedef		CBaseMonster		inherited;
	
public:
	const char*		RamPushType		() const override { return "fracture"; }
					CFracture 			();
	virtual ~CFracture 			() override;

	virtual void	Load				(const char* section) override;
	virtual void	CheckSpecParams		(u32 spec_params) override;

	virtual char*	get_monster_class_name () override { return (char*)"fracture"; }



};