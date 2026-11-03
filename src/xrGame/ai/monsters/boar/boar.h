#pragma once

#include "../basemonster/base_monster.h"
#include "../controlled_entity.h"
#include "../../../../xrScripts/script_export_space.h"

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CAI_Boar final : public CBaseMonster,
				 public CControlledEntity<CAI_Boar> {

	typedef		CBaseMonster	inherited;
	typedef		CControlledEntity<CAI_Boar>	CControlled;

public:
					CAI_Boar			();
	virtual ~CAI_Boar			() override;

	virtual void	Load				(const char* section) override;
	virtual bool	net_Spawn			(CSE_Abstract* DC) override;
	virtual void	reinit				() override;

	virtual void	UpdateCL			() override;

	virtual bool	CanExecRotationJump	() {return true;}
	virtual void	CheckSpecParams		(u32 spec_params) override;

	// look at enemy
	static void	_BCL	BoneCallback	(CBoneInstance *B);
	
			float	_velocity;
			float	_cur_delta, _target_delta;
			bool	look_at_enemy;

	virtual bool	ability_can_drag	() override {return true;}

	virtual char*	get_monster_class_name () override { return (char*) "boar"; }
	


};