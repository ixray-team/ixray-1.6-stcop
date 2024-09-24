#pragma once

#include "../BaseMonster/base_monster.h"
#include "../controlled_entity.h"
#include "../../../../xrScripts/script_export_space.h"

class CustomBoar : public CBaseMonster,
				 public CControlledEntity {

	using		inherited = CBaseMonster	;
	using		CControlled = CControlledEntity;

public:
	CustomBoar();
	virtual			~CustomBoar();

	virtual void	Load				(LPCSTR section);
	virtual BOOL	net_Spawn			(CSE_Abstract* DC);
	virtual void	reinit				();

	virtual void	UpdateCL			();

	virtual bool	CanExecRotationJump	() {return true;}

	// look at enemy
	static void	_BCL	BoneCallback	(CBoneInstance *B);
	
			float	velocity;
			float	cur_delta, target_delta;

			bool	look_at_enemy;
	
	virtual bool	ability_can_drag	() {return true;}

	virtual	char*	get_monster_class_name () { return (char*) "boar"; }
	
	DECLARE_SCRIPT_REGISTER_FUNCTION

};