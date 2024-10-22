#pragma once

#include "../BaseMonster/base_monster.h"
#include "../controlled_entity.h"
#include "../../../../xrScripts/script_export_space.h"

class CBoarBase : public CBaseMonster,
				 public CControlledEntity {
protected:
	using		inherited = CBaseMonster	;
	using		CControlled = CControlledEntity;

public:
	CBoarBase();
	virtual			~CBoarBase() override;

	virtual void	Load				(LPCSTR section) override;
	virtual BOOL	net_Spawn			(CSE_Abstract* DC) override;
	virtual void	reinit				() override;

	virtual void	UpdateCL			() override;

	// look at enemy
	static void	_BCL	BoneCallback	(CBoneInstance *B);
	
			float	velocity;
			float	cur_delta, target_delta;

			bool	look_at_enemy;
	
	virtual bool	ability_can_drag	() override {return true;}

	virtual	char*	get_monster_class_name () override { return (char*) "boar"; }
	
	DECLARE_SCRIPT_REGISTER_FUNCTION

};