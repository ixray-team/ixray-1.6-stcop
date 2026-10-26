#pragma once
#include "../basemonster/base_monster.h"
#include "../../../../xrScripts/script_export_space.h"

class CSnork final :	public CBaseMonster {
	typedef		CBaseMonster		inherited;

	SVelocityParam	m_fsVelocityJumpPrepare;
	SVelocityParam	m_fsVelocityJumpGround;

public:
					CSnork				();
	virtual ~CSnork				() override;

	virtual void	Load				(const char* section) override;
	virtual void	reinit				() override;
	virtual void	UpdateCL			() override;
	virtual void	CheckSpecParams		(u32 spec_params) override;
	virtual void	jump				(const Fvector &position, float factor) override;
	virtual bool	ability_jump_over_physics	() override {return true;}
	virtual bool	ability_distant_feel		() override {return true;}
	virtual void	HitEntityInJump		(const CEntity *pEntity) override;
			
			bool	find_geometry		(Fvector &dir);
			float	trace				(const Fvector &dir);

			bool	trace_geometry		(const Fvector &d, float &range);

	virtual bool	check_start_conditions	(ControlCom::EControlType type) override;
	virtual void	on_activate_control		(ControlCom::EControlType) override;
	virtual char*	get_monster_class_name () override { return (char*) "snork"; }

	virtual bool	run_home_point_when_enemy_inaccessible () const override { return false; }

private:
#ifdef _DEBUG	
	virtual void	debug_on_key		(int key);
#endif

public:

		u32		m_target_node;
		bool	start_threaten;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};