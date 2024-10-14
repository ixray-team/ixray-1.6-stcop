#pragma once
#include "../../ai_entity_definitions.h"
#include "../BaseMonster/base_monster.h"
#include "../../../../xrScripts/script_export_space.h"

class CSnorkBase : public CBaseMonster 
{
protected:
	using inherited = CBaseMonster;

	SVelocityParam	m_fsVelocityJumpPrepare;
	SVelocityParam	m_fsVelocityJumpGround;

public:
					CSnorkBase				();
	virtual			~CSnorkBase				() override;

	virtual void	Load				(LPCSTR section) override;
	virtual void	reinit				() override;
	virtual void	UpdateCL			() override;
	virtual void	CheckSpecParams		(u32 spec_params) override;
	virtual void	jump				(const Fvector &position, float factor) override;
	virtual bool	ability_jump_over_physics	() override { return true; }
	virtual bool	ability_distant_feel		() override { return true; }
	virtual void	HitEntityInJump		(const CEntity *pEntity) override;
			
			bool	find_geometry		(Fvector &dir);
			float	trace				(const Fvector &dir);

			bool	trace_geometry		(const Fvector &d, float &range);

	virtual bool	check_start_conditions	(ControlCom::EControlType type) override;
	virtual void	on_activate_control		(ControlCom::EControlType) override;
	virtual	char*	get_monster_class_name () override { return (char*) "snork"; }

	virtual bool	run_home_point_when_enemy_inaccessible () const override { return false; }

	bool	start_threaten;
	ref_sound		m_sound_start_threaten;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};