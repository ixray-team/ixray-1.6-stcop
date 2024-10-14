#pragma once
#include "../../ai_entity_definitions.h"
#include "../BaseMonster/base_monster.h"
#include "../controlled_entity.h"
#include "../ai_monster_bones.h"
#include "../anim_triple.h"
#include "../../../../xrScripts/script_export_space.h"

class CZombieBase :	public CBaseMonster, public CControlledEntity 
{
protected:
	using	inherited =	CBaseMonster;
	using	CControlled = CControlledEntity;

	bonesManipulation	Bones;

public:
					CZombieBase		();
	virtual			~CZombieBase	() override;

	virtual void	Load				(LPCSTR section) override;
	virtual BOOL	net_Spawn			(CSE_Abstract* DC) override;
	virtual void	reinit				() override;
	virtual	void	reload				(LPCSTR section) override;
	
	virtual	void	Hit					(SHit* pHDS) override;

	virtual bool	ability_pitch_correction () override  {return false; }

	virtual void	shedule_Update		(u32 dt) override;
	
	static	void 	BoneCallback		(CBoneInstance *B);
			void	vfAssignBones		();

	virtual bool	use_center_to_aim				() const override { return true; }
	virtual	char*	get_monster_class_name () override { return (char*) "zombie"; }

	CBoneInstance			*bone_spine;
	CBoneInstance			*bone_head;

	SAnimationTripleData	anim_triple_death[EntityDefinitions::CZombieBaseDef::FAKE_DEATH_TYPES_COUNT];
	u8				active_triple_idx;
	
	u32				time_dead_start;
	u32				last_hit_frame;
	u32				time_resurrect;

	u8				fake_death_count;
	float			health_death_threshold;
	u8				fake_death_left;

	bool			fake_death_fall_down	();
	void			fake_death_stand_up		();

	DECLARE_SCRIPT_REGISTER_FUNCTION
};