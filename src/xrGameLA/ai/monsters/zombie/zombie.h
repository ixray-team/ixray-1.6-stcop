#pragma once
#include "../BaseMonster/base_monster.h"
#include "../controlled_entity.h"
#include "../controlled_actor.h"
#include "../ai_monster_bones.h"
#include "../anim_triple.h"
#include "../../../../xrScripts/script_export_space.h"

#define FAKE_DEATH_TYPES_COUNT	4

class CZombie :	public CBaseMonster,
				public CControlledEntity<CZombie>, public CControlledActor {
	
	typedef		CBaseMonster				inherited;
	typedef		CControlledEntity<CZombie>	CControlled;

	bonesManipulation	Bones;

public:
					CZombie		();
	virtual			~CZombie	();	

	virtual void	Load				(LPCSTR section);
	virtual BOOL	net_Spawn			(CSE_Abstract* DC);
	virtual void	reinit				();
	virtual	void	reload				(LPCSTR section);
	
	virtual	void	Hit					(SHit* pHDS);

	virtual bool	ability_pitch_correction () {return false;}
	virtual void	UpdateCL				();
	virtual void	shedule_Update		(u32 dt);
	
	static	void 	BoneCallback		(CBoneInstance *B);
			void	vfAssignBones		();

	virtual bool	use_center_to_aim				() const {return true;}

		void			ActivateChokeEffector	();

private:
		void			LoadChokePPEffector	(LPCSTR section);

public:

	CBoneInstance			*bone_spine;
	CBoneInstance			*bone_head;

	SAnimationTripleData	anim_triple_death[FAKE_DEATH_TYPES_COUNT];
	SAnimationTripleData	anim_triple_choke;

	SPPInfo				pp_choke_effector;
	float				m_choke_want_value;
	float				m_choke_want_speed;

	u8				active_triple_idx;
	
	u32				time_dead_start;
	u32				last_hit_frame;
	u32				time_resurrect;
	bool				fakedeath_is_active;

	u8				fake_death_count;
	float			health_death_threshold;
	u8				fake_death_left;

	bool			fake_death_fall_down	(); //return true if everything is ok
	void			fake_death_stand_up		();
	virtual bool			fake_death_is_active	() const { return fakedeath_is_active;}
	IC		bool			WantChoke				() {return m_choke_want_value >= 1.f;}
	IC		void			ChokeCompleted			() {m_choke_want_value = 0.f;}

#ifdef _DEBUG
	virtual void	debug_on_key			(int key);
#endif


	DECLARE_SCRIPT_REGISTER_FUNCTION
};