#pragma once
#include "../basemonster/base_monster.h"
#include "../controlled_entity.h"
#include "../ai_monster_bones.h"
#include "../anim_triple.h"
#include "../../../../xrScripts/script_export_space.h"

#define FAKE_DEATH_TYPES_COUNT	4

class CZombie final :	public CBaseMonster,
				public CControlledEntity<CZombie> {
	
	typedef		CBaseMonster				inherited;
	typedef		CControlledEntity<CZombie>	CControlled;

	bonesManipulation	Bones;

	void StartFakeDeathRagdoll();
	void StartFakeDeathVanilla();
	void StopFakeDeathRagdoll();

public:
					CZombie		();
	virtual			~CZombie	();	

	virtual void	Load				(const char* section);
	virtual bool	net_Spawn			(CSE_Abstract* DC);
	virtual void	net_Destroy() override;
	virtual void	reinit				();
	virtual	void	reload				(const char* section);
	
	virtual	void	Hit					(SHit* pHDS);

	virtual bool	ability_pitch_correction () {return false;}
	virtual bool processing_enabled() override { return IsFakeDeathActive || inherited::processing_enabled(); }

	virtual void	shedule_Update		(u32 dt);
	
	static	void 	BoneCallback		(CBoneInstance *B);
			void	vfAssignBones		();

	virtual bool	use_center_to_aim				() const {return true;}
	virtual	char*	get_monster_class_name () { return (char*) "zombie"; }

	void Die(CObject* who) override;
	virtual bool ShouldMarkAsEnemy() override { return !IsFakeDeathActive; }
	
	virtual void save(NET_Packet &output_packet) override;
	virtual void load(IReader &input_packet) override;
	virtual void Serialize(ISaveObject& Object) override;

	CBoneInstance* bone_spine;
	CBoneInstance* bone_head;

	SAnimationTripleData anim_triple_death[FAKE_DEATH_TYPES_COUNT];
	u8 active_triple_idx;
	
	u32 time_dead_duration;
	u32 time_resurrect_duration;
	u32 time_out_frustum_duration;
	
	u32 time_dead_start;
	u32 last_hit_frame;
	u32 time_resurrect;
	u32 time_out_frustum;

	u8 fake_death_count;
	float health_death_threshold;
	u8 fake_death_left;
	bool IsFakeDeathActive = false;
	bool IsSpatialReactSound = false;
	bool IsSpatialPhysMove = false;

	bool			fake_death_fall_down	(); //return true if everything is ok
	void			fake_death_stand_up		();

#ifdef _DEBUG
	virtual void	debug_on_key			(int key);
#endif


	DECLARE_SCRIPT_REGISTER_FUNCTION
};