#pragma once

#include "../basemonster/base_monster.h"
#include "../../../../xrScripts/script_export_space.h"

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CAI_PseudoDog : public CBaseMonster {
	typedef		CBaseMonster	inherited;

public:

	float			m_anger_hunger_threshold;
	float			m_anger_loud_threshold;

	TTime			m_time_became_angry;

	TTime			time_growling;			// время нахождения в состоянии пугания

	enum {
		eAdditionalSounds		= MonsterSound::eMonsterSoundCustom,
		ePsyAttack				= eAdditionalSounds | 0,
	};
public:
					CAI_PseudoDog		();
	virtual ~CAI_PseudoDog		() override;

	virtual DLL_Pure	*_construct		() override;

	virtual void	Load				(const char* section) override;

	virtual void	reinit				() override;
	virtual void	reload				(const char* section) override;

	virtual bool	ability_can_drag	() override {return true;}
	virtual bool	ability_psi_attack	() override {return true;}

	virtual void	CheckSpecParams		(u32 spec_params) override;
	//virtual void	play_effect_sound	();

	virtual void	HitEntityInJump		(const CEntity *pEntity) override;

	virtual IStateManagerBase *create_state_manager	();
	virtual char*	get_monster_class_name () override { return (char*)"pseudodog"; }

private:
#ifdef _DEBUG	
	virtual void	debug_on_key		(int key);
#endif


};