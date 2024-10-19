#pragma once
#include "../../ai_entity_definitions.h"
#include "../BaseMonster/base_monster.h"
#include "../../../../xrScripts/script_export_space.h"

class CPseudoDogBase : public CBaseMonster 
{
protected:
	using	inherited =	CBaseMonster	;

public:
	float			m_anger_hunger_threshold;
	float			m_anger_loud_threshold;

	TTime			m_time_became_angry;

	TTime			time_growling;			// время нахождения в состоянии пугания

	enum {
		eAdditionalSounds		= MonsterSound::eMonsterSoundCustom,
		ePsyAttack				= eAdditionalSounds | 0,
	};

					CPseudoDogBase		();
	virtual			~CPseudoDogBase		() override;	

	virtual DLL_Pure	*_construct		() override;

	virtual void	Load				(LPCSTR section) override;

	virtual void	reinit				() override;
	virtual void	reload				(LPCSTR section) override;

	virtual bool	ability_can_drag	() override { return true; }
	virtual bool	ability_psi_attack	() override { return true; }

	virtual void	CheckSpecParams		(u32 spec_params) override;

	virtual void	HitEntityInJump		(const CEntity *pEntity) override;

	IStateManagerBase *create_state_manager();

	virtual	char*	get_monster_class_name () override { return (char*)"pseudodog"; }

private:

	DECLARE_SCRIPT_REGISTER_FUNCTION
};