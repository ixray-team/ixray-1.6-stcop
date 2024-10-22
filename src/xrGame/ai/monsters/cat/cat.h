#pragma once
#include "../BaseMonster/base_monster.h"
#include "../../../../xrScripts/script_export_space.h"

class CCatBase : public CBaseMonster{
protected:
	using		inherited = CBaseMonster;

public:
	CCatBase();
	virtual			~CCatBase() override;

	virtual void	Load				(LPCSTR section) override;
	virtual void	reinit				() override;

	virtual	void	UpdateCL			() override;

	virtual void	CheckSpecParams		(u32 spec_params) override;

			void	try_to_jump			();

	virtual	void	HitEntityInJump		(const CEntity *pEntity) override;

	virtual	char*	get_monster_class_name () override { return (char*) "cat"; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};