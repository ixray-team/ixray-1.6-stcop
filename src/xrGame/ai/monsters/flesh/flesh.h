#pragma once
#include "../BaseMonster/base_monster.h"
#include "../controlled_entity.h"
#include "../../../../xrScripts/script_export_space.h"

class CFleshBase : public CBaseMonster,
				  public CControlledEntity {
protected:
	using		inherited = CBaseMonster					;
	using		CControlled = CControlledEntity	;

public:
	CFleshBase();
	virtual					~CFleshBase() override;
	
	virtual	void	Load					(LPCSTR section) override;
	virtual	BOOL	net_Spawn				(CSE_Abstract* DC) override;

	virtual	void	CheckSpecParams			(u32 spec_params) override;

	virtual bool	ability_can_drag		() override { return true; }

	virtual	char*	get_monster_class_name () override { return (char*)"flesh"; }


private:
	bool	ConeSphereIntersection	(Fvector ConeVertex, float ConeAngle, Fvector ConeDir, 
									Fvector SphereCenter, float SphereRadius);
	
	DECLARE_SCRIPT_REGISTER_FUNCTION
};