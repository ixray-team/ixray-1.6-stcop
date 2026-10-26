#pragma once
#include "../basemonster/base_monster.h"
#include "../controlled_entity.h"
#include "../../../../xrScripts/script_export_space.h"

class CAI_Flesh final : public CBaseMonster,
				  public CControlledEntity<CAI_Flesh> {

	typedef		CBaseMonster					inherited;
	typedef		CControlledEntity<CAI_Flesh>	CControlled;

public:
							CAI_Flesh		();
	virtual ~CAI_Flesh		() override;

	virtual void	Load					(const char* section) override;
	virtual bool	net_Spawn				(CSE_Abstract* DC) override;

	virtual void	CheckSpecParams			(u32 spec_params) override;

	virtual bool	ability_can_drag		() override {return true;}

	virtual char*	get_monster_class_name () override { return (char*)"flesh"; }


private:
	bool	ConeSphereIntersection	(Fvector ConeVertex, float ConeAngle, Fvector ConeDir, 
									Fvector SphereCenter, float SphereRadius);
	
	DECLARE_SCRIPT_REGISTER_FUNCTION
};