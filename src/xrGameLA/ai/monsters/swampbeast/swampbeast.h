#pragma once
#include "../BaseMonster/base_monster.h"
#include "../controlled_entity.h"
#include "../../../../xrScripts/script_export_space.h"

class CAI_SwampBeast : public CBaseMonster,
				  public CControlledEntity<CAI_SwampBeast> {

	typedef		CBaseMonster					inherited;
	typedef		CControlledEntity<CAI_SwampBeast>	CControlled;

public:
							CAI_SwampBeast		();
	virtual					~CAI_SwampBeast		();	
	
	virtual	void	Load					(LPCSTR section);
	virtual	BOOL	net_Spawn				(CSE_Abstract* DC);

	virtual	void	CheckSpecParams			(u32 spec_params);

	virtual bool	ability_can_drag		() {return true;}

private:
	bool	ConeSphereIntersection	(Fvector ConeVertex, float ConeAngle, Fvector ConeDir, 
									Fvector SphereCenter, float SphereRadius);
	
	DECLARE_SCRIPT_REGISTER_FUNCTION
};

