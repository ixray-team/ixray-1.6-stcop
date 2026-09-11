#pragma once

#include "PhysicObject.h"

class CShell : public CPhysicObject
{
	using inherited = CPhysicObject;

	struct
	{
		bool is_parent_actor;
		Fvector dir;
		Fvector lin_vel;
		float speed;
		float dispersion;
	} params;
	
	bool need_eject = false;

public:
	virtual bool net_Spawn(CSE_Abstract* e) override;
	virtual void UpdateCL() override;
	
	virtual void PH_A_CrPr() override;
	virtual void PH_B_CrPr() override;

private:
	void Eject();
};
