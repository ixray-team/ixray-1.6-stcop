#pragma once

#include "PhysicObject.h"
#include "ode/contact.h"

struct SGameMtl;

class CShell : public CPhysicObject
{
	using inherited = CPhysicObject;

	struct
	{
		ALife::_OBJECT_ID weapon_id;
		Fvector dir;
		Fvector lin_vel;
		float speed;
		float dispersion;
	} params;
	
	bool need_eject = false;

	float bounce;
	float bounce_vel;

public:
	virtual bool net_Spawn(CSE_Abstract* e) override;
	virtual void UpdateCL() override;
	
	virtual void PH_A_CrPr() override;
	virtual void PH_B_CrPr() override;

private:
	void Eject();

	static void ContactCallback(bool& do_collide, bool bo1, dContact& c, SGameMtl* material_1, SGameMtl* material_2);
};
