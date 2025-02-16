////////////////////////////////////////////////////////////////////////////
//	Module		: weapon_collision.cpp
//	Created		: 12/10/2012
//	Modified 	: 22/10/2015
//	Author		: lost alpha (SkyLoader)
//	Description	: weapon HUD collision
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "weapon_collision.h"
#include "actor.h"

CWeaponCollision::CWeaponCollision()
{
	fReminderStrafe		= new CEnvelope();
	bFirstUpdate		= false;
}

CWeaponCollision::~CWeaponCollision()
{
	xr_delete		(fReminderStrafe);
}

void CWeaponCollision::Load(Fmatrix &o)
{
	Fvector	xyz	= o.c;
	Fvector	dir;
	o.getHPB(dir.x,dir.y,dir.z);

	fReminderDist		= xyz.z;
	fReminderNeedDist	= xyz.z;
	fReminderNeedStrafe	= dir.z;
	fReminderStrafe->InsertKey(Device.dwTimeGlobal, dir.z);
	fNewStrafeTime	= Device.dwTimeGlobal;
}

void CWeaponCollision::CheckState()
{
	dwMState		= Actor()->MovingState();
	is_limping		= Actor()->IsLimping();
	is_zoom			= Actor()->IsZoomAimingMode();
}

static const float COLLISION_SPEED_REMINDER = 1.f;
static const u16 TIME_REMINDER_STRAFE = 300;
static const float STRAFE_ANGLE = 0.1f;

void CWeaponCollision::UpdateCollision(Fmatrix &o, float range)
{
	if (!bFirstUpdate)
	{
		Load		(o);
		bFirstUpdate 	= true;
	}

	Fvector	pos	= o.c;

	if (range < 0.8f && !is_zoom)
		fReminderNeedDist	= pos.z - ((1.f - range - 0.2f) * 0.6f);
	else
		fReminderNeedDist	= pos.z;

	if (!fsimilar(fReminderDist, fReminderNeedDist)) {
		if (fReminderDist < fReminderNeedDist) {
			fReminderDist += COLLISION_SPEED_REMINDER * Device.fTimeDelta;
			if (fReminderDist > fReminderNeedDist)
				fReminderDist = fReminderNeedDist;
		} else if (fReminderDist > fReminderNeedDist) {
			fReminderDist -= COLLISION_SPEED_REMINDER * Device.fTimeDelta;
			if (fReminderDist < fReminderNeedDist)
				fReminderDist = fReminderNeedDist;
		}
	}

	if (!fsimilar(fReminderDist, pos.z))
	{
		pos.z 		= fReminderDist;
		o.c.set(pos);
	}
}

void CWeaponCollision::UpdateStrafes(Fmatrix &o)
{
	if (!bFirstUpdate)
	{
		Load		(o);
		bFirstUpdate 	= true;
	}

	Fvector	dir;
	o.getHPB(dir.x,dir.y,dir.z);

	float fLastStrafe = fReminderNeedStrafe;
	if ((dwMState&ACTOR_DEFS::mcLStrafe || dwMState&ACTOR_DEFS::mcRStrafe) && !is_zoom)
	{
		float k	= ((dwMState & ACTOR_DEFS::mcCrouch) ? 0.5f : 1.f);
		if (dwMState&ACTOR_DEFS::mcLStrafe)
			k *= -1.f;

		fReminderNeedStrafe	= dir.z + (STRAFE_ANGLE * k);

		if (dwMState&ACTOR_DEFS::mcFwd || dwMState&ACTOR_DEFS::mcBack)
			fReminderNeedStrafe	/= 2.f;

	} else fReminderNeedStrafe = dir.z;

	float result;
	if (fNewStrafeTime>(float)Device.dwTimeGlobal)
		result = fReminderStrafe->Evaluate(Device.dwTimeGlobal);
	else {
		if (fReminderStrafe->keys.size()>0)
			result = fReminderStrafe->Evaluate(fReminderStrafe->keys.back()->time);
	}

	if (!fsimilar(fLastStrafe, fReminderNeedStrafe))
	{
		float time_new = TIME_REMINDER_STRAFE+Device.dwTimeGlobal;

		fReminderStrafe->DeleteLastKeys(Device.dwTimeGlobal);

		if (!fsimilar(result, 0.f))
			fReminderStrafe->InsertKey(Device.dwTimeGlobal, result);
		else
			fReminderStrafe->InsertKey(Device.dwTimeGlobal, dir.z);
		fReminderStrafe->InsertKey(time_new, fReminderNeedStrafe);

		fNewStrafeTime			= time_new;

		result = fReminderStrafe->Evaluate(Device.dwTimeGlobal);
	}

	if (!fsimilar(result, dir.z))
	{
		dir.z 		= result;
		Fmatrix m;
		m.setHPB(dir.x,dir.y,dir.z);
		Fmatrix tmp;
		tmp.mul_43(o, m);
		o.set(tmp);
	} else {
		if (fReminderStrafe->keys.size()>10 && fNewStrafeTime<(float)Device.dwTimeGlobal)
		{
			fReminderStrafe->Clear();	//clear all keys
			fReminderStrafe->InsertKey(Device.dwTimeGlobal, fReminderNeedStrafe);
			fNewStrafeTime			= Device.dwTimeGlobal;
		}
	}

}