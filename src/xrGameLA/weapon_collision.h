////////////////////////////////////////////////////////////////////////////
//	Module		: weapon_collision.h
//	Created		: 12/10/2012
//	Modified	: 24/10/2015
//	Author		: lost alpha (SkyLoader)
//	Description	: weapon collision
////////////////////////////////////////////////////////////////////////////
#pragma once

#include "../envelope.h"

class CWeaponCollision
{
	public:
		CWeaponCollision();
		virtual ~CWeaponCollision();
		void		Load					(Fmatrix &o);
		void		UpdateCollision				(Fmatrix &o, float range);
		void		UpdateStrafes				(Fmatrix &o);
		void		CheckState				();

	private:
		float		fReminderDist;
		float		fReminderNeedDist;
		CEnvelope*	fReminderStrafe;
		float		fReminderNeedStrafe;
		float		fNewStrafeTime;
		u32		dwMState;
		bool		is_limping;
		bool		is_zoom;
		bool		bFirstUpdate;
};