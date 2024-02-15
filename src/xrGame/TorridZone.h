#pragma once
#include "mosquitobald.h"

class CObjectAnimator;

class CTorridZone :public CMosquitoBald
{
private:
	typedef	CCustomZone	inherited;
	CObjectAnimator		*m_animator;
public:
						CTorridZone			();
	virtual				~CTorridZone		();
	virtual void		UpdateWorkload		(u32 dt);
	virtual void		shedule_Update		(u32 dt);
	BOOL				net_Spawn			(CSE_Abstract* DC);
	virtual void		net_Destroy			();
	virtual bool		IsVisibleForZones	() { return true;		}
	virtual	bool		Enable				();
	virtual	bool		Disable				();
	virtual bool		register_schedule() const {return false;}
	// Lain: added
	virtual bool        light_in_slow_mode  ();
	virtual BOOL        AlwaysTheCrow       ();
};