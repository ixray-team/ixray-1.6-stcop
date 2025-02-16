#ifndef __XR_WEAPON_FN2000_H__
#define __XR_WEAPON_FN2000_H__

#pragma once

#include "WeaponMagazined.h"
#include "../xrScripts/script_export_space.h"

class CWeaponFN2000: public CWeaponMagazined
{
private:
	typedef CWeaponMagazined inherited;
public:
	CWeaponFN2000	();
	virtual			~CWeaponFN2000	();
/*	virtual void	Load			(LPCSTR section);
	virtual void			Fire2Start			();
	virtual void			Fire2End			();

	virtual bool Action(u16 cmd, u32 flags);*/

	DECLARE_SCRIPT_REGISTER_FUNCTION
};

#endif //__XR_WEAPON_FN2000_H__
