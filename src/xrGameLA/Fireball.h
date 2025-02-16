#pragma once
#include "bolt.h"
#include "WeaponKnife.h"
#include "script_export_space.h"
#include "../gamemtllib.h"
#include "level.h"

class CFireball :
	public CWeaponKnife
{
	typedef CWeaponKnife inherited;
public:
							CFireball();
	virtual					~CFireball(void);

	virtual void 			Load						(LPCSTR section);
	virtual bool			Action						(u16 cmd, u32 flags);
	virtual void			OnStateSwitch				(u32 S);
	virtual void			OnAnimationEnd				(u32 state);
	virtual void			switch2_PreFire				();
	virtual	void			LoadFireParams				(LPCSTR section, LPCSTR prefix);
	virtual void			UpdateCL					();
public:
	DECLARE_SCRIPT_REGISTER_FUNCTION
};

add_to_type_list(CFireball)
#undef script_type_list
#define script_type_list save_type_list(CFireball)
