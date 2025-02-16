#pragma once
#include "weaponbinoculars.h"
#include "script_export_space.h"

class CWeaponZoomable :	public CWeaponBinoculars
{
private:
	typedef CWeaponBinoculars inherited;
public:
			CWeaponZoomable();
	virtual	~CWeaponZoomable();

	void			Load				(LPCSTR section);

	virtual void	OnZoomIn			();
	virtual void	OnZoomOut			();
	virtual	void	ZoomInc				();
	virtual	void	ZoomDec				();

	virtual bool	Action				(u16 cmd, u32 flags);
	virtual void	GetBriefInfo		(xr_string& str_name, xr_string& icon_sect_name, xr_string& str_count);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};

add_to_type_list(CWeaponZoomable)
#undef script_type_list
#define script_type_list save_type_list(CWeaponZoomable)