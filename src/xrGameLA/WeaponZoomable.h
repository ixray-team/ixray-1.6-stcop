#pragma once
#include "weaponbinoculars.h"
#include "../xrScripts/script_export_space.h"

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
