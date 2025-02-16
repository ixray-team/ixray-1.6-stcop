#pragma once

#include "WeaponCustomPistol.h"
#include "../xrScripts/script_export_space.h"

class CUIFrameWindow;
class CUIStatic;
class CBinocularsVision;

// CBinocularsVision usage moved to CWeapon
class CWeaponBinoculars: public CWeaponCustomPistol
{
private:
	typedef CWeaponCustomPistol inherited;

public:
					CWeaponBinoculars	(); 
	virtual			~CWeaponBinoculars	();

	void			Load				(LPCSTR section);

	virtual void	OnZoomIn			();
	virtual void	OnZoomOut			();
	virtual	void	ZoomInc				();
	virtual	void	ZoomDec				();
	virtual BOOL	net_Spawn			(CSE_Abstract* DC);

	//tatarinrafa: Uncoment when needed
//	virtual void	save				(NET_Packet &output_packet);
//	virtual void	load				(IReader &input_packet);

	virtual bool	Action				(u16 cmd, u32 flags);
	virtual bool	use_crosshair		()	const {return false;}
	virtual void	GetBriefInfo		(xr_string& str_name, xr_string& icon_sect_name, xr_string& str_count);

	virtual LPCSTR	GetCurrentFireModeStr	() {return " ";};
protected:

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
