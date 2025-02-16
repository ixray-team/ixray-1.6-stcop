#include "stdafx.h"
#include "WeaponZoomable.h"

CWeaponZoomable::CWeaponZoomable() : CWeaponBinoculars()
{
}

CWeaponZoomable::~CWeaponZoomable()
{
}

void CWeaponZoomable::Load	(LPCSTR section)
{
	inherited::Load(section);
}

void CWeaponZoomable::GetBriefInfo(xr_string& str_name, xr_string& icon_sect_name, xr_string& str_count)
{
		CWeaponCustomPistol::GetBriefInfo		(str_name, icon_sect_name, str_count);
}

bool CWeaponZoomable::Action(u16 cmd, u32 flags) 
{
	return CWeaponCustomPistol::Action(cmd, flags);
}

void CWeaponZoomable::OnZoomIn		()
{
		inherited::OnZoomIn();
}

void CWeaponZoomable::OnZoomOut		()
{
		inherited::OnZoomOut();
}

void CWeaponZoomable::ZoomInc()
{
	inherited::ZoomInc();
}

void CWeaponZoomable::ZoomDec()
{
	inherited::ZoomDec();
}