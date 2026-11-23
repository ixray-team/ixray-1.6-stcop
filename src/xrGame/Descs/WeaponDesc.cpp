#include "StdAfx.h"
#include "WeaponDesc.h"

void SWeaponDesc::Load(const shared_str& Section)
{
	XReflect::ReadIni(*this, Section);
}
