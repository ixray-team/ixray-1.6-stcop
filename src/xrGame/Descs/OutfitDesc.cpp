#include "StdAfx.h"
#include "OutfitDesc.h"

void SCustomOutfitDesc::Load(const shared_str& Section)
{
	XReflect::ReadIni(*this, Section);
}
