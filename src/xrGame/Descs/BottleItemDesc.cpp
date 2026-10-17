#include "StdAfx.h"
#include "BottleItemDesc.h"

void SBottleItemDesc::Load(const shared_str& Section)
{
	XReflect::ReadIni(*this, Section);
}
