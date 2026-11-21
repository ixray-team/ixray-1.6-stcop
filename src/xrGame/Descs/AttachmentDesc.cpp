#include "StdAfx.h"
#include "AttachmentDesc.h"

void SMagazineDesc::Load(const shared_str& Section)
{
	XReflect::ReadIni(*this, Section);

	for (const auto& Type : xr_string(pSettings->r_string(Section, "ammo_class")).RemoveWhitespaces().Split(','))
	{
		AmmoTypes.push_back(Type.c_str());
	}
}
