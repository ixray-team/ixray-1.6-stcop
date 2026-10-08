#include "StdAfx.h"
#include "BottleItemDesc.h"

void SBottleItemDesc::Load(const shared_str& Section)
{
	BreakParticles = READ_IF_EXISTS(pSettings, r_string, Section, "break_particles", nullptr);
	BreakSound = READ_IF_EXISTS(pSettings, r_string, Section, "break_sound", nullptr);
}
