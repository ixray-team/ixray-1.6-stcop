#include "StdAfx.h"
#include "BottleItemDesc.h"

void SBottleItemDesc::Load(const shared_str& Section)
{
	BreakParticles = pSettings->read_if_exists<str_c>(Section, "break_particles", nullptr);
	BreakSound = pSettings->read_if_exists<str_c>(Section, "break_sound", nullptr);
}
