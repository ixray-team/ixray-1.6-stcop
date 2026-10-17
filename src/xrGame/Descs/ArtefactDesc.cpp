#include "StdAfx.h"
#include "ArtefactDesc.h"

void SArtefactDesc::Load(const shared_str& Section)
{
	if (!Section.size())
	{
		return;
	}

	XReflect::ReadIni(*this, Section);

	const char* HitAbsorbationSect = pSettings->r_string(Section, "hit_absorbation_sect");
	if (pSettings->section_exist(HitAbsorbationSect))
	{
		m_ArtefactHitImmunities.LoadImmunities(HitAbsorbationSect, pSettings);
	}

	m_bCanSpawnZone = !!pSettings->line_exist("artefact_spawn_zones", Section);
}
