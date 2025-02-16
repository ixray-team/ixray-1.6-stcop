// hit_immunity.cpp:	êëàññ äëÿ òåõ îáúåêòîâ, êîòîðûå ïîääåðæèâàþò
//						êîýôôèöèåíòû èììóíèòåòà äëÿ ðàçíûõ òèïîâ õèòîâ
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "hit_immunity.h"

CHitImmunity::CHitImmunity()
{
	m_HitImmunityKoefs.resize(ALife::eHitTypeMax);
	for(int i=0; i<ALife::eHitTypeMax; i++)
		m_HitImmunityKoefs[i] = 1.0f;
}


CHitImmunity::~CHitImmunity()
{
}
void CHitImmunity::LoadImmunities(LPCSTR imm_sect, CInifile const * ini)
{
	R_ASSERT2	(ini->section_exist(imm_sect), imm_sect);

	m_HitImmunityKoefs[ALife::EHitType::eHitTypeBurn]			= ini->r_float(imm_sect,"burn_immunity");
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeStrike]		= ini->r_float(imm_sect,"strike_immunity");
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeShock]		= ini->r_float(imm_sect,"shock_immunity");
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeWound]		= ini->r_float(imm_sect,"wound_immunity");
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeRadiation]	= ini->r_float(imm_sect,"radiation_immunity");
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeTelepatic]	= ini->r_float(imm_sect,"telepatic_immunity");
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeChemicalBurn] = ini->r_float(imm_sect,"chemical_burn_immunity");
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeExplosion]	= ini->r_float(imm_sect,"explosion_immunity");
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeFireWound]	= ini->r_float(imm_sect,"fire_wound_immunity");
	m_HitImmunityKoefs[ALife::EHitType::eHitTypePhysicStrike]	= READ_IF_EXISTS(ini, r_float, imm_sect,"physic_strike_wound_immunity", 1.0f);
//	m_HitImmunityKoefs[ALife::eHitTypeLightBurn]	= m_HitImmunityKoefs[ALife::eHitTypeBurn];
}

void CHitImmunity::AddImmunities(LPCSTR imm_sect, CInifile const * ini)
{
	R_ASSERT2	(ini->section_exist(imm_sect), imm_sect);

	m_HitImmunityKoefs[ALife::EHitType::eHitTypeBurn]			+= READ_IF_EXISTS(ini, r_float, imm_sect,"burn_immunity", 0.0f);
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeStrike]		+= READ_IF_EXISTS(ini, r_float, imm_sect,"strike_immunity", 0.0f);
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeShock]		+= READ_IF_EXISTS(ini, r_float, imm_sect,"shock_immunity", 0.0f);
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeWound]		+= READ_IF_EXISTS(ini, r_float, imm_sect,"wound_immunity", 0.0f);
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeRadiation]	+= READ_IF_EXISTS(ini, r_float, imm_sect,"radiation_immunity", 0.0f);
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeTelepatic]	+= READ_IF_EXISTS(ini, r_float, imm_sect,"telepatic_immunity", 0.0f);
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeChemicalBurn] += READ_IF_EXISTS(ini, r_float, imm_sect,"chemical_burn_immunity", 0.0f);
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeExplosion]	+= READ_IF_EXISTS(ini, r_float, imm_sect,"explosion_immunity", 0.0f);
	m_HitImmunityKoefs[ALife::EHitType::eHitTypeFireWound]	+= READ_IF_EXISTS(ini, r_float, imm_sect,"fire_wound_immunity", 0.0f);
	m_HitImmunityKoefs[ALife::EHitType::eHitTypePhysicStrike]	+= READ_IF_EXISTS(ini, r_float, imm_sect,"physic_strike_wound_immunity", 0.0f);
//	m_HitImmunityKoefs[ALife::eHitTypeLightBurn]	= m_HitImmunityKoefs[ALife::eHitTypeBurn];
}