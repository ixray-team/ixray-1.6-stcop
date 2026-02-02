#include "StdAfx.h"
#include "ArmorBase.h"
#include "object_broker.h"
#include "Actor.h"
#include "game_cl_base.h"
#include "Level.h"
#include "Inventory.h"
#include "BoneProtections.h"
#include "UIGameCustom.h"

#include "../Include/xrRender/Kinematics.h"

CArmorBase::CArmorBase()
{
	m_HitTypeProtection.resize(ALife::eHitTypeMax);
	for (int i = 0; i < ALife::eHitTypeMax; i++)
	{
		m_HitTypeProtection[i] = 1.0f;
	}

	m_boneProtection = new SBoneProtections();
}

CArmorBase::~CArmorBase()
{
	xr_delete(m_boneProtection);
}

BOOL CArmorBase::net_Spawn(CSE_Abstract* DC)
{
	if (IsGameTypeSingle())
	{
		ReloadBonesProtection();
	}

	BOOL res = inherited::net_Spawn(DC);
	return res;
}

void CArmorBase::net_Export(NET_Packet& P)
{
	inherited::net_Export(P);
	P.w_float_q8(GetCondition(), 0.0f, 1.0f);
}

void CArmorBase::net_Import(NET_Packet& P)
{
	inherited::net_Import(P);
	float _cond;
	P.r_float_q8(_cond, 0.0f, 1.0f);
	SetCondition(_cond);
}

void CArmorBase::OnH_A_Chield()
{
	inherited::OnH_A_Chield();
	if (!IsGameTypeSingle())
	{
		ReloadBonesProtection();
	}
}

void CArmorBase::Load(LPCSTR section)
{
	inherited::Load(section);

	m_HitTypeProtection[ALife::eHitTypeBurn] = pSettings->r_float(section, "burn_protection");
	m_HitTypeProtection[ALife::eHitTypeStrike] = pSettings->r_float(section, "strike_protection");
	m_HitTypeProtection[ALife::eHitTypeShock] = pSettings->r_float(section, "shock_protection");
	m_HitTypeProtection[ALife::eHitTypeWound] = pSettings->r_float(section, "wound_protection");
	m_HitTypeProtection[ALife::eHitTypeRadiation] = pSettings->r_float(section, "radiation_protection");
	m_HitTypeProtection[ALife::eHitTypeTelepatic] = pSettings->r_float(section, "telepatic_protection");
	m_HitTypeProtection[ALife::eHitTypeChemicalBurn] = pSettings->r_float(section, "chemical_burn_protection");
	m_HitTypeProtection[ALife::eHitTypeExplosion] = pSettings->r_float(section, "explosion_protection");
	m_HitTypeProtection[ALife::eHitTypeFireWound] = READ_IF_EXISTS(pSettings, r_float, section, "fire_wound_protection", 0.0f);
	m_HitTypeProtection[ALife::eHitTypePhysicStrike] = READ_IF_EXISTS(pSettings, r_float, section, "physic_strike_protection", m_HitTypeProtection[ALife::eHitTypeStrike]);
	m_HitTypeProtection[ALife::eHitTypeLightBurn] = m_HitTypeProtection[ALife::eHitTypeBurn];
    if (pSettings->line_exist(section, "hit_fraction_actor"))
    {
        m_boneProtection->m_fHitFrac = pSettings->r_float(section, "hit_fraction_actor");

        // Since hit_fraction_actor exists both in CS and COP, but fire_wound_protection was removed in COP,
        // We can use this hacky solution to determine which damage formula to use.
        // It not robust for mods, because they can have fire_wound_protection in configs, despite that
        // original COP engine doesn't read it.
        if (pSettings->line_exist(section, "fire_wound_protection"))
            m_boneProtection->m_hitFracType = SBoneProtections::HitFractionActorCS;
        else
            m_boneProtection->m_hitFracType = SBoneProtections::HitFractionActorCOP;
    }

	if (pSettings->line_exist(section, "nightvision_sect"))
	{
		m_NightVisionSect = pSettings->r_string(section, "nightvision_sect");
	}

	m_fHealthRestoreSpeed = READ_IF_EXISTS(pSettings, r_float, section, "health_restore_speed", 0.0f);
	m_fRadiationRestoreSpeed = READ_IF_EXISTS(pSettings, r_float, section, "radiation_restore_speed", 0.0f);
	m_fSatietyRestoreSpeed = READ_IF_EXISTS(pSettings, r_float, section, "satiety_restore_speed", 0.0f);
	m_fThirstRestoreSpeed = READ_IF_EXISTS(pSettings, r_float, section, "thirst_restore_speed", 0.0f);
	m_fPowerRestoreSpeed = READ_IF_EXISTS(pSettings, r_float, section, "power_restore_speed", 0.0f);
	m_fBleedingRestoreSpeed = READ_IF_EXISTS(pSettings, r_float, section, "bleeding_restore_speed", 0.0f);
	m_fPowerLoss = READ_IF_EXISTS(pSettings, r_float, section, "power_loss", 1.0f);
	clamp(m_fPowerLoss, 0.0f, 1.0f);

	m_BonesProtectionSect = READ_IF_EXISTS(pSettings, r_string, section, "bones_koeff_protection", "");

	bIsHudGasMaskAvailable = !!READ_IF_EXISTS(pSettings, r_bool, section, "hud_gas_mask_avaliable", true);		// FFx0001 ++
	bIsHudRainDropsAvailable = !!READ_IF_EXISTS(pSettings, r_bool, section, "hud_rain_drops_avaliable", true);  // FFx0001 ++

	if (pSettings->line_exist(section, "glass_present"))
	{
		GlassPresent = pSettings->r_bool(section, "glass_present");
	}

	// Added by Axel, to enable optional condition use on any item
	m_flags.set(FUsingCondition, READ_IF_EXISTS(pSettings, r_bool, section, "use_condition", true));
}

void CArmorBase::Hit(float hit_power, ALife::EHitType hit_type)
{
	float hit_power_not_k = hit_power;
	hit_power *= GetHitImmunity(hit_type);
	if (!psActorFlags.test(AF_INFINITE_DURABILITY))
	{
		ChangeCondition(-hit_power);
	}
}

void CArmorBase::AddBonesProtection(LPCSTR bones_section)
{
	CObject* parent = H_Parent();
	if (IsGameTypeSingle())
		parent = smart_cast<CObject*>(Level().CurrentViewEntity());

	if (parent && parent->Visual() && m_BonesProtectionSect.size())
		m_boneProtection->add(bones_section, smart_cast<IKinematics*>(parent->Visual()));
}

void CArmorBase::ReloadBonesProtection()
{
	CObject* parent = H_Parent();
	if (IsGameTypeSingle())
	{
		parent = Level().CurrentViewEntity();
	}

	if (parent && parent->Visual() && m_BonesProtectionSect.size())
	{
		m_boneProtection->reload(m_BonesProtectionSect, PKinematics(parent->Visual()));
	}
}

bool CArmorBase::install_upgrade_impl(LPCSTR section, bool test)
{
	bool result = inherited::install_upgrade_impl(section, test);

	result |= process_if_exists(section, "burn_protection", &CInifile::r_float, m_HitTypeProtection[ALife::eHitTypeBurn], test);
	result |= process_if_exists(section, "shock_protection", &CInifile::r_float, m_HitTypeProtection[ALife::eHitTypeShock], test);
	result |= process_if_exists(section, "strike_protection", &CInifile::r_float, m_HitTypeProtection[ALife::eHitTypeStrike], test);
	result |= process_if_exists(section, "wound_protection", &CInifile::r_float, m_HitTypeProtection[ALife::eHitTypeWound], test);
	result |= process_if_exists(section, "radiation_protection", &CInifile::r_float, m_HitTypeProtection[ALife::eHitTypeRadiation], test);
	result |= process_if_exists(section, "telepatic_protection", &CInifile::r_float, m_HitTypeProtection[ALife::eHitTypeTelepatic], test);
	result |= process_if_exists(section, "chemical_burn_protection", &CInifile::r_float, m_HitTypeProtection[ALife::eHitTypeChemicalBurn], test);
	result |= process_if_exists(section, "explosion_protection", &CInifile::r_float, m_HitTypeProtection[ALife::eHitTypeExplosion], test);
	result |= process_if_exists(section, "fire_wound_protection", &CInifile::r_float, m_HitTypeProtection[ALife::eHitTypeFireWound], test);

	LPCSTR str = {};
	bool result2 = process_if_exists_set(section, "nightvision_sect", &CInifile::r_string, str, test);
	if (result2 && !test)
	{
		m_NightVisionSect._set(str);
	}
	result |= result2;

	result |= process_if_exists(section, "health_restore_speed", &CInifile::r_float, m_fHealthRestoreSpeed, test);
	result |= process_if_exists(section, "radiation_restore_speed", &CInifile::r_float, m_fRadiationRestoreSpeed, test);
	result |= process_if_exists(section, "satiety_restore_speed", &CInifile::r_float, m_fSatietyRestoreSpeed, test);
	result |= process_if_exists(section, "thirst_restore_speed", &CInifile::r_float, m_fThirstRestoreSpeed, test);
	result |= process_if_exists(section, "power_restore_speed", &CInifile::r_float, m_fPowerRestoreSpeed, test);
	result |= process_if_exists(section, "bleeding_restore_speed", &CInifile::r_float, m_fBleedingRestoreSpeed, test);

	result |= process_if_exists(section, "power_loss", &CInifile::r_float, m_fPowerLoss, test);
	clamp(m_fPowerLoss, 0.0f, 1.0f);

	result2 = process_if_exists_set(section, "bones_koeff_protection", &CInifile::r_string, str, test);
	if (result2 && !test)
	{
		m_BonesProtectionSect = str;
		ReloadBonesProtection();
	}
	result2 = process_if_exists_set(section, "bones_koeff_protection_add", &CInifile::r_string, str, test);
	if (result2 && !test)
	{
		AddBonesProtection(str);
	}

	return result;
}

float CArmorBase::HitThroughArmor(float hit_power, s16 element, float ap, bool& add_wound, ALife::EHitType hit_type)
{
	float NewHitPower = hit_power;

	switch (m_boneProtection->m_hitFracType)
	{
	default:
	case SBoneProtections::HitFractionActorCOP:
	{
		if (hit_type == ALife::eHitTypeFireWound)
		{
			const float ba = GetBoneArmor(element);
			if (ba < 0.0f)
				return NewHitPower;

			float BoneArmor = ba * GetCondition();
			if (/*!fis_zero(ba, EPS) &&*/ ap > BoneArmor)
			{
				//пуля пробила бронь
				if (!IsGameTypeSingle())
				{
					float hit_fraction = (ap - BoneArmor) / ap;
					if (hit_fraction < m_boneProtection->m_fHitFrac)
						hit_fraction = m_boneProtection->m_fHitFrac;

					NewHitPower *= hit_fraction;
					NewHitPower *= m_boneProtection->getBoneProtection(element);
				}

				VERIFY(NewHitPower >= 0.0f);
			}
			else
			{
				//пуля НЕ пробила бронь
				NewHitPower *= m_boneProtection->m_fHitFrac;
				add_wound = false; 	//раны нет
			}
		}
		else
		{
			float one = 0.1f;
			if (hit_type == ALife::eHitTypeStrike ||
				hit_type == ALife::eHitTypeWound ||
				hit_type == ALife::eHitTypeWound_2 ||
				hit_type == ALife::eHitTypeExplosion)
			{
				one = 1.0f;
			}
			const float protect = GetDefHitTypeProtection(hit_type);
			NewHitPower -= protect * one;

			if (NewHitPower < 0.f)
				NewHitPower = 0.f;
		}

		//увеличить изношенность костюма
		Hit(hit_power, hit_type);
		break;
	}
	case SBoneProtections::HitFractionActorCS:
	{
		if (hit_type == ALife::eHitTypeFireWound)
		{
			const float BoneArmor = m_boneProtection->getBoneArmor(element) * GetCondition();

			if (ap > EPS && ap > BoneArmor)
			{
				//пуля пробила бронь
				const float d_ap = ap - BoneArmor;
				NewHitPower *= (d_ap / ap);

				if (NewHitPower < m_boneProtection->m_fHitFrac)
					NewHitPower = m_boneProtection->m_fHitFrac;

				if (!IsGameTypeSingle())
				{
					NewHitPower *= m_boneProtection->getBoneProtection(element);
				}

				if (NewHitPower < 0.0f)
					NewHitPower = 0.0f;
			}
			else
			{
				//пуля НЕ пробила бронь
				NewHitPower *= m_boneProtection->m_fHitFrac;
				add_wound = false; //раны нет
			}
		}
		else
		{
			float one = 0.1f;
			if (hit_type == ALife::eHitTypeWound ||
				hit_type == ALife::eHitTypeWound_2 ||
				hit_type == ALife::eHitTypeExplosion)
			{
				one = 1.0f;
			}

			const float protect = GetHitTypeProtection(hit_type, element);
			NewHitPower -= protect * one;
			if (NewHitPower < 0.0f)
				NewHitPower = 0.0f;
		}

		//увеличить изношенность костюма
		Hit(NewHitPower, hit_type);
		break;
	}
	case SBoneProtections::HitFraction:
	{
		if (hit_type == ALife::eHitTypeFireWound)
		{
			const float BoneArmor = m_boneProtection->getBoneArmor(element) * GetCondition() * (1 - ap);
			NewHitPower -= BoneArmor;
			if (NewHitPower < hit_power * m_boneProtection->m_fHitFrac)
				NewHitPower = hit_power * m_boneProtection->m_fHitFrac;
		}
		else
		{
			NewHitPower *= GetHitTypeProtection(hit_type, element);
		}

		//увеличить изношенность костюма
		Hit(hit_power, hit_type);
		break;
	}
	} // switch (m_boneProtection->m_hitFracType)

	return NewHitPower;
}

float CArmorBase::GetDefHitTypeProtection(ALife::EHitType hit_type)
{
    float base = m_HitTypeProtection[hit_type] * GetCondition();

    if (m_boneProtection->m_hitFracType == SBoneProtections::HitFraction)
        return 1.0f - base; // SOC

    return base; // CS/COP
}

float CArmorBase::GetHitTypeProtection(ALife::EHitType hit_type, s16 element)
{
    float base = m_HitTypeProtection[hit_type] * GetCondition();
    float bone = m_boneProtection->getBoneProtection(element);

    if (m_boneProtection->m_hitFracType == SBoneProtections::HitFraction)
        return 1.0f - base * bone; // SOC

    return base * bone; // CS/COP
}

float CArmorBase::GetBoneArmor(s16 element)
{
	return m_boneProtection->getBoneArmor(element);
}
