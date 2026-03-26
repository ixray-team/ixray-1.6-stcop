#include "StdAfx.h"
#include "EntityCondition.h"
#include "InventoryOwner.h"
#include "CustomOutfit.h"
#include "Inventory.h"
#include "Wound.h"
#include "Level.h"
#include "game_cl_base.h"
#include "entity_alive.h"
#include "../Include/xrRender/KinematicsAnimated.h"
#include "../Include/xrRender/Kinematics.h"
#include "object_broker.h"
#include "ActorHelmet.h"
#include "../xrCore/Save/SaveObject.h"
#include "Actor.h"

#define MAX_HEALTH 1.0f
#define MIN_HEALTH -0.01f


#define MAX_POWER 1.0f
#define MAX_RADIATION 1.0f
#define MAX_PSY_HEALTH 1.0f

CEntityConditionSimple::CEntityConditionSimple()
{
	max_health()		= MAX_HEALTH;
	SetHealth			( MAX_HEALTH );
}

CEntityConditionSimple::~CEntityConditionSimple()
{}

HitImmunity::HitTypeSVec CEntityCondition::m_GlobalWoundsFactorsForHitTypes = {};
HitImmunity::HitTypeSVec CEntityCondition::m_GlobalBleedingsFactorsForHitTypes = {};

CEntityCondition::CEntityCondition(CEntityAlive *object)
:CEntityConditionSimple()
{
	VERIFY				(object);

	m_object			= object;

	m_use_limping_state = false;
	m_iLastTimeCalled	= 0;
	m_bTimeValid		= false;

	m_fPowerMax			= MAX_POWER;
	m_fRadiationMax		= MAX_RADIATION;
	m_fPsyHealthMax		= MAX_PSY_HEALTH;
	m_fEntityMorale		=  m_fEntityMoraleMax = 1.f;


	m_fPower			= MAX_POWER;
	m_fRadiation		= 0;
	m_fPsyHealth		= MAX_PSY_HEALTH;

	m_fMinWoundSize			= 0.00001f;

	
	m_fHealthHitPart		= 1.0f;
	m_fPowerHitPart			= 0.5f;

	m_fBoostBurnImmunity			= 0.f;
	m_fBoostShockImmunity			= 0.f;
	m_fBoostRadiationImmunity		= 0.f;
	m_fBoostTelepaticImmunity		= 0.f;
	m_fBoostChemicalBurnImmunity	= 0.f;
	m_fBoostExplImmunity			= 0.f;
	m_fBoostStrikeImmunity			= 0.f;
	m_fBoostFireWoundImmunity		= 0.f;
	m_fBoostWoundImmunity			= 0.f;
	m_fBoostRadiationProtection		= 0.f;
	m_fBoostTelepaticProtection		= 0.f;
	m_fBoostChemicalBurnProtection	= 0.f;

	m_fDeltaHealth			= 0;
	m_fDeltaPower			= 0;
	m_fDeltaRadiation		= 0;
	m_fDeltaPsyHealth		= 0;

	m_fHealthLost			= 0.f;
	m_pWho					= nullptr;
	m_iWhoID				= 0;

	m_WoundVector.clear		();

	m_fKillHitTreshold		= 0;
	m_fLastChanceHealth		= 0;
	m_fInvulnerableTime		= 0;
	m_fInvulnerableTimeDelta= 0;

	m_fHitBoneScale			= 1.f;
	m_fWoundBoneScale		= 1.f;

	m_bIsBleeding			= false;
	m_bCanBeHarmed			= true;

	static bool wound_params_initialized = false;

	if (!wound_params_initialized)
	{
		m_GlobalWoundsFactorsForHitTypes.resize(ALife::eHitTypeMax);

		for (int i = 0; i < ALife::eHitTypeMax; i++)
		{
			m_GlobalWoundsFactorsForHitTypes[i] = pSettings->read_if_exists<float>("gunslinger_wound_factors", shared_str().printf("wound_factor_for_hit_type_%u", i).c_str(), 1.0f);
		}

		m_GlobalBleedingsFactorsForHitTypes.resize(ALife::eHitTypeMax);

		for (int i = 0; i < ALife::eHitTypeMax; i++)
		{
			m_GlobalBleedingsFactorsForHitTypes[i] = pSettings->read_if_exists<float>("gunslinger_wound_factors", shared_str().printf("bleeding_factor_for_hit_type_%u", i).c_str(), 1.0f);
		}
	}

	wound_params_initialized = true;
}

CEntityCondition::~CEntityCondition(void)
{
	ClearWounds				();
}

void CEntityCondition::ClearWounds()
{
	for(WOUND_VECTOR_IT it = m_WoundVector.begin(); m_WoundVector.end() != it; ++it)
		xr_delete(*it);
	m_WoundVector.clear();

	m_bIsBleeding = false;
}

void CEntityCondition::LoadCondition(const char* entity_section)
{
	const char*				section = pSettings->read_if_exists<LPCSTR>(entity_section,"condition_sect",entity_section);

	m_change_v.load		(section,"");

	m_fMinWoundSize			= pSettings->r_float(section,"min_wound_size");
	m_fHealthHitPart		= pSettings->r_float(section,"health_hit_part");
	m_fPowerHitPart			= pSettings->r_float(section,"power_hit_part");

	m_use_limping_state		= pSettings->read_if_exists<bool>(section,"use_limping_state",false);
	m_limping_threshold		= pSettings->read_if_exists<float>(section,"limping_threshold",.5f);

	m_fKillHitTreshold		= pSettings->read_if_exists<float>(section,"killing_hit_treshold",0.0f);
	m_fLastChanceHealth		= pSettings->read_if_exists<float>(section,"last_chance_health",0.0f);
	m_fInvulnerableTimeDelta= pSettings->read_if_exists<float>(section,"invulnerable_time",0.0f)/1000.f;

	m_WoundsFactorsForHitTypes.resize(ALife::eHitTypeMax);

	for (int i = 0; i < ALife::eHitTypeMax; i++)
	{
		m_WoundsFactorsForHitTypes[i] = pSettings->read_if_exists<float>(section, shared_str().printf("wound_factor_for_hit_type_%u", i).c_str(), 1.0f);
	}

	m_BleedingsFactorsForHitTypes.resize(ALife::eHitTypeMax);

	for (int i = 0; i < ALife::eHitTypeMax; i++)
	{
		m_BleedingsFactorsForHitTypes[i] = pSettings->read_if_exists<float>(section, shared_str().printf("bleeding_factor_for_hit_type_%u", i).c_str(), 1.0f);
	}
}

void CEntityCondition::LoadTwoHitsDeathParams(const char* section)
{
	m_fKillHitTreshold		= pSettings->read_if_exists<float>(section,"killing_hit_treshold",0.0f);
	m_fLastChanceHealth		= pSettings->read_if_exists<float>(section,"last_chance_health",0.0f);
	m_fInvulnerableTimeDelta= pSettings->read_if_exists<float>(section,"invulnerable_time",0.0f)/1000.f;
}

void CEntityCondition::reinit	()
{
	m_iLastTimeCalled		= 0;
	m_bTimeValid			= false;

	max_health()			= MAX_HEALTH;
	m_fPowerMax				= MAX_POWER;
	m_fRadiationMax			= MAX_RADIATION;
	m_fPsyHealthMax			= MAX_PSY_HEALTH;

	m_fEntityMorale			=  m_fEntityMoraleMax = 1.f;

	SetHealth				( MAX_HEALTH );
	m_fPower				= MAX_POWER;
	m_fRadiation			= 0;
	m_fPsyHealth			= MAX_PSY_HEALTH;

	m_fDeltaHealth			= 0;
	m_fDeltaPower			= 0;
	m_fDeltaRadiation		= 0;

	m_fDeltaCircumspection	= 0;
	m_fDeltaEntityMorale	= 0;
	m_fDeltaPsyHealth		= 0;

	m_fHealthLost			= 0.f;
	m_pWho					= nullptr;
	m_iWhoID				= 0;

	ClearWounds				();

}

void CEntityCondition::ChangeHealth(const float value)
{
	VERIFY(_valid(value));	
	m_fDeltaHealth += (CanBeHarmed() || (value > 0)) ? value : 0;
}

void CEntityCondition::ChangePower(const float value)
{
	m_fDeltaPower += value;
}

void CEntityCondition::ChangeRadiation(const float value)
{
	m_fDeltaRadiation += value;
}

void CEntityCondition::ChangePsyHealth(const float value)
{
	m_fDeltaPsyHealth += value;
}

void CEntityCondition::ChangeCircumspection(const float value)
{
	m_fDeltaCircumspection += value;
}
void CEntityCondition::ChangeEntityMorale(const float value)
{
	m_fDeltaEntityMorale += value;
}

void CEntityCondition::ChangeBleeding(const float percent)
{
	if (m_object->cast_actor() != nullptr)
	{
		int mask = ~static_cast<int>(1 << ALife::EHitType::eHitTypeBurn);
		ChangeBleedingCustom(percent, mask);
	}
	else
	{
		ChangeBleedingCustom(percent);
	}
}

void CEntityCondition::ChangeWoundsByType(const float percent, ALife::EHitType type)
{
	//затянуть раны
	for (WOUND_VECTOR_IT it = m_WoundVector.begin(); m_WoundVector.end() != it; ++it)
	{
		CWound* wound = *it;
		if (fis_zero(wound->TypeSize(type)))
			continue;

		(*it)->Incarnation(percent, m_fMinWoundSize);
		if (0 == (*it)->TotalSize())
			(*it)->SetDestroy(true);
	}
}

bool RemoveWoundPred(CWound* pWound)
{
	if(pWound->GetDestroy())
	{
		xr_delete		(pWound);
		return			true;
	}
	return				false;
}

void  CEntityCondition::UpdateWounds		()
{
	//убрать все зашившие раны из списка
	m_WoundVector.erase(
		std::remove_if(
			m_WoundVector.begin(),
			m_WoundVector.end(),
			&RemoveWoundPred
		),
		m_WoundVector.end()
	);
}

void CEntityCondition::UpdateConditionTime()
{
	u64 _cur_time = (IsGameTypeSingle()) ? Level().GetGameTime() : Level().timeServer();
	
	if(m_bTimeValid)
	{
		if (_cur_time > m_iLastTimeCalled){
			float x					= float(_cur_time-m_iLastTimeCalled)/1000.0f;
			SetConditionDeltaTime	(x);

		}else 
			SetConditionDeltaTime(0.0f);
	}
	else
	{
		SetConditionDeltaTime	(0.0f);
		m_bTimeValid			= true;

		m_fDeltaHealth			= 0;
		m_fDeltaPower			= 0;
		m_fDeltaRadiation		= 0;
		m_fDeltaCircumspection	= 0;
		m_fDeltaEntityMorale	= 0;
	}

	m_iLastTimeCalled			= _cur_time;
}

//вычисление параметров с ходом игрового времени
void CEntityCondition::UpdateCondition()
{
	if(GetHealth()<=0)			return;
	//-----------------------------------------
	bool CriticalHealth			= false;

	if (m_fDeltaHealth+GetHealth() <= 0)
	{
		CriticalHealth			= true;
		m_object->OnCriticalHitHealthLoss();
	}
	else
	{
		if (m_fDeltaHealth<0) m_object->OnHitHealthLoss(GetHealth()+m_fDeltaHealth);
	}
	//-----------------------------------------
	UpdateHealth				();
	//-----------------------------------------
	if (!CriticalHealth && m_fDeltaHealth+GetHealth() <= 0)
	{
		CriticalHealth			= true;
		m_object->OnCriticalWoundHealthLoss();
	};
	//-----------------------------------------
	UpdatePower					();
	UpdateRadiation				();
	//-----------------------------------------
	if (!CriticalHealth && m_fDeltaHealth+GetHealth() <= 0)
	{
		CriticalHealth = true;
		m_object->OnCriticalRadiationHealthLoss();
	};
	//-----------------------------------------
	UpdatePsyHealth				();

	UpdateEntityMorale			();

	if(Device.fTimeGlobal>m_fInvulnerableTime)
	{
		float curr_health			= GetHealth();
		if(curr_health>m_fKillHitTreshold && curr_health+m_fDeltaHealth<0)
		{
			SetHealth(m_fLastChanceHealth);
			m_fInvulnerableTime = Device.fTimeGlobal + m_fInvulnerableTimeDelta;
		}
		else
			SetHealth				( curr_health + m_fDeltaHealth );
	}

	m_fPower					+= m_fDeltaPower;
	m_fPsyHealth				+= m_fDeltaPsyHealth;
	m_fEntityMorale				+= m_fDeltaEntityMorale;
	m_fRadiation				+= m_fDeltaRadiation;
	
	m_fDeltaHealth				= 0;
	m_fDeltaPower				= 0;
	m_fDeltaRadiation			= 0;
	m_fDeltaPsyHealth			= 0;
	m_fDeltaCircumspection		= 0;
	m_fDeltaEntityMorale		= 0;
	float	l_health			= GetHealth() ;
	clamp						(l_health,			MIN_HEALTH, max_health());
	SetHealth					(l_health);
	clamp						(m_fPower,			0.0f,		m_fPowerMax);
	clamp						(m_fRadiation,		0.0f,		m_fRadiationMax);
	clamp						(m_fEntityMorale,	0.0f,		m_fEntityMoraleMax);
	clamp						(m_fPsyHealth,		0.0f,		m_fPsyHealthMax);
}

float CEntityCondition::HitOutfitEffect(float hit_power, ALife::EHitType hit_type, u16 element, float ap, bool& add_wound)
{
	CInventoryOwner* pInvOwner = m_object->cast_inventory_owner();
	if (pInvOwner == nullptr)
	{
		return hit_power;
	}

	CCustomOutfit* pOutfit = pInvOwner->GetOutfit();
	CHelmet* pHelmet = pInvOwner->GetHelmet();
	if (pOutfit == nullptr && pHelmet == nullptr)
	{
		return hit_power;
	}

	float new_hit_power = hit_power;
	if (pOutfit)
	{
		new_hit_power = pOutfit->HitThroughArmor(hit_power, element, ap, add_wound, hit_type);
	}

	if (pHelmet)
	{
		new_hit_power = pHelmet->HitThroughArmor(new_hit_power, element, ap, add_wound, hit_type);
	}

	if (bDebug)
	{
		Msg("new_hit_power = %.3f  hit_type = %s  ap = %.3f", new_hit_power, ALife::g_cafHitType2String(hit_type), ap);
	}

	return new_hit_power;
}

float CEntityCondition::HitPowerEffect(float power_loss)
{
	CInventoryOwner* pInvOwner = m_object->cast_inventory_owner();
	if (pInvOwner == nullptr)
	{
		return power_loss;
	}

	CCustomOutfit* pOutfit = pInvOwner->GetOutfit();
	if (pOutfit == nullptr)
	{
		return power_loss * 0.5f;
	}

    float new_power_loss = power_loss * pOutfit->GetPowerLoss();

	return new_power_loss;
}

CWound* CEntityCondition::AddWound(float hit_power, ALife::EHitType hit_type, u16 element)
{
	//максимальное число косточек 64
	VERIFY(element < 64 || BI_NONE == element);

	//запомнить кость по которой ударили и силу удара
	WOUND_VECTOR_IT it = m_WoundVector.begin();
	for (; it != m_WoundVector.end(); it++)
	{
		if ((*it)->GetBoneNum() == element)
		{
			break;
		}
	}

	CWound* pWound = nullptr;
	
	hit_power *= m_GlobalWoundsFactorsForHitTypes[hit_type] * m_WoundsFactorsForHitTypes[hit_type];

	//новая рана
	if (it == m_WoundVector.end())
	{
		pWound = new CWound(element);
		pWound->AddHit(hit_power * ::Random.randF(0.5f, 1.5f), hit_type);
		m_WoundVector.push_back(pWound);
	}
	//старая 
	else
	{
		pWound = *it;
		pWound->AddHit(hit_power * ::Random.randF(0.5f, 1.5f), hit_type);
	}

	VERIFY(pWound);
	return pWound;
}

float CEntityCondition::CorrectBleedingForHitType(ALife::EHitType hit_type, float bleeding)
{
	return bleeding * m_GlobalBleedingsFactorsForHitTypes[hit_type] * m_BleedingsFactorsForHitTypes[hit_type];
}

float CEntityCondition::GetWoundComponentByHitType(CWound* wound, ALife::EHitType hit_type)
{
	if (hit_type >= ALife::EHitType::eHitTypeMax)
	{
		return 0.0f;
	}

	return wound->TypeSize(hit_type);
}

void CEntityCondition::SetWoundComponentByHitType(CWound* wound, float value, ALife::EHitType hit_type)
{
	wound->SetHit(value, hit_type);
}

float CEntityCondition::CalcModifiedWoundTotalSize(CWound* wound, int hit_type_mask)
{
	float result = 0.0f;

	for (int i = 0; i < ALife::EHitType::eHitTypeMax; ++i)
	{
		if (hit_type_mask > 0 && ((1 << i) & hit_type_mask) == 0)
		{
			continue;
		}

		float bleeding = GetWoundComponentByHitType(wound, (ALife::EHitType)i);

		if (bleeding > 0.0f)
		{
			result += CorrectBleedingForHitType((ALife::EHitType)i, bleeding);
		}
	}

	return result;
}

bool CEntityCondition::ChangeBleedingForWound(CWound* wound, float percent, float min_wound_size, int hit_type_mask)
{
	for (int i = 0; i < ALife::EHitType::eHitTypeMax; ++i)
	{
		if (hit_type_mask > 0 && ((1 << i) & hit_type_mask) == 0)
		{
			continue;
		}

		float wound_size = GetWoundComponentByHitType(wound, (ALife::EHitType)i);
		wound_size -= percent;
		if (wound_size < min_wound_size)
		{
			wound_size = 0.0f;
		}

		SetWoundComponentByHitType(wound, wound_size, (ALife::EHitType)i);
	}

	bool result = true;
	for (int i = 0; i < ALife::EHitType::eHitTypeMax; ++i)
	{
		if (GetWoundComponentByHitType(wound, (ALife::EHitType)i) > EPS_S)
		{
			result = false;
			break;
		}
	}

	if (result)
	{
		for (int i = 0; i < ALife::EHitType::eHitTypeMax; ++i)
		{
			SetWoundComponentByHitType(wound, 0.0f, (ALife::EHitType)i);
		}
	}

	return result;
}

void CEntityCondition::ChangeBleedingCustom(float percent, int hit_type_mask)
{
	for (auto& wound : wounds())
	{
		if (ChangeBleedingForWound(wound, percent, m_fMinWoundSize, hit_type_mask))
		{
			wound->SetDestroy(true);
		}
	}
}

CWound* CEntityCondition::ConditionHit(SHit* pHDS)
{
	//кто нанес последний хит
	m_pWho = pHDS->who;
	m_iWhoID = (nullptr != pHDS->who) ? pHDS->who->ID() : 0;

	bool const is_special_hit_2_self		=	(pHDS->who == m_object) && (pHDS->boneID == BI_NONE);

	bool bAddWound = pHDS->add_wound;
	
	float hit_power_org = pHDS->damage();
	float hit_power = hit_power_org;
	hit_power = HitOutfitEffect( hit_power_org, pHDS->hit_type, pHDS->boneID, pHDS->armor_piercing, bAddWound );

	CActor* pActor = m_object->cast_actor();

	switch(pHDS->hit_type)
	{
	case ALife::eHitTypeTelepatic:
		hit_power -= m_fBoostTelepaticProtection;
		if(hit_power < 0.f)
			hit_power = 0.f;
		hit_power *= GetHitImmunity(pHDS->hit_type)-m_fBoostTelepaticImmunity;
		ChangePsyHealth(-hit_power);
		m_fHealthLost = hit_power*m_fHealthHitPart;
		m_fDeltaHealth -= CanBeHarmed() ? m_fHealthLost : 0;
		m_fDeltaPower -= hit_power*m_fPowerHitPart;
		bAddWound		=  false;
		break;
	case ALife::eHitTypeLightBurn:
	case ALife::eHitTypeBurn:
		hit_power *= GetHitImmunity(ALife::eHitTypeBurn)-m_fBoostBurnImmunity;
		m_fHealthLost = hit_power*m_fHealthHitPart*m_fHitBoneScale;
		m_fDeltaHealth -= CanBeHarmed() ? m_fHealthLost : 0;
		m_fDeltaPower -= hit_power*m_fPowerHitPart;
		bAddWound = pActor && pActor->HudAnimator()->BurnAnimator();
		break;
	case ALife::eHitTypeChemicalBurn:
		hit_power -= m_fBoostChemicalBurnProtection;
		if(hit_power < 0.f)
			hit_power = 0.f;
		hit_power *= GetHitImmunity(pHDS->hit_type)-m_fBoostChemicalBurnImmunity;
		m_fHealthLost = hit_power*m_fHealthHitPart;
		m_fDeltaHealth -= CanBeHarmed() ? m_fHealthLost : 0;
		m_fDeltaPower -= hit_power*m_fPowerHitPart;
		bAddWound		=  false;
		break;
	case ALife::eHitTypeShock:
		hit_power		*= GetHitImmunity(pHDS->hit_type)-m_fBoostShockImmunity;
		m_fHealthLost	=  hit_power*m_fHealthHitPart;
		m_fDeltaHealth -= CanBeHarmed() ? m_fHealthLost : 0;
		m_fDeltaPower	-= hit_power*m_fPowerHitPart;
		bAddWound		=  false;
		break;
	case ALife::eHitTypeRadiation:
		hit_power			-= m_fBoostRadiationProtection;
		if(hit_power < 0.f)
			hit_power = 0.f;
		hit_power			*= GetHitImmunity(pHDS->hit_type)-m_fBoostRadiationImmunity;
		m_fDeltaRadiation	+= hit_power;
		bAddWound			=  false;
		return nullptr;
		break;
	case ALife::eHitTypeExplosion:
		hit_power *= GetHitImmunity(pHDS->hit_type)-m_fBoostExplImmunity;
		m_fHealthLost = hit_power*m_fHealthHitPart;
		m_fDeltaHealth -= CanBeHarmed() ? m_fHealthLost : 0;
		m_fDeltaPower -= hit_power*m_fPowerHitPart;
		break;
	case ALife::eHitTypeStrike:
	case ALife::eHitTypePhysicStrike:
		hit_power *= GetHitImmunity(pHDS->hit_type)-m_fBoostStrikeImmunity;
		m_fHealthLost = hit_power*m_fHealthHitPart;
		m_fDeltaHealth -= CanBeHarmed() ? m_fHealthLost : 0;
		m_fDeltaPower -= hit_power*m_fPowerHitPart;
		bAddWound		=  false;
		break;
	case ALife::eHitTypeFireWound:
		hit_power *= GetHitImmunity(pHDS->hit_type)-m_fBoostFireWoundImmunity;
		m_fHealthLost = hit_power*m_fHealthHitPart*m_fHitBoneScale;
		m_fDeltaHealth -= CanBeHarmed() ? m_fHealthLost : 0;
		m_fDeltaPower -= hit_power*m_fPowerHitPart;
		break;
	case ALife::eHitTypeWound:
		hit_power *= GetHitImmunity(pHDS->hit_type)-m_fBoostWoundImmunity;
		m_fHealthLost = hit_power*m_fHealthHitPart*m_fHitBoneScale;
		m_fDeltaHealth -= CanBeHarmed() ? m_fHealthLost : 0;
		m_fDeltaPower -= hit_power*m_fPowerHitPart;
		break;
	default:
		{
			R_ASSERT2(0,"unknown hit type");
		}break;
	}

	if (bDebug && !is_special_hit_2_self ) 
	{
		Msg("%s hitted in %s with %f[%f]", m_object->Name(), PKinematics(m_object->Visual())->LL_BoneName_dbg(pHDS->boneID), m_fHealthLost*100.0f, hit_power_org);
	}
	//раны добавляются только живому
	if( bAddWound && GetHealth()>0 )
	{
		return AddWound(hit_power*m_fWoundBoneScale, pHDS->hit_type, pHDS->boneID);
	}else{
		return nullptr;
	}
}


float CEntityCondition::BleedingSpeed(int hit_type_mask)
{
	float result = 0.0f;

	for (auto& wound : wounds())
	{
		result += CalcModifiedWoundTotalSize(wound, hit_type_mask);
	}

	return result;
}

void CEntityCondition::UpdateHealth()
{
	float bleeding_speed = BleedingSpeed() * m_fDeltaTime * m_change_v.m_fV_Bleeding;
	m_bIsBleeding = fis_zero(bleeding_speed) ? false : true;
	m_fDeltaHealth -= CanBeHarmed() ? bleeding_speed : 0;
	m_fDeltaHealth += m_fDeltaTime * (m_change_v.m_fV_HealthRestore + m_fBoostHpRestore);

	VERIFY(_valid(m_fDeltaHealth));
	ChangeBleeding((m_change_v.m_fV_WoundIncarnation + m_fBoostBleedingRestore) * m_fDeltaTime);
}

void CEntityCondition::UpdatePower()
{}

void CEntityCondition::UpdatePsyHealth()
{
	m_fDeltaPsyHealth += m_change_v.m_fV_PsyHealth * m_fDeltaTime;
}

void CEntityCondition::UpdateRadiation()
{
	if (m_fRadiation > 0)
	{
		m_fDeltaRadiation -= (m_change_v.m_fV_Radiation + m_fBoostRadiationRestore) * m_fDeltaTime;

		m_fDeltaHealth -= CanBeHarmed() ? m_change_v.m_fV_RadiationHealth * m_fRadiation * m_fDeltaTime : 0.0f;
	}
}

void CEntityCondition::UpdateEntityMorale()
{
	if (m_fEntityMorale < m_fEntityMoraleMax)
	{
		m_fDeltaEntityMorale += m_change_v.m_fV_EntityMorale * m_fDeltaTime;
	}
}

bool CEntityCondition::IsLimping() const
{
	if (!m_use_limping_state)
	{
		return false;
	}

	return !!(m_fPower * GetHealth() <= m_limping_threshold);
}

void CEntityCondition::save	(NET_Packet &output_packet)
{
	u8 is_alive	= (GetHealth()>0.f)?1:0;
	
	output_packet.w_u8	(is_alive);
	if(is_alive)
	{
		save_data						(m_fPower,output_packet);
		save_data						(m_fRadiation,output_packet);
		save_data						(m_fEntityMorale,output_packet);
		save_data						(m_fPsyHealth,output_packet);

		output_packet.w_u8				((u8)m_WoundVector.size());
		for(WOUND_VECTOR_IT it = m_WoundVector.begin(); m_WoundVector.end() != it; it++)
			(*it)->save(output_packet);
	}
}

void CEntityCondition::load	(IReader &input_packet)
{
	m_bTimeValid = false;

	u8 is_alive				= input_packet.r_u8	();
	if(is_alive)
	{
		load_data						(m_fPower,input_packet);
		load_data						(m_fRadiation,input_packet);
		load_data						(m_fEntityMorale,input_packet);
		load_data						(m_fPsyHealth,input_packet);

		ClearWounds();
		m_WoundVector.resize(input_packet.r_u8());
		if(!m_WoundVector.empty())
			for(u32 i=0; i<m_WoundVector.size(); i++)
			{
				CWound* pWound = new CWound(BI_NONE);
				pWound->load(input_packet);
				m_WoundVector[i] = pWound;
			}
	}
}

void CEntityCondition::Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object,"CEntityCondition")
	{
		if (!Object.IsSave()) {
			m_bTimeValid = false;
		}

		bool is_alive = GetHealth() > 0.f;
		Object << is_alive;
		if (is_alive)
		{
			Object << m_fPower << m_fRadiation << m_fEntityMorale << m_fPsyHealth;
			if (!Object.IsSave()) {
				ClearWounds();
			}
			Object << m_WoundVector;
		}
	}
}

void CEntityCondition::SConditionChangeV::load(LPCSTR sect, LPCSTR prefix)
{
	string256				str;
	m_fV_Circumspection		= 0.01f;

	xr_strconcat(str,"radiation_v",prefix);
	m_fV_Radiation			= pSettings->r_float(sect,str);
	xr_strconcat(str,"radiation_health_v",prefix);
	m_fV_RadiationHealth	= pSettings->r_float(sect,str);
	xr_strconcat(str,"morale_v",prefix);
	m_fV_EntityMorale		= pSettings->r_float(sect,str);
	xr_strconcat(str,"psy_health_v",prefix);
	m_fV_PsyHealth			= pSettings->r_float(sect,str);	
	xr_strconcat(str,"bleeding_v",prefix);
	m_fV_Bleeding			= pSettings->r_float(sect,str);
	xr_strconcat(str,"wound_incarnation_v",prefix);
	m_fV_WoundIncarnation	= pSettings->r_float(sect,str);
	xr_strconcat(str,"health_restore_v",prefix);
	m_fV_HealthRestore		= pSettings->read_if_exists<float>(sect, str,0.0f);
}

void CEntityCondition::remove_links	(const CObject *object)
{
	if (m_pWho != object)
		return;

	m_pWho					= m_object;
	m_iWhoID				= m_object->ID();
}

bool CEntityCondition::ApplyInfluence(const SMedicineInfluenceValues& V, const shared_str& sect, bool use_sound)
{
	ChangeHealth	(V.fHealth);
	ChangePower		(V.fPower);
	ChangeSatiety	(V.fSatiety);
	ChangeThirst	(V.fThirst);
	ChangeRadiation	(V.fRadiation);
	ChangeBleeding	(V.fWoundsHeal);
	SetMaxPower		(GetMaxPower()+V.fMaxPowerUp);
	ChangeAlcohol	(V.fAlcohol);
	ChangeIntoxication(V.fIntoxication);
	return true;
}

bool CEntityCondition::ApplyBooster(const SBooster& B, const shared_str& sect, bool use_sound)
{
	return true;
}

void SMedicineInfluenceValues::Load(const shared_str& sect)
{
	fHealth			= pSettings->r_float(sect.c_str(), "eat_health");
	fPower			= pSettings->r_float(sect.c_str(), "eat_power");
	fSatiety		= pSettings->r_float(sect.c_str(), "eat_satiety");
	fThirst			= pSettings->read_if_exists<float>(sect.c_str(), "eat_thirst", 0);
	fSleepiness		= pSettings->read_if_exists<float>(sect.c_str(), "eat_sleepiness", 0);
	fRadiation		= pSettings->r_float(sect.c_str(), "eat_radiation");
	fWoundsHeal		= pSettings->r_float(sect.c_str(), "wounds_heal_perc");
	clamp			(fWoundsHeal, 0.f, 1.f);
	fMaxPowerUp		= pSettings->read_if_exists<float>(sect.c_str(),	"eat_max_power",0.0f);
	fAlcohol		= pSettings->read_if_exists<float>(sect.c_str(),	"eat_alcohol", 0.0f);
	fIntoxication	= pSettings->read_if_exists<float>(sect.c_str(),	"eat_intoxication", 0.0f);
	fTimeTotal		= pSettings->read_if_exists<float>(sect.c_str(),	"apply_time_sec", -1.0f);
}

void SBooster::Load(const shared_str& sect, EBoostParams type)
{
	fBoostTime = pSettings->r_float(sect.c_str(), "boost_time");
	fBoostMaxTime = fBoostTime;
	m_type = type;
	switch(type)
	{
		case eBoostHpRestore: fBoostValue = pSettings->r_float(sect.c_str(), "boost_health_restore"); break;
		case eBoostPowerRestore: fBoostValue = pSettings->r_float(sect.c_str(), "boost_power_restore"); break;
		case eBoostRadiationRestore: fBoostValue = pSettings->r_float(sect.c_str(), "boost_radiation_restore"); break;
		case eBoostBleedingRestore: fBoostValue = pSettings->r_float(sect.c_str(), "boost_bleeding_restore"); break;
		case eBoostMaxWeight: fBoostValue = pSettings->r_float(sect.c_str(), "boost_max_weight"); break;
		case eBoostBurnImmunity: fBoostValue = pSettings->r_float(sect.c_str(), "boost_burn_immunity"); break;
		case eBoostShockImmunity: fBoostValue = pSettings->r_float(sect.c_str(), "boost_shock_immunity"); break;
		case eBoostRadiationImmunity: fBoostValue = pSettings->r_float(sect.c_str(), "boost_radiation_immunity"); break;
		case eBoostTelepaticImmunity: fBoostValue = pSettings->r_float(sect.c_str(), "boost_telepat_immunity"); break;
		case eBoostChemicalBurnImmunity: fBoostValue = pSettings->r_float(sect.c_str(), "boost_chemburn_immunity"); break;
		case eBoostExplImmunity: fBoostValue = pSettings->r_float(sect.c_str(), "boost_explosion_immunity"); break;
		case eBoostStrikeImmunity: fBoostValue = pSettings->r_float(sect.c_str(), "boost_strike_immunity"); break;
		case eBoostFireWoundImmunity: fBoostValue = pSettings->r_float(sect.c_str(), "boost_fire_wound_immunity"); break;
		case eBoostWoundImmunity: fBoostValue = pSettings->r_float(sect.c_str(), "boost_wound_immunity"); break;
		case eBoostRadiationProtection: fBoostValue = pSettings->r_float(sect.c_str(), "boost_radiation_protection"); break;
		case eBoostTelepaticProtection: fBoostValue = pSettings->r_float(sect.c_str(), "boost_telepat_protection"); break;
		case eBoostChemicalBurnProtection: fBoostValue = pSettings->r_float(sect.c_str(), "boost_chemburn_protection"); break;
		default: NODEFAULT;	
	}
}

