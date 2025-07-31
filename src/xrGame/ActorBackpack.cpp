#include "stdafx.h"
#include "ActorBackpack.h"
#include "Actor.h"
#include "Inventory.h"
#include "../../xrSound/ai_sounds.h"

static LPCSTR backpack_on = "interface\\inv_backpack_on";
static LPCSTR backpack_off = "interface\\inv_backpack_off";

CBackpack::CBackpack()
{
    m_flags.set(FUsingCondition, FALSE);
    m_dress_snd[0].create(backpack_on, st_Effect, SOUND_TYPE_IDLE);
    m_dress_snd[1].create(backpack_off, st_Effect, SOUND_TYPE_IDLE);
}

void CBackpack::Load(LPCSTR section)
{
    inherited::Load(section);

    m_additional_weight = pSettings->r_float(section, "additional_inventory_weight");
    m_additional_weight2 = pSettings->r_float(section, "additional_inventory_weight2");
    m_fPowerRestoreSpeed = READ_IF_EXISTS(pSettings, r_float, section, "power_restore_speed", 0.0f);
    m_fPowerLoss = READ_IF_EXISTS(pSettings, r_float, section, "power_loss", 1.0f);
    clamp(m_fPowerLoss, EPS, 1.0f);

    m_fJumpSpeed = READ_IF_EXISTS(pSettings, r_float, section, "jump_speed", 1.f);
    m_fWalkAccel = READ_IF_EXISTS(pSettings, r_float, section, "walk_accel", 1.f);
    m_fOverweightWalkK = READ_IF_EXISTS(pSettings, r_float, section, "overweight_walk_accel", 1.f);

    hit_add_weight = READ_IF_EXISTS(pSettings, r_bool, section, "hit_add_weight", false);

    m_flags.set(FUsingCondition, READ_IF_EXISTS(pSettings, r_bool, section, "use_condition", TRUE));
}


void CBackpack::OnMoveToSlot(const SInvItemPlace& previous_place)
{
    inherited::OnMoveToSlot(previous_place);

    if (previous_place.type == eItemPlaceRuck)
    {
        //m_dress_snd[0].destroy();
        m_dress_snd[0].play(this, sm_2D);
    }
}

void CBackpack::OnMoveToRuck(const SInvItemPlace& previous_place)
{
    inherited::OnMoveToRuck(previous_place);

    if (previous_place.type == eItemPlaceSlot)
    {
        //m_dress_snd[1].destroy();
        m_dress_snd[1].play(this, sm_2D);
    }
}

void CBackpack::Hit(float hit_power, ALife::EHitType hit_type)
{
    if (IsUsingCondition() == false)
        return;
    hit_power *= GetHitImmunity(hit_type);
    ChangeCondition(-hit_power);
}

bool CBackpack::install_upgrade_impl(LPCSTR section, bool test)
{
    bool result = inherited::install_upgrade_impl(section, test);

    result |= process_if_exists(section, "power_restore_speed", &CInifile::r_float, m_fPowerRestoreSpeed, test);
    result |= process_if_exists(section, "power_loss", &CInifile::r_float, m_fPowerLoss, test);
    clamp(m_fPowerLoss, 0.0f, 1.0f);

    result |= process_if_exists(section, "additional_inventory_weight", &CInifile::r_float, m_additional_weight, test);
    result |=
        process_if_exists(section, "additional_inventory_weight2", &CInifile::r_float, m_additional_weight2, test);

    return result;
}

float CBackpack::GetDefHitTypeProtection(ALife::EHitType hit_type) const
{
    return GetHitImmunity(hit_type) * GetCondition();
}

float CBackpack::HitThroughArmor(float hit_power, s16 element, float ap, bool& add_wound, ALife::EHitType hit_type) const
{
    float NewHitPower = hit_power;

    float one = 0.1f;
    if (hit_type == ALife::eHitTypeStrike || hit_type == ALife::eHitTypeWound ||
        hit_type == ALife::eHitTypeWound_2 || hit_type == ALife::eHitTypeExplosion)
    {
        one = 1.0f;
    }
    float protect = GetDefHitTypeProtection(hit_type);
    NewHitPower -= protect * one;

    if (NewHitPower < 0.f)
        NewHitPower = 0.f;

    return NewHitPower;
}
