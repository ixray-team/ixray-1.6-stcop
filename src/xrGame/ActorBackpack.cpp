#include "StdAfx.h"
#include "ActorBackpack.h"
#include "Actor.h"
#include "Inventory.h"
#include "../../xrSound/ai_sounds.h"

static LPCSTR backpack_on = "interface\\inv_backpack_on";
static LPCSTR backpack_off = "interface\\inv_backpack_off";

CBackpack::CBackpack()
{
    m_flags.set(FUsingCondition, FALSE);

    //TODO: Make optional dress sounds
    //m_dress_snd[0].create(backpack_on, st_Effect, SOUND_TYPE_IDLE);
    //m_dress_snd[1].create(backpack_off, st_Effect, SOUND_TYPE_IDLE);
}

void CBackpack::Load(LPCSTR section)
{
    inherited::Load(section);

    m_additional_weight = READ_IF_EXISTS(pSettings, r_float, section, "additional_inventory_weight", 0.0f);
    m_additional_weight2 = READ_IF_EXISTS(pSettings, r_float, section, "additional_inventory_weight2", 0.0f);
    m_fPowerRestoreSpeed = READ_IF_EXISTS(pSettings, r_float, section, "power_restore_speed", 0.0f);

    m_flags.set(FUsingCondition, READ_IF_EXISTS(pSettings, r_bool, section, "use_condition", FALSE));
}

void CBackpack::OnMoveToSlot(const SInvItemPlace& previous_place)
{
    inherited::OnMoveToSlot(previous_place);

    if (previous_place.type == eItemPlaceRuck)
    {
        //m_dress_snd[0].destroy();
        //m_dress_snd[0].play(this, sm_2D);
    }
}

void CBackpack::OnMoveToRuck(const SInvItemPlace& previous_place)
{
    inherited::OnMoveToRuck(previous_place);

    if (previous_place.type == eItemPlaceSlot)
    {
        //m_dress_snd[1].destroy();
        //m_dress_snd[1].play(this, sm_2D);
    }
}

bool CBackpack::install_upgrade_impl(LPCSTR section, bool test)
{
    bool result = inherited::install_upgrade_impl(section, test);

    result |= process_if_exists(section, "additional_inventory_weight", &CInifile::r_float, m_additional_weight, test);
    result |= process_if_exists(section, "additional_inventory_weight2", &CInifile::r_float, m_additional_weight2, test);
    result |= process_if_exists(section, "power_restore_speed", &CInifile::r_float, m_fPowerRestoreSpeed, test);

    return result;
}