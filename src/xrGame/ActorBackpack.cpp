#include "stdafx.h"
#include "ActorBackpack.h"
#include "Actor.h"
#include "Inventory.h"
//#include "../../xrSound/ai_sounds.h"

static constexpr const char* backpack_on = "interface\\inv_backpack_on";
static constexpr const char* backpack_off = "interface\\inv_backpack_off";

CBackpack::CBackpack()
{
    m_flags.set(FUsingCondition, false);

    //TODO: Make optional dress sounds
    //m_dress_snd[0].create(backpack_on, st_Effect, SOUND_TYPE_IDLE);
    //m_dress_snd[1].create(backpack_off, st_Effect, SOUND_TYPE_IDLE);
}

void CBackpack::Load(const char* section)
{
    inherited::Load(section);

    m_additional_weight = pSettings->read_if_exists<float>(section,"additional_inventory_weight",0.0f);
    m_additional_weight2 = pSettings->read_if_exists<float>(section,"additional_inventory_weight2",0.0f);
    m_fPowerRestoreSpeed = pSettings->read_if_exists<float>(section,"power_restore_speed",0.0f);

    m_flags.set(FUsingCondition, pSettings->read_if_exists<bool>(section,"use_condition",false));
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

bool CBackpack::install_upgrade_impl(const char* section, bool test)
{
    bool result = inherited::install_upgrade_impl(section, test);

    result |= process_if_exists(section, "additional_inventory_weight", m_additional_weight, test);
    result |= process_if_exists(section, "additional_inventory_weight2", m_additional_weight2, test);
    result |= process_if_exists(section, "power_restore_speed", m_fPowerRestoreSpeed, test);

    return result;
}

bool CBackpack::can_be_attached() const
{
    CObject* h_parent = const_cast<CObject*>(H_Parent());
    if (const CActor* pA = h_parent != nullptr ? h_parent->cast_actor() : nullptr)
    {
        return pA->inventory().InSlot(this);
    }

    return true;
}