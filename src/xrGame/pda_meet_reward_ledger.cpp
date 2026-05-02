#include "StdAfx.h"

#include "pda_meet_reward_ledger.h"

#include "InventoryOwner.h"
#include "pda_communication.h"

CPdaMeetRewardLedger& CPdaMeetRewardLedger::Get()
{
    static CPdaMeetRewardLedger ledger;
    return ledger;
}

bool CPdaMeetRewardLedger::HasPendingFor(const CInventoryOwner* npc) const
{
    if (!PdaCommunication().IsEnabled() || npc == nullptr)
    {
        return false;
    }

    return PendingCountFor(npc) > 0;
}

u32 CPdaMeetRewardLedger::PendingCountFor(const CInventoryOwner* /*npc*/) const
{
    if (!PdaCommunication().IsEnabled())
    {
        return 0;
    }

    return static_cast<u32>(_entries.size());
}
