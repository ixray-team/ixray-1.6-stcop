#pragma once

#include "../xrCore/_types.h"
#include "../xrEngine/AI/alife_space.h"

// Phase 2: pending physical rewards (save/load, issue on face-to-face talk).
// Beta: types and empty registry only; no Schedule/TryIssue yet.

struct SPdaMeetRewardEntry
{
    shared_str issuerNpcId;
    shared_str dialogId;
    shared_str phraseId;
    xr_vector<shared_str> sections;
    xr_vector<u32> counts;
    s32 money = 0;
    ALife::_TIME_ID scheduledTime = 0;
};

class CPdaMeetRewardLedger final
{
public:
    static CPdaMeetRewardLedger& Get();

    bool HasPendingFor(const CInventoryOwner* npc) const;
    u32 PendingCountFor(const CInventoryOwner* npc) const;

private:
    CPdaMeetRewardLedger() = default;

private:
    xr_vector<SPdaMeetRewardEntry> _entries;
};

IC CPdaMeetRewardLedger& PdaMeetRewardLedger() { return CPdaMeetRewardLedger::Get(); }
