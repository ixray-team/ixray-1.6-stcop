#include "StdAfx.h"

#include "../../xrCore/vector.h"
#include "IxAiAgent.h"
#include "IxAiBehaviourTree.h"
#include "IxAiConstants.h"
#include "IxAiStackTuning.h"

IxAiAgent::IxAiAgent()
{
    _behaviourTree = new IxAiBehaviourTree();
}

IxAiAgent::~IxAiAgent()
{
    xr_delete(_behaviourTree);
}

void IxAiAgent::SetObjectId(u16 objectId)
{
    _objectId = objectId;
}

u16 IxAiAgent::GetObjectId() const
{
    return _objectId;
}

void IxAiAgent::SetProfile(const IxAiBehaviourProfile& profile)
{
    VERIFY(profile._alertRadius > EPS_L);
    VERIFY(profile._suspicionDecayRate >= 0.f);
    VERIFY(profile._flankRange > EPS_L);
    VERIFY(profile._silencedGunHearingMultiplier >= 0.f);
    _profile = profile;

    if (_behaviourTree != nullptr)
    {
        _behaviourTree->RebuildRoot(profile);
    }
}

const IxAiBehaviourProfile& IxAiAgent::GetProfile() const
{
    return _profile;
}

IxAiAlertLevel IxAiAgent::GetAlertLevel() const
{
    return _alertLevel;
}

void IxAiAgent::SetAlertLevel(IxAiAlertLevel level)
{
    _alertLevel = level;
}

f32 IxAiAgent::GetSuspicionScore() const
{
    return _suspicionScore;
}

void IxAiAgent::AddSuspicion(f32 amount)
{
    _suspicionScore += amount;

    if (_suspicionScore > IxAiConstants::kSuspicionScoreMax)
    {
        _suspicionScore = IxAiConstants::kSuspicionScoreMax;
    }
}

void IxAiAgent::DecaySuspicion(f32 amount)
{
    _suspicionScore -= amount;

    if (_suspicionScore < 0.f)
    {
        _suspicionScore = 0.f;
    }
}

void IxAiAgent::AccumulatePerception(const xr_vector<IxAiPerceptionEvent>& events, const Fvector& selfPosition, f32 nowTime)
{
    f32 suspicionFromEvents = 0.f;
    bool hasFocus = false;
    Fvector focusPosition{};

    _memoryModel.AccumulatePerception(events, selfPosition, nowTime, _profile._alertRadius, suspicionFromEvents, hasFocus, focusPosition);

    if (suspicionFromEvents > EPS_S)
    {
        AddSuspicion(suspicionFromEvents);
    }

    if (hasFocus)
    {
        _spatialHints._lastFocusPosition = focusPosition;
        _spatialHints._hasLastFocus = true;
    }
}

void IxAiAgent::ReceiveSquadFanout(const IxAiPerceptionEvent& event, f32 nowTime)
{
    f32 suspicionBump = 0.f;
    _memoryModel.IngestSquadFanout(event, nowTime, suspicionBump);

    if (suspicionBump > EPS_S)
    {
        AddSuspicion(suspicionBump);
    }

    const bool stealthFanout =
        (event._squadFanoutFlags & (u8)IxAiSquadFanoutFlags::StealthClassified) != 0u &&
        g_ixAiRuntimeTuning.squadFanoutStealthHitHandlingEnabled;

    const bool allowDirectFocus =
        !stealthFanout || !g_ixAiRuntimeTuning.squadFanoutSuppressDirectFocusOnStealthHit;

    if (allowDirectFocus && event._intensity >= g_ixAiRuntimeTuning.squadChannelFocusIntensityMin)
    {
        _spatialHints._lastFocusPosition = event._position;
        _spatialHints._hasLastFocus = true;
    }
}

void IxAiAgent::TickMemory(f32 deltaTime, f32 nowTime)
{
    f32 suspicionLeak = 0.f;
    _memoryModel.TickBeliefLayers(deltaTime, nowTime, suspicionLeak);

    if (suspicionLeak > EPS_S)
    {
        AddSuspicion(suspicionLeak);
    }
}

void IxAiAgent::Update(f32 deltaTime)
{
    if (_behaviourTree != nullptr)
    {
        _behaviourTree->Tick(*this, deltaTime);
    }

    (void)deltaTime;
}

bool IxAiAgent::HasLastFocus() const
{
    return _spatialHints._hasLastFocus;
}

const Fvector& IxAiAgent::GetLastFocusPosition() const
{
    VERIFY(_spatialHints._hasLastFocus);
    return _spatialHints._lastFocusPosition;
}

u32 IxAiAgent::GetBridgeLastDangerPushTime() const
{
    return _pushTimestamps._bridgeDangerPush;
}

void IxAiAgent::SetBridgeLastDangerPushTime(u32 time)
{
    _pushTimestamps._bridgeDangerPush = time;
}

u32 IxAiAgent::GetBridgeLastCombatPushTime() const
{
    return _pushTimestamps._bridgeCombatPush;
}

void IxAiAgent::SetBridgeLastCombatPushTime(u32 time)
{
    _pushTimestamps._bridgeCombatPush = time;
}

u32 IxAiAgent::GetTacticHintDangerPushTime() const
{
    return _pushTimestamps._tacticHintDangerPush;
}

void IxAiAgent::SetTacticHintDangerPushTime(u32 time)
{
    _pushTimestamps._tacticHintDangerPush = time;
}

u32 IxAiAgent::GetCoverHintDangerPushTime() const
{
    return _pushTimestamps._coverHintDangerPush;
}

void IxAiAgent::SetCoverHintDangerPushTime(u32 time)
{
    _pushTimestamps._coverHintDangerPush = time;
}

u32 IxAiAgent::GetInvestigateHintPushTime() const
{
    return _pushTimestamps._investigateHintPush;
}

void IxAiAgent::SetInvestigateHintPushTime(u32 time)
{
    _pushTimestamps._investigateHintPush = time;
}

bool IxAiAgent::HasTacticalHint() const
{
    return _spatialHints._hasTacticalHint;
}

const Fvector& IxAiAgent::GetTacticalHintPosition() const
{
    VERIFY(_spatialHints._hasTacticalHint);
    return _spatialHints._tacticalHintPosition;
}

void IxAiAgent::SetTacticalHint(const Fvector& worldPosition, bool valid)
{
    _spatialHints._tacticalHintPosition = worldPosition;
    _spatialHints._hasTacticalHint = valid;
}

u32 IxAiAgent::GetMemorySlotCount() const
{
    return _memoryModel.GetSensorySlotCount();
}

const IxAiMemorySlot& IxAiAgent::GetMemorySlot(u32 index) const
{
    return _memoryModel.GetSensorySlot(index);
}

const IxAiMemoryModel& IxAiAgent::GetMemoryModel() const
{
    return _memoryModel;
}

IxAiMemoryModel& IxAiAgent::GetMemoryModel()
{
    return _memoryModel;
}
