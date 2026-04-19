#include "StdAfx.h"

#include "../../xrCore/vector.h"
#include "IxAiConstants.h"
#include "IxAiDecay.h"
#include "IxAiMemoryModel.h"
#include "IxAiStackTuning.h"

IxAiMemoryModel::IxAiMemoryModel() = default;

IxAiMemoryModel::~IxAiMemoryModel() = default;

void IxAiMemoryModel::PushSensorySample(IxAiPerceptionEventType type, const Fvector& position, f32 weight, f32 nowTime)
{
    if (type == IxAiPerceptionEventType::None)
    {
        return;
    }

    const f32 injected = weight * g_ixAiRuntimeTuning.memorySampleWeightScale;

    for (auto slotIterator = _sensorySlots.begin(); slotIterator != _sensorySlots.end(); ++slotIterator)
    {
        IxAiMemorySlot& slot = *slotIterator;

        if (slot._type == type && slot._position.distance_to_sqr(position) < IxAiConstants::kMemoryMergeDistanceSq)
        {
            slot._strength += injected;
            slot._timeStamp = nowTime;
            slot._strength = clampr(slot._strength, 0.f, IxAiConstants::kMemoryStrengthCap);
            return;
        }
    }

    if (_sensorySlots.size() >= IxAiConstants::kMemorySlotCap)
    {
        _sensorySlots.pop_front();
    }

    IxAiMemorySlot slot{};
    slot._type = type;
    slot._position = position;
    slot._timeStamp = nowTime;
    slot._strength = injected;
    _sensorySlots.push_back(slot);
}

void IxAiMemoryModel::InjectWorkingBelief(
    IxAiPerceptionEventType type,
    const Fvector& position,
    f32 confidenceDelta,
    f32 nowTime)
{
    if (type == IxAiPerceptionEventType::None || confidenceDelta <= EPS_S)
    {
        return;
    }

    for (auto grainIterator = _workingBeliefs.begin(); grainIterator != _workingBeliefs.end(); ++grainIterator)
    {
        IxAiBeliefGrain& grain = *grainIterator;

        if (grain._type == type && grain._position.distance_to_sqr(position) < IxAiConstants::kWorkingBeliefMergeDistanceSq)
        {
            grain._confidence += confidenceDelta;
            grain._confidence = clampr(grain._confidence, 0.f, IxAiConstants::kBeliefConfidenceCap);
            grain._timeStamp = nowTime;
            grain._position = position;
            return;
        }
    }

    if (_workingBeliefs.size() >= IxAiConstants::kWorkingBeliefCap)
    {
        _workingBeliefs.pop_front();
    }

    IxAiBeliefGrain grain{};
    grain._type = type;
    grain._position = position;
    grain._timeStamp = nowTime;
    grain._confidence = clampr(confidenceDelta, 0.f, IxAiConstants::kBeliefConfidenceCap);
    _workingBeliefs.push_back(grain);
}

void IxAiMemoryModel::PromoteSensoryToWorking(f32 nowTime)
{
    for (auto slotIterator = _sensorySlots.begin(); slotIterator != _sensorySlots.end(); ++slotIterator)
    {
        const IxAiMemorySlot& slot = *slotIterator;

        if (slot._strength < IxAiConstants::kBeliefPromoteStrengthThreshold)
        {
            continue;
        }

        const f32 bump = slot._strength * IxAiConstants::kBeliefPromoteStrengthToConfidence;
        InjectWorkingBelief(slot._type, slot._position, bump, nowTime);
    }
}

void IxAiMemoryModel::IngestSquadFanout(const IxAiPerceptionEvent& event, f32 nowTime, f32& outSuspicionBump)
{
    outSuspicionBump = 0.f;

    if (event._type == IxAiPerceptionEventType::None)
    {
        return;
    }

    PushSensorySample(event._type, event._position, event._intensity, nowTime);
    outSuspicionBump = event._intensity * g_ixAiRuntimeTuning.squadChannelSuspicionScale;
}

void IxAiMemoryModel::AccumulatePerception(
    const xr_vector<IxAiPerceptionEvent>& events,
    const Fvector& selfPosition,
    f32 nowTime,
    f32 alertRadius,
    f32& outSuspicionFromEvents,
    bool& outHasFocus,
    Fvector& outFocusPosition)
{
    outSuspicionFromEvents = 0.f;
    outHasFocus = false;

    if (alertRadius < EPS_L)
    {
        return;
    }

    f32 bestWeighted = 0.f;
    Fvector bestPosition{};
    bool haveBest = false;

    for (const IxAiPerceptionEvent& event : events)
    {
        const f32 dist = selfPosition.distance_to(event._position);
        const f32 falloff = 1.f - clampr(dist / alertRadius, 0.f, 1.f);
        const f32 weighted = event._intensity * falloff;

        if (weighted > EPS_S)
        {
            outSuspicionFromEvents += weighted;
        }

        if (weighted > IxAiConstants::kMemoryPushThreshold)
        {
            PushSensorySample(event._type, event._position, weighted, nowTime);
        }

        if (weighted > bestWeighted)
        {
            bestWeighted = weighted;
            bestPosition = event._position;
            haveBest = true;
        }
    }

    if (haveBest && bestWeighted > EPS_S)
    {
        outFocusPosition = bestPosition;
        outHasFocus = true;
    }
}

void IxAiMemoryModel::TickBeliefLayers(f32 deltaTime, f32 nowTime, f32& outSuspicionFromSensoryLeak)
{
    outSuspicionFromSensoryLeak = 0.f;

    const f32 decayRate = g_ixAiRuntimeTuning.memoryDecayPerSecond;
    const f32 sensoryRetention = IxAiDecay::ExponentialRetentionFactor(deltaTime, decayRate);

    for (auto slotIterator = _sensorySlots.begin(); slotIterator != _sensorySlots.end();)
    {
        slotIterator->_strength *= sensoryRetention;

        if (slotIterator->_strength < g_ixAiRuntimeTuning.memoryStrengthEpsilon)
        {
            slotIterator = _sensorySlots.erase(slotIterator);
        }
        else
        {
            outSuspicionFromSensoryLeak += slotIterator->_strength * deltaTime * g_ixAiRuntimeTuning.memorySuspicionLeakScale;
            ++slotIterator;
        }
    }

    const f32 beliefRetention = IxAiDecay::ExponentialRetentionFactor(deltaTime, IxAiConstants::kWorkingBeliefDecayPerSecond);

    for (auto beliefIterator = _workingBeliefs.begin(); beliefIterator != _workingBeliefs.end();)
    {
        beliefIterator->_confidence *= beliefRetention;

        if (beliefIterator->_confidence < IxAiConstants::kWorkingBeliefEpsilon)
        {
            beliefIterator = _workingBeliefs.erase(beliefIterator);
        }
        else
        {
            ++beliefIterator;
        }
    }

    PromoteSensoryToWorking(nowTime);
}

u32 IxAiMemoryModel::GetSensorySlotCount() const
{
    return (u32)_sensorySlots.size();
}

const IxAiMemorySlot& IxAiMemoryModel::GetSensorySlot(u32 index) const
{
    VERIFY(index < _sensorySlots.size());
    return _sensorySlots[index];
}

u32 IxAiMemoryModel::GetWorkingBeliefCount() const
{
    return (u32)_workingBeliefs.size();
}

const IxAiBeliefGrain& IxAiMemoryModel::GetWorkingBelief(u32 index) const
{
    VERIFY(index < _workingBeliefs.size());
    return _workingBeliefs[index];
}
