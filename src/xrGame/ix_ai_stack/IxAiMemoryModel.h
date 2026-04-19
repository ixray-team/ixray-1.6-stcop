#pragma once

#include "../../xrCore/_stl_extensions.h"
#include "../../xrCore/_vector3d.h"
#include "IxAiTypes.h"

class IxAiMemoryModel final
{
public:
    IxAiMemoryModel();
    ~IxAiMemoryModel();

    IxAiMemoryModel(const IxAiMemoryModel&) = delete;
    IxAiMemoryModel& operator=(const IxAiMemoryModel&) = delete;

    void AccumulatePerception(
        const xr_vector<IxAiPerceptionEvent>& events,
        const Fvector& selfPosition,
        f32 nowTime,
        f32 alertRadius,
        f32& outSuspicionFromEvents,
        bool& outHasFocus,
        Fvector& outFocusPosition);

    void TickBeliefLayers(f32 deltaTime, f32 nowTime, f32& outSuspicionFromSensoryLeak);

    void IngestSquadFanout(const IxAiPerceptionEvent& event, f32 nowTime, f32& outSuspicionBump);

    u32 GetSensorySlotCount() const;
    const IxAiMemorySlot& GetSensorySlot(u32 index) const;

    u32 GetWorkingBeliefCount() const;
    const IxAiBeliefGrain& GetWorkingBelief(u32 index) const;

private:
    void PushSensorySample(IxAiPerceptionEventType type, const Fvector& position, f32 weight, f32 nowTime);
    void InjectWorkingBelief(IxAiPerceptionEventType type, const Fvector& position, f32 confidenceDelta, f32 nowTime);
    void PromoteSensoryToWorking(f32 nowTime);

    xr_deque<IxAiMemorySlot> _sensorySlots{};
    xr_deque<IxAiBeliefGrain> _workingBeliefs{};
};
