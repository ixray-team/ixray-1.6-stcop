#pragma once

#include "../../xrCore/_stl_extensions.h"
#include "IxAiMemoryModel.h"
#include "IxAiTypes.h"

class IxAiBehaviourTree;

struct IxAiAgentPushTimestamps final
{
    u32 _bridgeDangerPush{};
    u32 _bridgeCombatPush{};
    u32 _tacticHintDangerPush{};
    u32 _coverHintDangerPush{};
    u32 _investigateHintPush{};
};

struct IxAiAgentSpatialHints final
{
    bool _hasLastFocus{};
    Fvector _lastFocusPosition{};
    bool _hasTacticalHint{};
    Fvector _tacticalHintPosition{};
};

class IxAiAgent final
{
public:
    IxAiAgent();
    ~IxAiAgent();

    IxAiAgent(const IxAiAgent&) = delete;
    IxAiAgent& operator=(const IxAiAgent&) = delete;

    void SetObjectId(u16 objectId);
    u16 GetObjectId() const;

    void SetProfile(const IxAiBehaviourProfile& profile);
    const IxAiBehaviourProfile& GetProfile() const;

    IxAiAlertLevel GetAlertLevel() const;
    void SetAlertLevel(IxAiAlertLevel level);

    f32 GetSuspicionScore() const;
    void AddSuspicion(f32 amount);
    void DecaySuspicion(f32 amount);

    void AccumulatePerception(const xr_vector<IxAiPerceptionEvent>& events, const Fvector& selfPosition, f32 nowTime);

    void ReceiveSquadFanout(const IxAiPerceptionEvent& event, f32 nowTime);

    void TickMemory(f32 deltaTime, f32 nowTime);

    void Update(f32 deltaTime);

    bool HasLastFocus() const;
    const Fvector& GetLastFocusPosition() const;

    u32 GetBridgeLastDangerPushTime() const;
    void SetBridgeLastDangerPushTime(u32 time);

    u32 GetBridgeLastCombatPushTime() const;
    void SetBridgeLastCombatPushTime(u32 time);

    u32 GetTacticHintDangerPushTime() const;
    void SetTacticHintDangerPushTime(u32 time);

    u32 GetCoverHintDangerPushTime() const;
    void SetCoverHintDangerPushTime(u32 time);

    u32 GetInvestigateHintPushTime() const;
    void SetInvestigateHintPushTime(u32 time);

    bool HasTacticalHint() const;
    const Fvector& GetTacticalHintPosition() const;
    void SetTacticalHint(const Fvector& worldPosition, bool valid);

    u32 GetMemorySlotCount() const;
    const IxAiMemorySlot& GetMemorySlot(u32 index) const;

    const IxAiMemoryModel& GetMemoryModel() const;
    IxAiMemoryModel& GetMemoryModel();

private:
    u16 _objectId{};
    IxAiBehaviourProfile _profile{};
    IxAiAlertLevel _alertLevel{IxAiAlertLevel::Vigilant};
    f32 _suspicionScore{};
    IxAiBehaviourTree* _behaviourTree{};

    IxAiAgentSpatialHints _spatialHints{};
    IxAiAgentPushTimestamps _pushTimestamps{};

    IxAiMemoryModel _memoryModel{};
};
