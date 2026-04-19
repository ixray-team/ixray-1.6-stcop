#pragma once

#include "../../xrCore/_stl_extensions.h"

#include "IxAiTypes.h"

class IxAiAgent;
class CAI_Stalker;
class IxAiPerceptionSystem;
class IxAiTacticsSystem;

class IxAiManager final
{
public:
    IxAiManager();
    ~IxAiManager();

    IxAiManager(const IxAiManager&) = delete;
    IxAiManager& operator=(const IxAiManager&) = delete;

    void Update(f32 deltaTime);

    IxAiPerceptionSystem& Perception();
    const IxAiPerceptionSystem& Perception() const;

    IxAiTacticsSystem& Tactics();
    const IxAiTacticsSystem& Tactics() const;

    u32 GetAgentCount() const;
    IxAiAgent* GetAgentByIndex(u32 index);
    const IxAiAgent* GetAgentByIndex(u32 index) const;

    f32 GetLastUpdateDurationMs() const;
    u32 GetLastVisualProbeCount() const;
    u32 GetLastCorpseProbeCount() const;

    bool HasValidSubsystems() const;

    void DeliverSquadFanout(const xr_vector<u16>& recipientObjectIds, const IxAiPerceptionEvent& event);

private:
    void SyncStalkerAgents();

    void RunAgentPerceptionAndBehaviourPass(
        IxAiAgent& agent,
        const Fvector& agentPosition,
        f32 perceptionQueryRadius,
        f32 deltaTime,
        f32 nowTime,
        bool hasThreat,
        const Fvector& threatPosition,
        CAI_Stalker* stalker);

    static void ApplySuspicionDecayAndAlertBands(IxAiAgent& agent, f32 deltaTime);

    IxAiPerceptionSystem* _perception{};
    IxAiTacticsSystem* _tactics{};
    xr_vector<IxAiAgent*> _agents{};
    xr_hash_map<u16, size_t> _agentIndexByObjectId{};
    xr_vector<IxAiPerceptionEvent> _scratchEvents{};
    u32 _registryPhase{};
    f32 _lastUpdateDurationMs{};
    u32 _lastVisualProbeCount{};
    u32 _lastCorpseProbeCount{};
};
