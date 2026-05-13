#include "StdAfx.h"

#include <algorithm>
#include <chrono>

#include "../../xrEngine/device.h"
#include "../../xrEngine/IGame_Level.h"
#include "../../xrEngine/xr_object.h"
#include "../Actor.h"
#include "../entity_alive.h"
#include "../Level.h"
#include "../ai/stalker/ai_stalker.h"
#include "IxAiAgent.h"
#include "IxAiConstants.h"
#include "IxAiDecay.h"
#include "IxAiLocalityCoordinator.h"
#include "IxAiManager.h"
#include "IxAiPerceptionSystem.h"
#include "IxAiStackApi.h"
#include "IxAiStackTelemetry.h"
#include "IxAiStackTuning.h"

#include <new>
#include "IxAiStalkerLegacyOutput.h"
#include "IxAiTacticsSystem.h"

namespace
{
    CAI_Stalker* TryGetAliveStalker(CObject* objectPtr)
    {
        if (objectPtr == nullptr || objectPtr->getDestroy())
        {
            return nullptr;
        }

        CAI_Stalker* stalker = objectPtr->cast_stalker();

        if (stalker == nullptr || !stalker->g_Alive())
        {
            return nullptr;
        }

        return stalker;
    }
}

void IxAiManager::ApplySuspicionDecayAndAlertBands(IxAiAgent& agent, f32 deltaTime)
{
    const IxAiBehaviourProfile& profile = agent.GetProfile();
    const f32 decay = IxAiDecay::LinearSuspicionStep(profile._suspicionDecayRate, g_ixAiRuntimeTuning.globalSuspicionDecayScale, deltaTime);
    agent.DecaySuspicion(decay);

    const f32 suspicion = agent.GetSuspicionScore();

    if (suspicion < g_ixAiRuntimeTuning.suspicionToSuspicious)
    {
        agent.SetAlertLevel(IxAiAlertLevel::Vigilant);
    }
    else if (suspicion < g_ixAiRuntimeTuning.suspicionToSearch)
    {
        agent.SetAlertLevel(IxAiAlertLevel::Suspicious);
    }
    else if (suspicion < g_ixAiRuntimeTuning.suspicionToCombat)
    {
        agent.SetAlertLevel(IxAiAlertLevel::Search);
    }
    else
    {
        agent.SetAlertLevel(IxAiAlertLevel::Combat);
    }
}

IxAiManager::IxAiManager()
{
    _perception = new (std::nothrow) IxAiPerceptionSystem();
    _tactics = new (std::nothrow) IxAiTacticsSystem();
    _scratchEvents.reserve(IxAiConstants::kPerceptionMaxEventsNearQuery);
}

IxAiManager::~IxAiManager()
{
    for (IxAiAgent* agent : _agents)
    {
        xr_delete(agent);
    }

    _agents.clear();
    _agentIndexByObjectId.clear();

    xr_delete(_tactics);
    xr_delete(_perception);
}

void IxAiManager::SyncStalkerAgents()
{
    if (g_pGameLevel == nullptr || !g_pGameLevel->bReady)
    {
        return;
    }

    CObjectList& objects = g_pGameLevel->Objects;
    const u32 objectCount = objects.o_count();
    u32 aliveStalkerCount = 0;

    for (u32 iteratorIndex = 0; iteratorIndex < objectCount; ++iteratorIndex)
    {
        CObject* objectPtr = objects.o_get_by_iterator(iteratorIndex);

        if (TryGetAliveStalker(objectPtr) != nullptr)
        {
            ++aliveStalkerCount;
        }
    }

    xr_hash_set<u16> seenIds{};
    seenIds.reserve(aliveStalkerCount);

    if (_agents.capacity() < (size_t)aliveStalkerCount)
    {
        _agents.reserve(aliveStalkerCount);
    }

    const size_t mapReserveSize = std::max(_agentIndexByObjectId.size(), (size_t)aliveStalkerCount);
    _agentIndexByObjectId.reserve(mapReserveSize);

    for (u32 iteratorIndex = 0; iteratorIndex < objectCount; ++iteratorIndex)
    {
        CObject* objectPtr = objects.o_get_by_iterator(iteratorIndex);
        CAI_Stalker* stalker = TryGetAliveStalker(objectPtr);

        if (stalker == nullptr)
        {
            continue;
        }

        const u16 objectId = stalker->ID();
        seenIds.insert(objectId);

        if (_agentIndexByObjectId.find(objectId) != _agentIndexByObjectId.end())
        {
            continue;
        }

        IxAiAgent* newAgent = new IxAiAgent();
        newAgent->SetObjectId(objectId);

        IxAiBehaviourProfile defaultProfile{};
        defaultProfile._suspicionDecayRate = IxAiConstants::kDefaultProfileSuspicionDecayRate;
        defaultProfile._alertRadius = IxAiConstants::kDefaultProfileAlertRadius;
        defaultProfile._silencedGunHearingMultiplier = IxAiConstants::kDefaultProfileSilencedGunHearingMultiplier;
        defaultProfile._alwaysLooksForCover = false;
        defaultProfile._flankRange = IxAiConstants::kDefaultProfileFlankRange;
        defaultProfile._behaviourKind =
            ((objectId % 2u) == 0u) ? IxAiBehaviourKind::GuardBasic : IxAiBehaviourKind::FlankerLite;
        defaultProfile._useBehaviourTreeNodePool =
            (defaultProfile._behaviourKind == IxAiBehaviourKind::FlankerLite);
        newAgent->SetProfile(defaultProfile);

        const size_t newIndex = _agents.size();
        _agents.push_back(newAgent);
        _agentIndexByObjectId.emplace(objectId, newIndex);
    }

    size_t agentIndex = 0;

    while (agentIndex < _agents.size())
    {
        const u16 id = _agents[agentIndex]->GetObjectId();

        if (seenIds.find(id) != seenIds.end())
        {
            ++agentIndex;
            continue;
        }

        xr_delete(_agents[agentIndex]);

        const size_t lastIndex = _agents.size() - 1;

        if (agentIndex != lastIndex)
        {
            _agents[agentIndex] = _agents[lastIndex];
            const u16 movedId = _agents[agentIndex]->GetObjectId();
            _agentIndexByObjectId[movedId] = agentIndex;
        }

        _agents.pop_back();
        _agentIndexByObjectId.erase(id);
    }
}

void IxAiManager::Update(f32 deltaTime)
{
    xrCriticalSectionGuard tuningGuard(g_ixAiRuntimeTuningCs);

    const auto timeStart = std::chrono::high_resolution_clock::now();

    IxAiStackTelemetry_ResetFrame();

    const f32 nowTime = Device.fTimeGlobal;

    _perception->Update(deltaTime, nowTime);

    ++_registryPhase;

    if ((_registryPhase % IxAiConstants::kAgentSyncIntervalFrames) == 0u)
    {
        SyncStalkerAgents();
    }

    CActor* actor = Actor();
    CEntityAlive* actorAlive = nullptr;

    if (actor != nullptr && actor->g_Alive() && !actor->getDestroy())
    {
        actorAlive = actor;
    }

    u32 visualProbes = 0;
    const u32 interval = std::max(1u, g_ixAiRuntimeTuning.visualProbeIntervalFrames);
    const u32 maxProbe = g_ixAiRuntimeTuning.maxVisualProbesPerFrame;

    if (actorAlive != nullptr && g_pGameLevel != nullptr && g_pGameLevel->bReady)
    {
        const u32 objectCount = g_pGameLevel->Objects.o_count();

        for (u32 iteratorIndex = 0; iteratorIndex < objectCount && visualProbes < maxProbe; ++iteratorIndex)
        {
            CObject* objectPtr = g_pGameLevel->Objects.o_get_by_iterator(iteratorIndex);

            if (objectPtr == nullptr || objectPtr->getDestroy())
            {
                continue;
            }

            CAI_Stalker* stalkerProbe = objectPtr->cast_stalker();

            if (stalkerProbe == nullptr || !stalkerProbe->g_Alive())
            {
                continue;
            }

            const u16 stalkerId = stalkerProbe->ID();

            if (((_registryPhase + stalkerId) % interval) != 0u)
            {
                continue;
            }

            _perception->IngestStalkerVisualProbe(
                *stalkerProbe,
                *actorAlive,
                nowTime,
                g_ixAiRuntimeTuning.visualProbeIntensity,
                g_ixAiRuntimeTuning.visualProbeRadius);
            ++visualProbes;
        }
    }

    _lastVisualProbeCount = visualProbes;

    u32 corpseProbes = 0;
    const u32 corpseInterval = std::max(1u, g_ixAiRuntimeTuning.corpseProbeIntervalFrames);
    const f32 corpseRadius = g_ixAiRuntimeTuning.corpseProbeRadius;
    const f32 corpseRadiusSq = corpseRadius * corpseRadius;

    if (actorAlive != nullptr && g_pGameLevel != nullptr && g_pGameLevel->bReady)
    {
        const Fvector actorPosition = actorAlive->Position();
        const u32 objectCountCorpse = g_pGameLevel->Objects.o_count();

        for (u32 iteratorIndex = 0; iteratorIndex < objectCountCorpse && corpseProbes < IxAiConstants::kCorpseProbeMaxPerFrame;
             ++iteratorIndex)
        {
            CObject* objectPtr = g_pGameLevel->Objects.o_get_by_iterator(iteratorIndex);

            if (objectPtr == nullptr || objectPtr->getDestroy())
            {
                continue;
            }

            const u16 objectId = objectPtr->ID();

            if (((_registryPhase + objectId) % corpseInterval) != 0u)
            {
                continue;
            }

            CEntityAlive* entityAlive = objectPtr->cast_entity_alive();

            if (entityAlive == nullptr || entityAlive->g_Alive())
            {
                continue;
            }

            const Fvector corpsePos = entityAlive->Position();

            if (corpsePos.distance_to_sqr(actorPosition) > corpseRadiusSq)
            {
                continue;
            }

            _perception->IngestCorpseProbe(
                corpsePos,
                entityAlive->ID(),
                nowTime,
                g_ixAiRuntimeTuning.corpseEventIntensity,
                IxAiConstants::kCorpseIngestEventRadius);
            ++corpseProbes;
        }
    }

    _lastCorpseProbeCount = corpseProbes;

    CObject* viewEntity = (g_pGameLevel != nullptr) ? g_pGameLevel->CurrentViewEntity() : nullptr;
    Fvector lodReference{};
    bool hasLodReference = false;

    if (viewEntity != nullptr)
    {
        lodReference = viewEntity->Position();
        hasLodReference = true;
    }

    const bool hasThreat = (actorAlive != nullptr);
    Fvector threatPosition{};

    if (hasThreat)
    {
        threatPosition = actorAlive->Position();
    }

    for (IxAiAgent* agent : _agents)
    {
        if (g_pGameLevel == nullptr)
        {
            break;
        }

        CObject* objectPtr = g_pGameLevel->Objects.net_Find(agent->GetObjectId());

        if (objectPtr == nullptr || objectPtr->getDestroy())
        {
            continue;
        }

        CAI_Stalker* stalker = objectPtr->cast_stalker();
        const Fvector agentPosition = objectPtr->Position();

        f32 perceptionQueryRadius = agent->GetProfile()._alertRadius;

        if (hasLodReference && agentPosition.distance_to_sqr(lodReference) > IxAiConstants::kLodDistanceSq)
        {
            perceptionQueryRadius *= IxAiConstants::kLodAlertRadiusScale;
        }

        RunAgentPerceptionAndBehaviourPass(*agent, agentPosition, perceptionQueryRadius, deltaTime, nowTime, hasThreat, threatPosition, stalker);
    }

    const auto timeEnd = std::chrono::high_resolution_clock::now();
    _lastUpdateDurationMs = std::chrono::duration<f32, std::milli>(timeEnd - timeStart).count();
}

IxAiPerceptionSystem& IxAiManager::Perception()
{
    R_ASSERT2(_perception, "IxAiManager::_perception");
    return *_perception;
}

const IxAiPerceptionSystem& IxAiManager::Perception() const
{
    R_ASSERT2(_perception, "IxAiManager::_perception");
    return *_perception;
}

IxAiTacticsSystem& IxAiManager::Tactics()
{
    R_ASSERT2(_tactics, "IxAiManager::_tactics");
    return *_tactics;
}

const IxAiTacticsSystem& IxAiManager::Tactics() const
{
    R_ASSERT2(_tactics, "IxAiManager::_tactics");
    return *_tactics;
}

u32 IxAiManager::GetAgentCount() const
{
    return (u32)_agents.size();
}

IxAiAgent* IxAiManager::GetAgentByIndex(u32 index)
{
    VERIFY(index < _agents.size());
    return _agents[index];
}

const IxAiAgent* IxAiManager::GetAgentByIndex(u32 index) const
{
    VERIFY(index < _agents.size());
    return _agents[index];
}

f32 IxAiManager::GetLastUpdateDurationMs() const
{
    return _lastUpdateDurationMs;
}

u32 IxAiManager::GetLastVisualProbeCount() const
{
    return _lastVisualProbeCount;
}

u32 IxAiManager::GetLastCorpseProbeCount() const
{
    return _lastCorpseProbeCount;
}

bool IxAiManager::HasValidSubsystems() const
{
    return _perception != nullptr && _tactics != nullptr;
}

void IxAiManager::DeliverSquadFanout(const xr_vector<u16>& recipientObjectIds, const IxAiPerceptionEvent& event)
{
    const f32 nowTime = event._timestamp;
    const bool applyStealthPositionBlend =
        IxAiStackApi::IsFeatureEnabled(IxAiFeatureGate::SquadStealthFanout) &&
        (event._squadFanoutFlags & (u8)IxAiSquadFanoutFlags::StealthClassified) != 0u;

    for (u16 objectId : recipientObjectIds)
    {
        const auto iterator = _agentIndexByObjectId.find(objectId);

        if (iterator == _agentIndexByObjectId.end())
        {
            continue;
        }

        IxAiPerceptionEvent deliveryEvent = event;

        if (applyStealthPositionBlend && g_pGameLevel != nullptr && g_pGameLevel->bReady)
        {
            CObject* objectPtr = g_pGameLevel->Objects.net_Find(objectId);

            if (objectPtr != nullptr && !objectPtr->getDestroy())
            {
                const Fvector recipientPosition = objectPtr->Position();
                const f32 victimWeight = clampr(g_ixAiRuntimeTuning.squadFanoutStealthVictimPositionWeight, 0.f, 1.f);
                deliveryEvent._position.lerp(recipientPosition, event._position, victimWeight);
            }
        }

        IxAiAgent* agent = _agents[iterator->second];
        agent->ReceiveSquadFanout(deliveryEvent, nowTime);
    }
}

void IxAiManager::RunAgentPerceptionAndBehaviourPass(
    IxAiAgent& agent,
    const Fvector& agentPosition,
    f32 perceptionQueryRadius,
    f32 deltaTime,
    f32 nowTime,
    bool hasThreat,
    const Fvector& threatPosition,
    CAI_Stalker* stalker)
{
    _scratchEvents.clear();
    _perception->CopyEventsNear(agentPosition, perceptionQueryRadius, nowTime, _scratchEvents);
    IxAiLocalityCoordinator::ApplyActorAnchoredAttenuation(agentPosition, hasThreat, threatPosition, _scratchEvents);
    agent.AccumulatePerception(_scratchEvents, agentPosition, nowTime);
    agent.TickMemory(deltaTime, nowTime);
    ApplySuspicionDecayAndAlertBands(agent, deltaTime);

    if (stalker != nullptr && hasThreat)
    {
        _tactics->EvaluateForStalker(agent, *stalker, threatPosition);
    }

    if (stalker != nullptr)
    {
        _tactics->TryPublishTacticDangerHint(*stalker, agent);

        if (hasThreat)
        {
            _tactics->TryPublishCoverDangerHint(*stalker, agent, threatPosition, _registryPhase);
        }
    }

    agent.Update(deltaTime);

    if (stalker != nullptr)
    {
        _tactics->TryPublishInvestigateMovementHint(*stalker, agent);
        IxAiStalkerLegacyOutput_Apply(*stalker, agent);
    }
}
