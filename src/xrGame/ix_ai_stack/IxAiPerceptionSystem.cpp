#include "StdAfx.h"

#include "../../xrEngine/device.h"
#include "../../xrSound/Sound.h"
#include "../ai/stalker/ai_stalker.h"
#include "../entity_alive.h"
#include "../GameObject.h"
#include "IxAiConstants.h"
#include "IxAiPerceptionSystem.h"
#include "IxAiSoundClassification.h"
#include "IxAiStackTelemetry.h"
#include "IxAiVisionEvaluator.h"

#include "../../xrEngine/xr_object.h"

IxAiPerceptionSystem::IxAiPerceptionSystem()
{
    _globalEvents.reserve(IxAiConstants::kPerceptionGlobalEventCap);
    _spatialQueryScratch.reserve(IxAiConstants::kPerceptionGlobalEventCap);
}

IxAiPerceptionSystem::~IxAiPerceptionSystem() = default;

void IxAiPerceptionSystem::Update(f32 deltaTime, f32 nowTime)
{
    (void)deltaTime;
    PruneOldEvents(nowTime);
}

void IxAiPerceptionSystem::Clear()
{
    _globalEvents.clear();
    _spatialGrid.Clear();
    _spatialDirty = true;
}

void IxAiPerceptionSystem::IngestStalkerVisualProbe(
    CAI_Stalker& stalker,
    CEntityAlive& targetAlive,
    f32 nowTime,
    f32 intensity,
    f32 radius)
{
    if (!stalker.g_Alive() || stalker.getDestroy())
    {
        return;
    }

    if (!targetAlive.g_Alive() || targetAlive.getDestroy())
    {
        return;
    }

    if (!stalker.is_relation_enemy(&targetAlive))
    {
        return;
    }

    CGameObject* gameObject = targetAlive.cast_game_object();

    if (gameObject == nullptr)
    {
        return;
    }

    if (!IxAiVisionEvaluator::StalkerHasClearSightToTarget(stalker, targetAlive))
    {
        return;
    }

    IxAiPerceptionEvent event{};
    event._position = targetAlive.Position();
    event._type = IxAiPerceptionEventType::VisualPlayer;
    event._intensity = intensity;
    event._radius = radius;
    event._timestamp = nowTime;
    event._sourceObjectId = targetAlive.ID();

    _globalEvents.push_back(event);

    while (_globalEvents.size() > IxAiConstants::kPerceptionGlobalEventCap)
    {
        _globalEvents.erase(_globalEvents.begin());
    }

    _spatialDirty = true;
}

void IxAiPerceptionSystem::IngestCorpseProbe(
    const Fvector& corpsePosition,
    u16 corpseObjectId,
    f32 nowTime,
    f32 intensity,
    f32 radius)
{
    IxAiPerceptionEvent event{};
    event._position = corpsePosition;
    event._type = IxAiPerceptionEventType::VisualCorpse;
    event._intensity = intensity;
    event._radius = radius;
    event._timestamp = nowTime;
    event._sourceObjectId = corpseObjectId;

    _globalEvents.push_back(event);

    while (_globalEvents.size() > IxAiConstants::kPerceptionGlobalEventCap)
    {
        _globalEvents.erase(_globalEvents.begin());
    }

    _spatialDirty = true;
}

void IxAiPerceptionSystem::IngestEngineSoundDelegates(const xr_vector<IGame_Level::_esound_delegate>& delegates)
{
    const f32 nowTime = Device.fTimeGlobal;
    u32 ingestedCount = 0;

    for (const IGame_Level::_esound_delegate& delegateItem : delegates)
    {
        if (delegateItem.source == nullptr || delegateItem.dest == nullptr)
        {
            continue;
        }

        if (delegateItem.source->slot == 0)
        {
            continue;
        }

        const CSound_params& params = delegateItem.source->get_params();
        Fvector position = params.position;
        if (delegateItem.source->is_2d())
        {
            position.add(Sound->listener_position());
        }

        const int soundType = delegateItem.source->g_type;
        const IxAiPerceptionEventType mappedType = IxAiSoundClassification::MapEngineSoundType(soundType, delegateItem.power);

        if (mappedType == IxAiPerceptionEventType::None)
        {
            continue;
        }

        IxAiPerceptionEvent event{};
        event._position = position;
        event._type = mappedType;
        event._intensity = delegateItem.power;
        event._radius = params.max_ai_distance;
        event._timestamp = nowTime;

        CObject* who = delegateItem.source->g_object;
        if (who != nullptr)
        {
            event._sourceObjectId = who->ID();
        }
        else
        {
            event._sourceObjectId = u16(-1);
        }

        _globalEvents.push_back(event);
        ++ingestedCount;

        while (_globalEvents.size() > IxAiConstants::kPerceptionGlobalEventCap)
        {
            _globalEvents.erase(_globalEvents.begin());
        }
    }

    if (ingestedCount > 0u)
    {
        IxAiStackTelemetry_AddSoundIngest(ingestedCount);
        _spatialDirty = true;
    }
}

void IxAiPerceptionSystem::IngestDistractionCue(
    const Fvector& position,
    u16 sourceObjectId,
    f32 nowTime,
    f32 intensity,
    f32 radius,
    IxAiPerceptionEventType type)
{
    if (type == IxAiPerceptionEventType::None)
    {
        return;
    }

    IxAiPerceptionEvent event{};
    event._position = position;
    event._type = type;
    event._intensity = intensity;
    event._radius = radius;
    event._timestamp = nowTime;
    event._sourceObjectId = sourceObjectId;

    _globalEvents.push_back(event);

    while (_globalEvents.size() > IxAiConstants::kPerceptionGlobalEventCap)
    {
        _globalEvents.erase(_globalEvents.begin());
    }

    IxAiStackTelemetry_AddSoundIngest(1u);
    _spatialDirty = true;
}

void IxAiPerceptionSystem::CopyEventsNear(const Fvector& origin, f32 radius, f32 nowTime, xr_vector<IxAiPerceptionEvent>& out) const
{
    out.clear();

    if (out.capacity() < (size_t)IxAiConstants::kPerceptionMaxEventsNearQuery)
    {
        out.reserve(IxAiConstants::kPerceptionMaxEventsNearQuery);
    }

    if (_spatialDirty)
    {
        _spatialGrid.Rebuild(_globalEvents);
        _spatialDirty = false;
    }

    const f32 radiusSq = radius * radius;
    const size_t eventCount = _globalEvents.size();

    if (_spatialQueryScratch.capacity() < eventCount)
    {
        _spatialQueryScratch.reserve(eventCount);
    }

    _spatialGrid.GatherCandidateIndices(origin, radius, _spatialQueryScratch);

    for (u32 eventIndex : _spatialQueryScratch)
    {
        VERIFY(eventIndex < _globalEvents.size());
        const IxAiPerceptionEvent& event = _globalEvents[eventIndex];

        if (nowTime - event._timestamp > IxAiConstants::kPerceptionEventRetentionSeconds)
        {
            continue;
        }

        if (event._type == IxAiPerceptionEventType::None)
        {
            continue;
        }

        const f32 distSq = origin.distance_to_sqr(event._position);

        if (distSq > radiusSq)
        {
            continue;
        }

        out.push_back(event);

        if (out.size() >= IxAiConstants::kPerceptionMaxEventsNearQuery)
        {
            break;
        }
    }
}

u32 IxAiPerceptionSystem::GetGlobalEventCount() const
{
    return (u32)_globalEvents.size();
}

const IxAiPerceptionEvent& IxAiPerceptionSystem::GetGlobalEvent(u32 index) const
{
    VERIFY(index < _globalEvents.size());
    return _globalEvents[index];
}

void IxAiPerceptionSystem::PruneOldEvents(f32 nowTime)
{
    if (_globalEvents.empty())
    {
        return;
    }

    const f32 cutoff = nowTime - IxAiConstants::kPerceptionEventRetentionSeconds;
    bool removedAny = false;

    for (auto it = _globalEvents.begin(); it != _globalEvents.end();)
    {
        if (it->_timestamp < cutoff)
        {
            it = _globalEvents.erase(it);
            removedAny = true;
        }
        else
        {
            ++it;
        }
    }

    if (removedAny)
    {
        _spatialDirty = true;
    }
}
