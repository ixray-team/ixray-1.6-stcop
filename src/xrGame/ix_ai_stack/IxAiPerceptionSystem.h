#pragma once

#include "../../xrCore/_stl_extensions.h"
#include "../../xrEngine/IGame_Level.h"
#include "IxAiPerceptionSpatialGrid.h"
#include "IxAiTypes.h"

class CAI_Stalker;
class CEntityAlive;

class IxAiPerceptionSystem final
{
public:
    IxAiPerceptionSystem();
    ~IxAiPerceptionSystem();

    void Update(f32 deltaTime, f32 nowTime);
    void Clear();

    void IngestEngineSoundDelegates(const xr_vector<IGame_Level::_esound_delegate>& delegates);
    void IngestDistractionCue(
        const Fvector& position,
        u16 sourceObjectId,
        f32 nowTime,
        f32 intensity,
        f32 radius,
        IxAiPerceptionEventType type);
    void IngestStalkerVisualProbe(CAI_Stalker& stalker, CEntityAlive& targetAlive, f32 nowTime, f32 intensity, f32 radius);
    void IngestCorpseProbe(const Fvector& corpsePosition, u16 corpseObjectId, f32 nowTime, f32 intensity, f32 radius);
    void CopyEventsNear(const Fvector& origin, f32 radius, f32 nowTime, xr_vector<IxAiPerceptionEvent>& out) const;

    u32 GetGlobalEventCount() const;
    const IxAiPerceptionEvent& GetGlobalEvent(u32 index) const;

private:
    void PruneOldEvents(f32 nowTime);

    xr_vector<IxAiPerceptionEvent> _globalEvents{};
    mutable IxAiPerceptionSpatialGrid _spatialGrid{};
    mutable bool _spatialDirty{true};
    mutable xr_vector<u32> _spatialQueryScratch{};
};
