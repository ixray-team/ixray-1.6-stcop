#include "StdAfx.h"

#include "../../xrCore/EngineExternal.h"
#include "../../xrEngine/device.h"
#include "../../xrPhysics/ExtendedGeom.h"
#include "../../xrPhysics/PhysicsShell.h"
#include "../Bolt.h"
#include "../Level.h"
#include "IxAiConstants.h"
#include "IxAiManager.h"
#include "IxAiPhysicsIngest.h"
#include "IxAiStackApi.h"
#include "IxAiTypes.h"

void IxAiStackIngestBoltImpact(const Fvector& position, u16 sourceObjectId, f32 linearSpeed)
{
    if (g_dedicated_server)
    {
        return;
    }

    if (!EngineExternal().CallOfPripyatMode())
    {
        return;
    }

    if (!EngineExternal()[EEngineExternalGame::EnableIxAiStack])
    {
        return;
    }

    if (!IsGameTypeSingle())
    {
        return;
    }

    IxAiManager* manager = IxAiStackApi::Manager();

    if (manager == nullptr)
    {
        return;
    }

    const f32 nowTime = Device.fTimeGlobal;
    const f32 intensity = clampr(linearSpeed * 0.035f, 0.42f, 1.85f);

    manager->Perception().IngestDistractionCue(
        position,
        sourceObjectId,
        nowTime,
        intensity,
        12.f,
        IxAiPerceptionEventType::SoundBoltImpact);
}

void IxAiBoltContactCallback(bool& do_collide, bool bo1, dContact& c, SGameMtl* material_1, SGameMtl* material_2)
{
    (void)do_collide;
    (void)bo1;
    (void)material_1;
    (void)material_2;

    if (g_dedicated_server)
    {
        return;
    }

    if (!EngineExternal().CallOfPripyatMode())
    {
        return;
    }

    if (!EngineExternal()[EEngineExternalGame::EnableIxAiStack])
    {
        return;
    }

    if (!IsGameTypeSingle())
    {
        return;
    }

    dxGeomUserData* userData1 = PHRetrieveGeomUserData(c.geom.g1);
    dxGeomUserData* userData2 = PHRetrieveGeomUserData(c.geom.g2);

    CBolt* bolt = userData1 != nullptr ? smart_cast<CBolt*>(userData1->ph_ref_object) : nullptr;

    if (bolt == nullptr)
    {
        bolt = userData2 != nullptr ? smart_cast<CBolt*>(userData2->ph_ref_object) : nullptr;
    }

    if (bolt == nullptr || bolt->getDestroy())
    {
        return;
    }

    if (bolt->PPhysicsShell() == nullptr || !bolt->PPhysicsShell()->isActive())
    {
        return;
    }

    const u32 now = Device.dwTimeGlobal;
    static u32 s_lastBoltIxTime = 0u;
    static u16 s_lastBoltIxId = u16(-1);

    if (bolt->ID() == s_lastBoltIxId && (now - s_lastBoltIxTime) < IxAiConstants::kBoltIxThrottleMs)
    {
        return;
    }

    s_lastBoltIxId = bolt->ID();
    s_lastBoltIxTime = now;

    Fvector velocity{};
    bolt->PPhysicsShell()->get_LinearVel(velocity);
    const f32 speed = velocity.magnitude();

    IxAiStackIngestBoltImpact(bolt->Position(), bolt->ID(), speed);
}
