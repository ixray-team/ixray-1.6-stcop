#include "stdafx.h"
#include "AnimNotifyGame.h"
#include "pch_script.h"
#include "alife_object_registry.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "alife_spawn_registry.h"
#include "alife_registry_container.h" // without this not compiles

#include "ai_space.h"
#include "alife_registry_container_composition.h"
#include "alife_registry_container_space.h"
#include "alife_simulator.h"
#include "AnimNotifyDisableInfo.h"
#include "AnimNotifyGiveInfo.h"
#include "AnimNotifyLuaFunctor.h"
#include "AnimNotifyPlayParticle.h"
#include "AnimNotifyPlaySound.h"
#include "Level.h"
#include "../xrCore/AnimNotify/AnimNotifyRegistry.h"

void CAnimNotifyHandler::TriggerNotify(IAnimNotifyMessage* notify)
{
    xrCriticalSectionGuard guard(NotifyQueue.Lock);
    NotifyQueue.Queue.push(notify);
}

void CAnimNotifyHandler::Update()
{
    xrCriticalSectionGuard guard(NotifyQueue.Lock);
    while (!NotifyQueue.Queue.empty())
    {
        auto Name = NotifyQueue.Queue.front();
        NotifyQueue.Queue.pop();
        ProcessNotify(Name);
        xr_delete(Name);
    }
}

void CAnimNotifyHandler::ProcessNotify(IAnimNotifyMessage* Message)
{
    auto Notify = CAnimNotifyRegistry::GetInstance().get(Message->notify);
    R_ASSERT3(Notify, "Invalid notify", Message->notify.c_str());
    Notify->Execute(Message->render_visual, Message->bone_id);
}

IAnimNotify* CAnimNotifyHandler::ConstructNotify(const EAnimNotifyType type)
{
    switch (type)
    {
    case EAnimNotifyType::give_info:
        {
            return new CAnimNotifyGiveInfo();
        }
    case EAnimNotifyType::disable_info:
        {
            return new CAnimNotifyDisableInfo();
        }
    case EAnimNotifyType::lua_functor:
        {
            return new CAnimNotifyLuaFunctor();
        }
    case EAnimNotifyType::play_sound:
        {
            return new CAnimNotifyPlaySound();
        }
    case EAnimNotifyType::play_particle:
        {
            return new CAnimNotifyPlayParticle();
        }
    }
    VERIFY2(false, "Unknown anim notify type");
    return nullptr;
}
