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
#include "Level.h"
#include "../xrCore/AnimNotify/AnimNotifyRegistry.h"

void CAnimNotifyHandler::TriggerNotify(IAnimNotifyMessage* notify)
{
    xrCriticalSectionGuard guard(NotifyQueue.Lock);
    NotifyQueue.Queue.push(notify);
}

void CAnimNotifyHandler::Update()
{
    {
        xrCriticalSectionGuard guard(GiveInfoQueue.Lock);
        while (!GiveInfoQueue.Queue.empty())
        {
            shared_str Info = GiveInfoQueue.Queue.front();
            GiveInfoQueue.Queue.pop();
            GiveInfo(Info);
        }
    }
    {
        xrCriticalSectionGuard guard(DisableInfoQueue.Lock);
        while (!DisableInfoQueue.Queue.empty())
        {
            shared_str Info = DisableInfoQueue.Queue.front();
            DisableInfoQueue.Queue.pop();
            DisableInfo(Info);
        }
    }
    {
        xrCriticalSectionGuard guard(FunctorQueue.Lock);
        while (!FunctorQueue.Queue.empty())
        {
            shared_str Func = FunctorQueue.Queue.front();
            FunctorQueue.Queue.pop();
            ProcessFunctor(Func);
        }
    }
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
}

void CAnimNotifyHandler::ProcessNotify(IAnimNotifyMessage* Message)
{
    CAnimNotifyRegistry::GetInstance().get(Message->notify)->Execute(Message->render_visual, Message->bone_id);
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
    }
    return nullptr;
}
