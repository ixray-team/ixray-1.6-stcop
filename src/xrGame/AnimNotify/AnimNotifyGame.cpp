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
#include "Level.h"
#include "../xrCore/AnimNotify/AnimNotifyRegistry.h"

void CAnimNotifyHandler::TriggerGiveInfo(shared_str Info)
{
    xrCriticalSectionGuard guard(GiveInfoQueue.Lock);
    GiveInfoQueue.Queue.push(Info);
}

void CAnimNotifyHandler::TriggerDisableInfo(shared_str Info)
{
    xrCriticalSectionGuard guard(DisableInfoQueue.Lock);
    DisableInfoQueue.Queue.push(Info);
}

void CAnimNotifyHandler::TriggerFunctor(shared_str Func)
{
    xrCriticalSectionGuard guard(FunctorQueue.Lock);
    FunctorQueue.Queue.push(Func);
}

void CAnimNotifyHandler::TriggerNotify(shared_str Name)
{
    xr_string buffer = Name.c_str();
    std::ranges::transform(buffer, buffer.begin(), tolower);
    Name = buffer.c_str();
    xrCriticalSectionGuard guard(NotifyQueue.Lock);
    NotifyQueue.Queue.push(Name);
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
            shared_str Name = NotifyQueue.Queue.front();
            NotifyQueue.Queue.pop();
            ProcessNotify(Name);
        }
    }
}

void CAnimNotifyHandler::GiveInfo(shared_str Info)
{
    KNOWN_INFO_VECTOR *known_info = ai().get_alife()->registry(info_portions).object(0, true);
    VERIFY(known_info);
    if (std::find_if(known_info->begin(), known_info->end(), CFindByIDPred(Info)) == known_info->end())
    {
        known_info->push_back(Info);
    }
}

void CAnimNotifyHandler::DisableInfo(shared_str Info)
{
    KNOWN_INFO_VECTOR *known_info = ai().get_alife()->registry(info_portions).object(0, true);
    VERIFY(known_info);
    if (auto It = std::find_if(known_info->begin(), known_info->end(), CFindByIDPred(Info));
        It != known_info->end())
    {
        known_info->erase(It);
    }
}

void CAnimNotifyHandler::ProcessFunctor(shared_str Func)
{
    try
    {
        luabind::functor<void> funct;
        if (ai().script_engine().functor(Func.c_str(), funct))
        {
            funct();
        }
    } catch (...)
    {
        R_ASSERT3(false, "Unable to process AnimNotify functor", Func.c_str());
    }
}

void CAnimNotifyHandler::ProcessNotify(shared_str Name)
{
    auto& registry = CAnimNotifyRegistry::GetInstance();
    if (!registry.contains(Name)) // BROKEN, letters in registry only in lowercase
    {
        R_ASSERT3(false, "Unable to process AnimNotify", Name.c_str());
        return;
    }
    const auto& Sect = registry.get(Name);
    if (Sect.GiveInfo.size())
    {
        GiveInfo(Sect.GiveInfo);
    }
    if (Sect.DisableInfo.size())
    {
        DisableInfo(Sect.DisableInfo);
    }
    if (Sect.Functor.size())
    {
        ProcessFunctor(Sect.Functor);
    }
}
