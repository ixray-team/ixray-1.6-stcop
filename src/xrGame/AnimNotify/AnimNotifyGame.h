#pragma once
#include <../../xrCore/AnimNotify/AnimNotify.h>

class CAnimNotifyHandler : public IAnimNotifyHandler
{
public:
    void TriggerGiveInfo(shared_str Info) override;
    void TriggerDisableInfo(shared_str Info) override;
    void TriggerFunctor(shared_str Func) override;
    void TriggerNotify(shared_str Name) override;

    void Update() override;

private:
    struct LockedQueue
    {
        xrCriticalSection Lock;
        xr_queue<shared_str> Queue;
    };
    LockedQueue GiveInfoQueue;
    LockedQueue DisableInfoQueue;
    LockedQueue FunctorQueue;
    LockedQueue NotifyQueue;

    void GiveInfo(shared_str Info);
    void DisableInfo(shared_str Info);
    void ProcessFunctor(shared_str Func);
    void ProcessNotify(shared_str Name);
};
