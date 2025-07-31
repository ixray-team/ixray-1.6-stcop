#pragma once
#include <../../xrCore/AnimNotify/AnimNotify.h>

class CAnimNotifyHandler : public IAnimNotifyHandler
{
public:
    void TriggerNotify(IAnimNotifyMessage* notify) override;

    void Update() override;

private:
    struct LockedQueue
    {
        xrCriticalSection Lock;
        xr_queue<IAnimNotifyMessage*> Queue;
    };
    LockedQueue NotifyQueue;

    void ProcessNotify(IAnimNotifyMessage* Message);

public:
    IAnimNotify* ConstructNotify(const EAnimNotifyType type) override;
};
