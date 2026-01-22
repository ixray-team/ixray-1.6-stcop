#pragma once
#include <../../xrCore/AnimNotify/AnimNotify.h>

class CAnimNotifyHandler : public IAnimNotifyHandler
{
public:
    void TriggerNotify(IAnimNotifyMessage&& notify) override;

    void Update() override;

private:
    xrCriticalSection cs;
    xr_vector<IAnimNotifyMessage> m_msgs;

public:
    IAnimNotify* ConstructNotify(const EAnimNotifyType type) override;
};
