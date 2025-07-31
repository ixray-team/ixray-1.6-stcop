#pragma once
#include "AnimNotifyTypes.h"

class IRenderVisual;

class XRCORE_API IAnimNotifyHandler
{
public:
    virtual ~IAnimNotifyHandler() = default;
    virtual void TriggerNotify(IAnimNotifyMessage* notify) = 0;

    virtual void Update() = 0;

    static void SetHandler(IAnimNotifyHandler* pHandler);
    static bool IsValid(){return Handler != nullptr;}
    static IAnimNotifyHandler& Get();

    virtual IAnimNotify* ConstructNotify(const EAnimNotifyType type) = 0;
    
private:
    static IAnimNotifyHandler* Handler;
};


