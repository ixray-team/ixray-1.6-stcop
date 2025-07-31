#pragma once

class XRCORE_API IAnimNotifyHandler
{
public:
    virtual ~IAnimNotifyHandler() = default;
    virtual void TriggerGiveInfo(shared_str Info) = 0;
    virtual void TriggerDisableInfo(shared_str Info) = 0;
    virtual void TriggerFunctor(shared_str Func) = 0;
    virtual void TriggerNotify(shared_str Name) = 0;

    virtual void Update() = 0;

    static void SetHandler(IAnimNotifyHandler* pHandler);
    static bool IsValid(){return Handler != nullptr;}
    static IAnimNotifyHandler& Get();
    
private:
    static IAnimNotifyHandler* Handler;
};


