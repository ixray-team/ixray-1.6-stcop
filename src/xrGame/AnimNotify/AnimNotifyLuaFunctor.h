#pragma once
#include "../xrCore/AnimNotify/AnimNotifyTypes.h"

class CAnimNotifyLuaFunctor : public IAnimNotify
{
    shared_str Func;
    const char* sect;
public:
    void Construct(const CInifile& ini, const char* sect) override;
    void Execute(IRenderVisual* visual, u16 bone_id) override;
};
