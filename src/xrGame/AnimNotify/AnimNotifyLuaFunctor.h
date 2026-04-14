#pragma once
#include "../xrCore/AnimNotify/AnimNotifyTypes.h"

class CAnimNotifyLuaFunctor : public IAnimNotify
{
    shared_str Func;
    LPCSTR sect;
public:
    void Construct(const CInifile& ini, LPCSTR sect) override;
    void Execute(IRenderVisual* visual, u16 bone_id) override;
};
