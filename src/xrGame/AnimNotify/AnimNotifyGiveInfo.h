#pragma once
#include "AnimNotify/AnimNotifyTypes.h"

class CAnimNotifyGiveInfo : public IAnimNotify
{
    shared_str Info;
public:
    void Construct(const CInifile& ini, LPCSTR sect) override;
    void Execute(IRenderVisual* visual, u16 bone_id) override;
};
