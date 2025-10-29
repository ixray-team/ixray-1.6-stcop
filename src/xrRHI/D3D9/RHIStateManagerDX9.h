#pragma once
#include "../RHI.h"

class RHIStateManagerDX9 :
    public IRHIStateManager
{
public:
    RHIStateManagerDX9();
    ~RHIStateManagerDX9();

    void Apply() override;
    void EnableScissoring(bool enable = true) override;
    void SetStencil(u32 enable, u32 func, u32 ref, u32 mask, u32 writemask, u32 fail, u32 pass, u32 zfail) override;
    void SetDepthEnable(u32 enable) override;
    void SetDepthFunc(u32 func) override;
    void SetColorWriteEnable(u32 mask) override;
    void SetCullMode(u32 mode) override;
    void UnmapConstants() override;
    void Reset() override {};
private:
    IDirect3DDevice9* Device;
};