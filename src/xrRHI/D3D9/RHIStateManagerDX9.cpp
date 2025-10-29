#include "Device.h"
#include "RHIStateManagerDX9.h"

#define D3DCOLORWRITEENABLE_ALL \
    (D3DCOLORWRITEENABLE_RED | D3DCOLORWRITEENABLE_GREEN | D3DCOLORWRITEENABLE_BLUE | D3DCOLORWRITEENABLE_ALPHA)

RHIStateManagerDX9::RHIStateManagerDX9()
{
    Device = static_cast<IDirect3DDevice9*>(GRHI->DevicePtr->RawDevice);
}

RHIStateManagerDX9::~RHIStateManagerDX9()
{
}

void RHIStateManagerDX9::Apply()
{
    // DX9 applies states immediately, no need to cache and apply later
}

void RHIStateManagerDX9::EnableScissoring(bool Enable)
{
    Device->SetRenderState(D3DRS_SCISSORTESTENABLE, Enable);
}

void RHIStateManagerDX9::SetStencil(u32 Enable, u32 Func, u32 Ref, u32 Mask, u32 WriteMask, u32 Fail, u32 Pass, u32 ZFail)
{
    Device->SetRenderState(D3DRS_STENCILENABLE, Enable);
    if (Enable)
    {
        Device->SetRenderState(D3DRS_STENCILFUNC, Func);
        Device->SetRenderState(D3DRS_STENCILREF, Ref);
        Device->SetRenderState(D3DRS_STENCILMASK, Mask);
        Device->SetRenderState(D3DRS_STENCILWRITEMASK, WriteMask);
        Device->SetRenderState(D3DRS_STENCILFAIL, Fail);
        Device->SetRenderState(D3DRS_STENCILPASS, Pass);
        Device->SetRenderState(D3DRS_STENCILZFAIL, ZFail);
    }
}

void RHIStateManagerDX9::SetDepthEnable(u32 Enable)
{
    Device->SetRenderState(D3DRS_ZENABLE, Enable);
}

void RHIStateManagerDX9::SetDepthFunc(u32 Func)
{
    Device->SetRenderState(D3DRS_ZFUNC, Func);
}

void RHIStateManagerDX9::SetColorWriteEnable(u32 Mask)
{
    Device->SetRenderState(D3DRS_COLORWRITEENABLE, Mask);
    Device->SetRenderState(D3DRS_COLORWRITEENABLE1, Mask);
    Device->SetRenderState(D3DRS_COLORWRITEENABLE2, Mask);
    Device->SetRenderState(D3DRS_COLORWRITEENABLE3, Mask);
}

void RHIStateManagerDX9::SetCullMode(u32 Mode)
{
    CacheCullMode = Mode;
    Device->SetRenderState(D3DRS_CULLMODE, Mode);
}

void RHIStateManagerDX9::SetAlphaRef(u32 mode)
{
    Device->SetRenderState(D3DRS_ALPHAREF, mode);
}

void RHIStateManagerDX9::UnmapConstants()
{
    // DX9 doesn't use constant buffers
}