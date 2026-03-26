#include "stdafx.h"
#include "r4_rendertarget.h"

void CRenderTarget::phase_bake_tonemap_lut()
{
    ShaderElement* S = (&*(s_tonemap_lut_bake->E[0]));
    SPass& P = *(S->passes[0]);

    RCache.set_States(P.state);
    RCache.set_Constants(P.constants);
    RCache.set_Textures(P.T);
    RCache.set_CS(P.cs);

    // параметры для bake
    RCache.set_c("tm_lut_params", 32.0f, 1.0f / 32.0f, 16.0f, 0.0f);
    RCache.set_c("autoexposure_params", ps_r2_autoexposure_key, ps_r2_autoexposure_min, ps_r2_autoexposure_max, ps_r2_autoexposure_bias);

    UINT UAVInitialCounts = 1;
    ID3D11UnorderedAccessView* nullUAV[1] = { nullptr };
    ID3D11ShaderResourceView* nullSRV[16] = { nullptr };

    ID3D11RenderTargetView* oldRTV[8] = { nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr };
    ID3D11DepthStencilView* oldDSV = nullptr;
    ID3D11RenderTargetView* nullRTV[8] = { nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr };

    RContext->OMGetRenderTargets(8, oldRTV, &oldDSV);
    RContext->OMSetRenderTargets(8, nullRTV, nullptr);

    RContext->CSSetUnorderedAccessViews(0, 1, &u_tonemap_lut_3d, &UAVInitialCounts);

    // 32x32x32 volume with [numthreads(8,8,8)] => 4x4x4 groups
    RCache.Compute(4, 4, 4);

    RContext->CSSetUnorderedAccessViews(0, 1, nullUAV, &UAVInitialCounts);
    RContext->CSSetShaderResources(0, 16, nullSRV);
    RContext->OMSetRenderTargets(8, oldRTV, oldDSV);

    for (u32 i = 0; i < 8; ++i)
        _RELEASE(oldRTV[i]);
    _RELEASE(oldDSV);
}