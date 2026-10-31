// CBlender_Compile_combined.cpp
#include "stdafx.h"

#include "ResourceManager.h"
#include "blenders/Blender_Recorder.h"
#include "blenders/Blender.h"
#include "dxRenderDeviceRender.h"
#include "SH_Texture.h"
#include "ShaderBindings.h"

#include "tss.h"
#include "../../xrEngine/EngineAPI.h"

void fix_texture_name(LPSTR fn);

// Provide DX9-style wrappers that call the same implementations

void CBlender_Compile::r_Stencil(bool Enable, u32 Func, u32 Mask, u32 WriteMask, u32 Fail, u32 Pass, u32 ZFail)
{
    RS.SetRS(D3DRS_STENCILENABLE, BC(Enable));
    if (!Enable)
    {
        return;
    }

    RS.SetRS(D3DRS_STENCILFUNC, Func);
    RS.SetRS(D3DRS_STENCILMASK, Mask);
    RS.SetRS(D3DRS_STENCILWRITEMASK, WriteMask);
    RS.SetRS(D3DRS_STENCILFAIL, Fail);
    RS.SetRS(D3DRS_STENCILPASS, Pass);
    RS.SetRS(D3DRS_STENCILZFAIL, ZFail);

    RS.SetRS(D3DRS_CCW_STENCILFUNC, Func);
    RS.SetRS(D3DRS_CCW_STENCILFAIL, Fail);
    RS.SetRS(D3DRS_CCW_STENCILPASS, Pass);
    RS.SetRS(D3DRS_CCW_STENCILZFAIL, ZFail);
}

void CBlender_Compile::r_StencilRef(u32 Ref)
{
    RS.SetRS(D3DRS_STENCILREF, Ref);
}

void CBlender_Compile::r_CullMode(D3DCULL Mode)
{
    RS.SetRS(D3DRS_CULLMODE, (u32)Mode);
}

void CBlender_Compile::CreatePassShaders()
{
    if (!shaders_pending)
        return;
    shaders_pending = false;

    if (pass_compute)
    {
        dest.cs = DEV->_CreateCS(pass_cs, Defines);
        return;
    }
    dest.ps = DEV->_CreatePS(pass_ps, Defines);
    dest.vs = DEV->_CreateVS(pass_vs, Defines);
    dest.gs = DEV->_CreateGS(pass_gs, Defines);
    dest.hs = DEV->_CreateHS(pass_hs, Defines);
    dest.ds = DEV->_CreateDS(pass_ds, Defines);
    dest.cs = DEV->_CreateCS("null");
}

void CBlender_Compile::r_dx10Texture(const char* ResourceName, const char* texture)
{
    VERIFY(ResourceName);
    const u32 Slot = ShaderBindings::Texture(ResourceName);
    if (!texture || Slot == u32(-1))
        return;
    string256 TexName;
    xr_strcpy(TexName, texture);
    fix_texture_name(TexName);

    const u32 stage = (pass_compute ? CTexture::rstCompute : CTexture::rstPixel) + Slot;
    passTextures.erase(std::remove_if(passTextures.begin(), passTextures.end(), [stage](const auto& it) { return it.first == stage; }), passTextures.end());
    passTextures.push_back(std::make_pair(stage, ref_texture(DEV->_CreateTexture(TexName))));
}

void CBlender_Compile::r_dx10Unbind(const char* ResourceName)
{
    const u32 stage = (pass_compute ? CTexture::rstCompute : CTexture::rstPixel) + ShaderBindings::Texture(ResourceName);
    passTextures.erase(std::remove_if(passTextures.begin(), passTextures.end(), [stage](const auto& it) { return it.first == stage; }), passTextures.end());
}

void CBlender_Compile::r_ColorWriteEnable(bool cR, bool cG, bool cB, bool cA)
{
    BYTE Mask = 0;
    Mask |= cR ? D3DCOLORWRITEENABLE_RED : 0;
    Mask |= cG ? D3DCOLORWRITEENABLE_GREEN : 0;
    Mask |= cB ? D3DCOLORWRITEENABLE_BLUE : 0;
    Mask |= cA ? D3DCOLORWRITEENABLE_ALPHA : 0;

    RS.SetRS(D3DRS_COLORWRITEENABLE, Mask);
    RS.SetRS(D3DRS_COLORWRITEENABLE1, Mask);
    RS.SetRS(D3DRS_COLORWRITEENABLE2, Mask);
    RS.SetRS(D3DRS_COLORWRITEENABLE3, Mask);
}

void CBlender_Compile::r_Pass(const char* _vs, const char* _ps, bool bFog, bool bZtest, bool bZwrite, bool bABlend, D3DBLEND abSRC, D3DBLEND abDST, bool aTest, u32 aRef)
{
    r_Pass(_vs, "null", _ps, bFog, bZtest, bZwrite, bABlend, abSRC, abDST, aTest, aRef);
}

void CBlender_Compile::r_Pass(const char* _vs, const char* _gs, const char* _ps, bool, bool bZtest, bool bZwrite, bool bABlend, D3DBLEND abSRC, D3DBLEND abDST, bool aTest, u32 aRef)
{
    RS.Invalidate();
    ctable.clear();
    passTextures.clear();
    passMatrices.clear();
    pass_compute = false;

    PassSET_ZB(bZtest, bZwrite);
    PassSET_Blend(bABlend, abSRC, abDST, aTest, aRef);

    if (LightingModeIsStaticRender() && aTest)
        AddDefine("USE_R1_ALPHA_TEST", "1");

    xr_strcpy(pass_vs, _vs ? _vs : "null");
    xr_strcpy(pass_ps, _ps ? _ps : "null");
    xr_strcpy(pass_gs, _gs ? _gs : "null");
    xr_strcpy(pass_hs, "null");
    xr_strcpy(pass_ds, "null");
    xr_strcpy(pass_cs, "null");
    shaders_pending = true;

    SetPassPriority(-1);
}

void CBlender_Compile::r_TessPass(const char* vs, const char* hs, const char* ds, const char* gs, const char* ps, bool bFog, bool bZtest, bool bZwrite, bool bABlend, D3DBLEND abSRC, D3DBLEND abDST, bool aTest, u32 aRef)
{
    // Reuse r_Pass to create base shaders then overwrite HS/DS and merge their consts.
    r_Pass(vs, gs, ps, bFog, bZtest, bZwrite, bABlend, abSRC, abDST, aTest, aRef);
    xr_strcpy(pass_hs, hs ? hs : "null");
    xr_strcpy(pass_ds, ds ? ds : "null");
}

void CBlender_Compile::r_ComputePass(const char* cs)
{
    ctable.clear();
    passTextures.clear();
    pass_compute = true;
    xr_strcpy(pass_cs, cs ? cs : "null");
    shaders_pending = true;
}

void CBlender_Compile::r_End(bool clear)
{
    CreatePassShaders();
    ctable.DetailScale = detail_scaler;
    dest.constants = DEV->_CreateConstantTable(ctable);
    dest.state = DEV->_CreateState(RS.GetContainer());
    dest.T = DEV->_CreateTextureList(passTextures);
    dest.M = DEV->_CreateMatrixList(passMatrices);

    SH->passes.push_back(DEV->_CreatePass(dest));

    if (clear)
    {
        Defines.clear();
    }

    SetPassPriority(-1);
}

void CBlender_Compile::SetPassPriority(int iPriority)
{
    dest.iPriority = u8(iPriority);
}