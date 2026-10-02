// CBlender_Compile_combined.cpp
#include "stdafx.h"

#include "ResourceManager.h"
#include "blenders/Blender_Recorder.h"
#include "blenders/Blender.h"
#include "dxRenderDeviceRender.h"

#include "tss.h"
#include "../../xrEngine/EngineAPI.h"

void fix_texture_name(LPSTR fn);

void CBlender_Compile::i_Address(u32 s, u32 address)
{
    if (s == u32(-1))
    {
        Msg("i_Address: invalid sampler index");
        return;
    }
    RS.SetSAMP(s, D3DSAMP_ADDRESSU, address);
    RS.SetSAMP(s, D3DSAMP_ADDRESSV, address);
    RS.SetSAMP(s, D3DSAMP_ADDRESSW, address);
}

void CBlender_Compile::i_BorderColor(u32 s, u32 color)
{
    if (s == u32(-1))
    {
        Msg("i_BorderColor: invalid sampler index");
        return;
    }
    RS.SetSAMP(s, D3DSAMP_BORDERCOLOR, color);
}

void CBlender_Compile::i_Filter_Min(u32 s, u32 f)
{
    VERIFY(s != u32(-1));
    RS.SetSAMP(s, D3DSAMP_MINFILTER, f);
}

void CBlender_Compile::i_Filter_Mip(u32 s, u32 f)
{
    VERIFY(s != u32(-1));
    RS.SetSAMP(s, D3DSAMP_MIPFILTER, f);
}

void CBlender_Compile::i_Filter_Mag(u32 s, u32 f)
{
    VERIFY(s != u32(-1));
    RS.SetSAMP(s, D3DSAMP_MAGFILTER, f);
}

void CBlender_Compile::i_FilterAnizo(u32 s, bool value)
{
    VERIFY(s != u32(-1));
    RS.SetSAMP(s, XRDX10SAMP_ANISOTROPICFILTER, value);
}

void CBlender_Compile::i_Filter(u32 s, u32 _min, u32 _mip, u32 _mag)
{
    VERIFY(s != u32(-1));
    i_Filter_Min(s, _min);
    i_Filter_Mip(s, _mip);
    i_Filter_Mag(s, _mag);
}

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

u32 CBlender_Compile::r_dx10Sampler(const char* ResourceName)
{
    VERIFY(ResourceName);
    string256 name;
    xr_strcpy(name, ResourceName);
    fix_texture_name(name);

    ref_constant C = ctable.get(name);
    if (!C) return u32(-1);
    R_ASSERT(C->type == RC_sampler);
    u32 stage = C->samp.index;

    if (0 == xr_strcmp(ResourceName, "smp_nofilter"))
    {
        i_Address(stage, D3DTADDRESS_CLAMP);
        i_Filter(stage, D3DTEXF_POINT, D3DTEXF_NONE, D3DTEXF_POINT);
    }

    if (0 == xr_strcmp(ResourceName, "smp_rtlinear"))
    {
        i_Address(stage, D3DTADDRESS_CLAMP);
        i_Filter(stage, D3DTEXF_LINEAR, D3DTEXF_NONE, D3DTEXF_LINEAR);
    }

    if (0 == xr_strcmp(ResourceName, "smp_linear"))
    {
        i_Address(stage, D3DTADDRESS_WRAP);
        i_Filter(stage, D3DTEXF_LINEAR, D3DTEXF_LINEAR, D3DTEXF_LINEAR);
    }

    if (0 == xr_strcmp(ResourceName, "smp_base"))
    {
        i_Address(stage, D3DTADDRESS_WRAP);
        i_FilterAnizo(stage, true);
    }

    if (0 == xr_strcmp(ResourceName, "smp_material"))
    {
        i_Address(stage, D3DTADDRESS_CLAMP);
        i_Filter(stage, D3DTEXF_LINEAR, D3DTEXF_NONE, D3DTEXF_LINEAR);
        RS.SetSAMP(stage, D3DSAMP_ADDRESSW, D3DTADDRESS_WRAP);
    }

    if (0 == xr_strcmp(ResourceName, "smp_smap"))
    {
        i_Address(stage, D3DTADDRESS_CLAMP);
        i_Filter(stage, D3DTEXF_LINEAR, D3DTEXF_NONE, D3DTEXF_LINEAR);
        RS.SetSAMP(stage, XRDX10SAMP_COMPARISONFILTER, true);
        RS.SetSAMP(stage, XRDX10SAMP_COMPARISONFUNC, RHI_COMPARISON_LESS_EQUAL);
    }

    if (0 == xr_strcmp(ResourceName, "smp_jitter"))
    {
        i_Address(stage, D3DTADDRESS_WRAP);
        i_Filter(stage, D3DTEXF_POINT, D3DTEXF_NONE, D3DTEXF_POINT);
    }

    return stage;
}

void CBlender_Compile::r_dx10Texture(const char* ResourceName, const char* texture)
{
    VERIFY(ResourceName);
    if (!texture) return;
    string256 TexName;
    xr_strcpy(TexName, texture);
    fix_texture_name(TexName);

    ref_constant C = ctable.get(ResourceName);
    if (!C) return;

    R_ASSERT(C->type == RC_dx10texture);
    u32 stage = C->samp.index;
    passTextures.push_back(std::make_pair(stage, ref_texture(DEV->_CreateTexture(TexName))));
}

void CBlender_Compile::r_Constant(const char* name, RHIShaderConstant::Setup* s)
{
    R_ASSERT(s);
    ref_constant C = ctable.get(name);
    if (C) C->handler = s;
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

void CBlender_Compile::r_Pass(const char* _vs, const char* _gs, const char* _ps, bool bFog, bool bZtest, bool bZwrite, bool bABlend, D3DBLEND abSRC, D3DBLEND abDST, bool aTest, u32 aRef)
{
    RS.Invalidate();
    ctable.clear();
    passTextures.clear();
    passMatrices.clear();
    passConstants.clear();
    dwStage = 0;

    PassSET_ZB(bZtest, bZwrite);
    PassSET_Blend(bABlend, abSRC, abDST, aTest, aRef);
    PassSET_LightFog(false, bFog);

    if (LightingModeIsStatic() && aTest)
        RImplementation.addShaderOption("USE_R1_ALPHA_TEST", "1");

    SPS* ps = DEV->_CreatePS(_ps);
    SVS* vs = DEV->_CreateVS(_vs);
    dest.ps = ps;
    dest.vs = vs;
    ctable.merge(&ps->constants);
    ctable.merge(&vs->constants);

    SGS* gs = DEV->_CreateGS(_gs);
    dest.gs = gs;

    dest.hs = DEV->_CreateHS("null");
    dest.ds = DEV->_CreateDS("null");
    dest.cs = DEV->_CreateCS("null");
    if (gs) ctable.merge(&gs->constants);

    if (0 == _stricmp(_ps, "null"))
    {
        RS.SetTSS(0, D3DTSS_COLOROP, D3DTOP_DISABLE);
        RS.SetTSS(0, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
    }

    SetPassPriority(-1);
}

void CBlender_Compile::r_TessPass(const char* vs, const char* hs, const char* ds, const char* gs, const char* ps, bool bFog, bool bZtest, bool bZwrite, bool bABlend, D3DBLEND abSRC, D3DBLEND abDST, bool aTest, u32 aRef)
{
    // Reuse r_Pass to create base shaders then overwrite HS/DS and merge their consts.
    r_Pass(vs, gs, ps, bFog, bZtest, bZwrite, bABlend, abSRC, abDST, aTest, aRef);

    dest.hs = DEV->_CreateHS(hs);
    dest.ds = DEV->_CreateDS(ds);

    ctable.merge(&dest.hs->constants);
    ctable.merge(&dest.ds->constants);
}

void CBlender_Compile::r_ComputePass(const char* cs)
{
    ctable.clear();
    dest.cs = DEV->_CreateCS(cs);
    ctable.merge(&dest.cs->constants);
}

void CBlender_Compile::r_End(bool clear)
{
    SetMapping();
    dest.constants = DEV->_CreateConstantTable(ctable);
    dest.state = DEV->_CreateState(RS.GetContainer());
    dest.T = DEV->_CreateTextureList(passTextures);
	
    dest.C = nullptr;
    ref_matrix_list temp(nullptr);
	
#ifdef _EDITOR
    dest.M = nullptr;
#endif

    SH->passes.push_back(DEV->_CreatePass(dest));

    if (clear)
    {
        RImplementation.clearAllShaderOptions();
    }

    SetPassPriority(-1);
}

void CBlender_Compile::SetPassPriority(int iPriority)
{
    dest.iPriority = u8(iPriority);
}