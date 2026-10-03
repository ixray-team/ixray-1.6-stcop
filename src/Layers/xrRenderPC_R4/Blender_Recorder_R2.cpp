// CBlender_Compile_combined.cpp
#include "stdafx.h"

#include "ResourceManager.h"
#include "blenders/Blender_Recorder.h"
#include "blenders/Blender.h"
#include "dxRenderDeviceRender.h"
#include "ShaderBind.h"
#include "dx10FixedConstants.h"
#include "SH_Texture.h"

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

const CBlender_Compile::PassBind* CBlender_Compile::FindBind(const char* name, char space) const
{
    for (u32 i = 0; i < pass_bind_count; ++i)
    {
        if (pass_binds[i].space == space && !xr_strcmp(pass_binds[i].name, name))
            return &pass_binds[i];
    }
    return nullptr;
}

void CBlender_Compile::AddBind(const char* name, u32 slot, char space)
{
    PassBind& bind = pass_binds[pass_bind_count++];
    xr_strcpy(bind.name, name);
    bind.slot = slot;
    bind.space = space;
}

void CBlender_Compile::CreatePassShaders()
{
    if (!shaders_pending)
        return;
    shaders_pending = false;

    ShaderBindSlot slots[32] = {};
    for (u32 i = 0; i < pass_bind_count; ++i)
    {
        xr_strcpy(slots[i].name, pass_binds[i].name);
        slots[i].slot = pass_binds[i].slot;
        slots[i].space = pass_binds[i].space;
    }

    ShaderBind_Set(slots, pass_compute || _stricmp(pass_ps, "null") ? pass_bind_count : 0);
    if (pass_compute)
        dest.cs = DEV->_CreateCS(pass_cs);
    else
        dest.ps = DEV->_CreatePS(pass_ps);
    ShaderBind_Clear();

    if (pass_compute)
        return;
    dest.vs = DEV->_CreateVS(pass_vs);
    dest.gs = DEV->_CreateGS(pass_gs);
    dest.hs = DEV->_CreateHS(pass_hs);
    dest.ds = DEV->_CreateDS(pass_ds);
    dest.cs = DEV->_CreateCS("null");
}

u32 CBlender_Compile::r_dx10Sampler(const char* ResourceName)
{
    VERIFY(ResourceName);
    string256 name;
    xr_strcpy(name, ResourceName);
    fix_texture_name(name);

    const u32 base = pass_compute ? CTexture::rstCompute : CTexture::rstPixel;
    if (const PassBind* bound = FindBind(name, 's'))
        return base + bound->slot;
    if (next_samp >= 16)
    {
        Msg("! r_dx10Sampler: no free sampler slot for %s", name);
        return u32(-1);
    }
    const u32 stage = base + next_samp;
    AddBind(name, next_samp++, 's');

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

void CBlender_Compile::r_dx10Texture(const char* ResourceName, const char* texture, u32 slot)
{
    VERIFY(ResourceName);
    if (!texture) return;
    string256 TexName;
    xr_strcpy(TexName, texture);
    fix_texture_name(TexName);

    if (const PassBind* bound = FindBind(ResourceName, 't'))
        slot = bound->slot;
    else
    {
        if (slot == u32(-1))
            slot = next_tex;
        if (slot >= 16)
        {
            Msg("! r_dx10Texture: no free texture slot for %s", ResourceName);
            return;
        }
        next_tex = std::max(next_tex, slot + 1);
        AddBind(ResourceName, slot, 't');
    }

    const u32 stage = (pass_compute ? CTexture::rstCompute : CTexture::rstPixel) + slot;
    if (!xr_strcmp(ResourceName, "s_base"))
    {
        ref_constant C = ctable.get(ResourceName);
        RHIShaderConstant* sampler = C ? &*C : AddConstant(ResourceName);
        sampler->destination = RC_dest_sampler;
        sampler->type = RC_dx10texture;
        sampler->samp.index = u16(stage);
        sampler->samp.cls = RC_dx10texture;
    }
    passTextures.erase(std::remove_if(passTextures.begin(), passTextures.end(), [stage](const auto& it) { return it.first == stage; }), passTextures.end());
    passTextures.push_back(std::make_pair(stage, ref_texture(DEV->_CreateTexture(TexName))));
}

void CBlender_Compile::r_dx10Unbind(const char* ResourceName)
{
    const PassBind* bound = FindBind(ResourceName, 't');
    if (!bound)
        return;

    const u32 gone = bound->slot;
    const u32 base = pass_compute ? CTexture::rstCompute : CTexture::rstPixel;
    passTextures.erase(std::remove_if(passTextures.begin(), passTextures.end(), [&](const auto& it) { return it.first == base + gone; }), passTextures.end());
    for (auto& it : passTextures)
    {
        if (it.first > base + gone && it.first < base + 16)
            --it.first;
    }

    PassBind* const first = pass_binds;
    std::move(first + (bound - first) + 1, first + pass_bind_count, first + (bound - first));
    --pass_bind_count;
    for (u32 i = 0; i < pass_bind_count; ++i)
    {
        if (pass_binds[i].space == 't' && pass_binds[i].slot > gone)
            --pass_binds[i].slot;
    }
    --next_tex;

    const PassBind* sbase = FindBind("s_base", 't');
    ref_constant C = sbase ? ctable.get("s_base") : nullptr;
    if (C)
        C->samp.index = u16(base + sbase->slot);
}

RHIShaderConstant* CBlender_Compile::AddConstant(const char* name)
{
    ref_constant C = new RHIShaderConstant();
    C->name = name;
    C->name_hash = FixedConstants::NameHash(name);
    ctable.table.push_back(C);
    std::sort(ctable.table.begin(), ctable.table.end(), [](const ref_constant& a, const ref_constant& b) { return xr_strcmp(a->name, b->name) < 0; });
    ctable.handlers_valid = false;
    return &*C;
}

void CBlender_Compile::r_Setup(const char* name, RHIShaderConstant::Setup* s)
{
    AddConstant(name)->handler = s;
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
    pass_bind_count = 0;
    next_tex = 0;
    next_samp = 0;
    pass_compute = false;

    PassSET_ZB(bZtest, bZwrite);
    PassSET_Blend(bABlend, abSRC, abDST, aTest, aRef);
    PassSET_LightFog(false, bFog);

    if (LightingModeIsStatic() && aTest)
        RImplementation.addShaderOption("USE_R1_ALPHA_TEST", "1");

    xr_strcpy(pass_vs, _vs ? _vs : "null");
    xr_strcpy(pass_ps, _ps ? _ps : "null");
    xr_strcpy(pass_gs, _gs ? _gs : "null");
    xr_strcpy(pass_hs, "null");
    xr_strcpy(pass_ds, "null");
    xr_strcpy(pass_cs, "null");
    shaders_pending = true;

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
    xr_strcpy(pass_hs, hs ? hs : "null");
    xr_strcpy(pass_ds, ds ? ds : "null");
}

void CBlender_Compile::r_ComputePass(const char* cs)
{
    ctable.clear();
    passTextures.clear();
    pass_bind_count = 0;
    next_tex = 0;
    next_samp = 0;
    pass_compute = true;
    xr_strcpy(pass_cs, cs ? cs : "null");
    shaders_pending = true;
}

void CBlender_Compile::r_End(bool clear)
{
    CreatePassShaders();
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