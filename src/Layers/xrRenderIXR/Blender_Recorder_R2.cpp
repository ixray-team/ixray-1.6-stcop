// CBlender_Compile_combined.cpp
#include "stdafx.h"

#include "ResourceManager.h"
#include "blenders/Blender_Recorder.h"
#include "blenders/Blender.h"
#include "dxRenderDeviceRender.h"
#include "SH_Texture.h"
#include "ShaderBindings.h"
#include "MaterialFeatures.h"
#include "EditorSurface.h"

#include "tss.h"
#include "xrRender_console.h"
#include "../../xrEngine/EngineAPI.h"

void fix_texture_name(char* fn);

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
    dest.Features = MaterialFeature::FromDefines(Defines);

    // What kind of vertex and baked light a surface of the forward level frame has is told by its shader's name
    const bool IsVertexLit = !strncmp(pass_vs, "vert", 4);
    const bool IsSwaying = !strncmp(pass_vs, "tree_w", 6);
    const bool IsStill = !strncmp(pass_vs, "tree_s", 6);
    const bool IsUnlit = !xr_strcmp(pass_ps, "r1_selflight");

    if (!pass_compute)
    {
        dest.Features |= (IsVertexLit ? MaterialFeature::VertexLit : 0u) | (IsSwaying || IsStill ? MaterialFeature::Placed : 0u) | (IsStill ? MaterialFeature::Still : 0u) | (IsUnlit ? MaterialFeature::Unlit : 0u);
    }

    // A cluster of that frame reads all of it, and what its material's maps are, from the material's record
    // (SURFACE_FOLDED of common\surface_forward.hlsli): the level's materials and the trees' are compiled once, as
    // lightmapped level geometry, and share a draw. The terrain is one material and stays as compiled, and so does
    // what mirrors a cube: its shaders read the cube by the record too
    if (IsCluster && !pass_compute && (IsVertexLit || IsSwaying || IsStill || !strncmp(pass_vs, "lmap", 4)) && xr_strcmp(pass_vs, "lmapE"))
    {
        Defines.erase(xr_string("USE_PBR"));
        Defines.erase(xr_string("USE_STEEPPARALLAX"));
        Defines.erase(xr_string("USE_TDETAIL_BUMP"));
        Defines.erase(xr_string("SURFACE_LEVEL"));
        const char* Name = strstr(pass_vs, "_bump") ? "lmap_bump" : "lmap";
        xr_strcpy(pass_vs, Name);

        if (xr_strcmp(pass_ps, "null"))
        {
            xr_strcpy(pass_ps, Name);
        }
    }

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
    u8 Mask = 0;
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

// The model shader of the features a level shader is named for: its mirrored cube, its detail, its bump
static const char* ModelShaderName(const char* Level, string64& Model)
{
    if (!Level || !xr_strcmp(Level, "null"))
    {
        return Level;
    }

    xr_strconcat(Model, !strncmp(Level, "lmapE", 5) ? "model_env_hq" : strstr(Level, "_dt") ? "model_def_hq_dt" : "model_def_hq", strstr(Level, "_bump") ? "_bump" : "");
    return Model;
}

void CBlender_Compile::r_Pass(const char* _vs, const char* _gs, const char* _ps, bool, bool bZtest, bool bZwrite, bool bABlend, D3DBLEND abSRC, D3DBLEND abDST, bool aTest, u32 aRef)
{
    // A level shader on a moving object is lit as a model, by the light its tracker measures where it stands
    string64 ModelVertex, ModelPixel;
    _vs = IsModelCluster ? ModelShaderName(_vs, ModelVertex) : _vs;
    _ps = IsModelCluster ? ModelShaderName(_ps, ModelPixel) : _ps;

    RS.Invalidate();
    ctable.clear();
    passTextures.clear();
    passMatrices.clear();
    pass_compute = false;

    // A pass of the forward level frame that writes depth is drawn as its depth, then its colour (r_End): every
    // such pass when the frame computes occlusion from the depth, the alpha-tested ones under r_msaa, where the
    // depth carries the coverage. Foliage asks to blend by its alpha and writes depth: its blending is dropped
    const bool IsOpaque = !bABlend || (abSRC == D3DBLEND_ONE && abDST == D3DBLEND_ZERO);
    const bool IsFoliage = bABlend && abSRC == D3DBLEND_SRCALPHA && abDST == D3DBLEND_INVSRCALPHA && aTest && aRef;
    const bool IsLevelPass = !IsReflectionFace && (iElement == ShaderVariant::NormalHQ || iElement == ShaderVariant::NormalLQ) && SH->passes.empty();
    const bool IsCovered = IsLevelPass && bZtest && bZwrite && (IsOpaque || IsFoliage) && (ForwardOcclusion() || (ps_r_msaa && aTest && aRef));

    PassSET_ZB(bZtest, bZwrite);
    // A cluster drawn into the cube around the eye is one pass: foliage is cut by its texture there, not blended
    const bool IsFaceCluster = IsCluster && IsReflectionFace && SH->passes.empty();
    PassSET_Blend(bABlend && !((IsCovered || IsFaceCluster || (IsCluster && !IsSortedCluster)) && IsFoliage), abSRC, abDST, aTest, aRef);

    if (aTest)
        AddDefine("USE_R1_ALPHA_TEST", "1");

    if (IsCluster)
        AddDefine("SURFACE_CLUSTER", "1");

    // The main view's clusters are drawn into the multisampled targets: what is cut out tests every sample it covers
    if (IsCluster && !IsReflectionFace && aTest && ps_r_msaa)
        AddDefine("SURFACE_CLUSTER_COVERAGE", "1");

    // The pixel stage of a model has a permutation of its light alone: a cluster of a model tells it what it shades. The
    // XML blends name their model shaders r1_model_*, deffer_model, models_*
    if (IsCluster && _vs && strstr(_vs, "model"))
        AddDefine("SURFACE_CLUSTER_MODEL", "1");

    if (IsReflectionFace)
        AddDefine("SURFACE_REFLECTION_FACE", "1");

    // The scene's forward draw makes opaque surfaces of the level, and foliage as one, cut by its texture: which
    // materials it draws must not follow a setting, as its bins are made once a level. Its sorted list takes the one
    // pass of a sorted material as it is, so do the HUD and the 3D icons
    IsClusterRejected |= IsSortedCluster ? !IsLevelPass && !((iElement == ShaderVariant::HUD || iElement == ShaderVariant::UI) && SH->passes.empty()) : IsCluster && !((IsLevelPass || IsFaceCluster) && bZtest && bZwrite && (IsOpaque || IsFoliage));

    // Many blenders make a pass with the default arguments and set its state after: only one that binds its base
    // texture as a surface of the forward level frame does (r1_tex) is split
    IsCoverCandidate = IsCovered;
    IsCoveredPass = false;
    IsCoveredTested = aTest;

    // A blended surface is weighed order-independently, but for what is cut out by its texture, whose depth the frame
    // writes, and the HUD, which keeps its blending sorted
    IsOitPass = IsLevelPass && (!IsCluster || IsSortedCluster) && ForwardWboit() && IsSurfaceBlender(BT) && bABlend && abSRC == D3DBLEND_SRCALPHA && abDST == D3DBLEND_INVSRCALPHA && !(aTest && aRef);

    xr_strcpy(pass_vs, _vs ? _vs : "null");
    xr_strcpy(pass_ps, _ps ? _ps : "null");
    xr_strcpy(pass_gs, _gs ? _gs : "null");
    xr_strcpy(pass_hs, "null");
    xr_strcpy(pass_ds, "null");
    xr_strcpy(pass_cs, "null");
    shaders_pending = true;

    SetPassPriority(-1);
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
    if (IsCoveredPass)
    {
        IsCoveredPass = false;

        // The depth pass leaves the colour alone. It writes the surface's normal into the second target when the
        // occlusion takes it; with no normal to write and no alpha test it needs no pixel stage
        string128 ColourStage;
        xr_strcpy(ColourStage, pass_ps);

        if (!IsCoveredTested && !ForwardOcclusionNormals())
        {
            xr_strcpy(pass_ps, "null");
        }

        AddDefine("USE_DEPTH_PREPASS", "1");
        r_ColorWriteEnable(false, false, false, false);
        // A state given for one target makes the targets' states independent: the mask of the first is not the second's
        RS.SetRS(1, D3DRS_COLORWRITEENABLE, D3DCOLORWRITEENABLE_RED | D3DCOLORWRITEENABLE_GREEN | D3DCOLORWRITEENABLE_BLUE | D3DCOLORWRITEENABLE_ALPHA);

        if (ForwardVelocityPass())
        {
            RS.SetRS(2, D3DRS_COLORWRITEENABLE, D3DCOLORWRITEENABLE_RED | D3DCOLORWRITEENABLE_GREEN);
        }

        r_End(false);

        xr_strcpy(pass_ps, ColourStage);
        Defines.erase(xr_string("USE_DEPTH_PREPASS"));
        AddDefine("USE_DEPTH_PREPASSED", "1");
        r_ColorWriteEnable(true, true, true, true);
        RS.SetRS(D3DRS_ZFUNC, D3DCMP_EQUAL);
        RS.SetRS(D3DRS_ZWRITEENABLE, FALSE);
        shaders_pending = true;
    }

    // It adds its weighed colour into one target and multiplies the share of the scene it leaves into the other; the
    // frame draws it first of the sorted surfaces (RenderForwardOit), so it writes no depth
    if (IsOitPass)
    {
        IsOitPass = false;
        AddDefine("USE_SURFACE_WBOIT", "1");
        RS.SetRS(D3DRS_ZWRITEENABLE, FALSE);
        PassSET_ablend_mode(u32(-1), true, D3DBLEND_ONE, D3DBLEND_ONE);
        PassSET_ablend_mode(1, true, D3DBLEND_DESTCOLOR, D3DBLEND_ZERO);
        SH->flags.bStrictB2F = true;
        SH->flags.OrderIndependent = true;
    }

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
