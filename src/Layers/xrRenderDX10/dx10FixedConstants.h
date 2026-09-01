#pragma once
#include "dx10ConstantBuffer.h"

struct alignas(16) CBFrame
{
    Fvector4 timers;
    Fvector4 fog_plane;
    Fvector4 fog_params;
    Fvector4 fog_color;
    Fvector4 L_sun_color;
    Fvector4 L_sun_dir_w;
    Fvector4 L_sun_dir_e;
    Fvector4 L_hemi_color;
    Fvector4 L_ambient;
    Fvector4 L_sky_color;
    Fvector4 env_wind;
    Fvector4 water_intensity;
    Fvector4 sun_shafts_intensity;
    Fvector4 rain_params;
    Fvector4 nvg_color;
    Fvector4 m_hud_params;
    Fvector4 m_zoom_deviation;
    Fvector4 m_affects;
    Fvector4 m_actor_params;
    Fvector4 m_timearrow;
    Fvector4 m_timearrow2;
    Fvector4 test_exp_to_shaders_1;
    Fvector4 test_exp_to_shaders_2;
    Fvector4 color_params;
    Fvector4 color_grading;
    Fvector4 c_brightness;
    Fvector4 c_colormap;
};

struct alignas(16) CBView
{
    Fvector4 m_V[3];
    Fvector4 m_P[4];
    Fvector4 m_VP[4];
    Fvector4 m_invV[3];
    Fvector4 m_invP[4];
    Fvector4 m_VP_old[4];
    Fvector4 m_invVP_old[4];
    Fvector4 eye_position;
    Fvector4 eye_direction;
    Fvector4 eye_normal;
    Fvector4 m_taa_jitter;
    Fvector4 screen_res;
    Fvector4 scaled_screen_res;
    Fvector4 pos_decompression_params2;
};

struct alignas(16) CBObject
{
    Fvector4 m_W[3];
    Fvector4 m_invW[3];
    Fvector4 m_WV[3];
    Fvector4 m_WVP[4];
    Fvector4 m_WVP_old[4];
};

struct alignas(16) CBPass
{
    Fvector4 m_P_hud[4];
    Fvector4 m_texgen[4];
    Fvector4 m_xform[4];
    Fvector4 m_xform_v[4];
    Fvector4 consts;
    Fvector4 wave;
    Fvector4 wind;
    Fvector4 consts_old;
    Fvector4 wave_old;
    Fvector4 wind_old;
    Fvector4 c_scale;
    Fvector4 c_bias;
    Fvector4 c_sun;
    Fvector4 m_invP_hud[4];
    Fvector4 mblur_params;
};

struct alignas(16) CBMaterial
{
    Fvector4 L_material;
    Fvector4 hemi_cube_pos_faces;
    Fvector4 hemi_cube_neg_faces;
    Fvector4 dt_params;
    Fvector4 parallax;
    float    def_aref;
    float    m_AlphaRef;
    float    _pad0[2];
    Fvector4 L_model_light_color;
    Fvector4 L_model_light_dir;
    Fvector4 triLOD;
    Fvector4 m_lmap[2];
    Fvector4 tfactor;
};

struct alignas(16) CBLight
{
    Fvector4 Ldynamic_color;
    Fvector4 Ldynamic_pos;
    Fvector4 Ldynamic_dir;
    int      Ldynamic_hud;
    float    _pad[3];
    Fvector4 m_shadow_sun[12];
    Fvector4 m_shadow[4];
    Fvector4 m_sunmask[3];
};

namespace FixedConstants
{
    // b0..b4: cb_frame, cb_view, cb_object, cb_material, cb_light
    constexpr u32 kSlots = 6;

    void Create();
    void Destroy();
    void UpdateFrame();
    void UpdateView();
    void UpdateObject(const Fmatrix& mW);
    void UpdateMaterial();
    void BindFrame();
    void BindView();
    void BindObject();
    void BindMaterial();
    void BindLight();
    void BindAll();
    void InvalidateBindings();
    bool IsFixedName(const char* n);
    int  FixedClass(const char* n);
    void Flush();

    void SetHemiMaterial(float x,float y,float z,float w);
    void SetHemiPosFaces(float x,float y,float z);
    void SetHemiNegFaces(float x,float y,float z);
    void SetHemiTfactor(const Fvector4& v);
    void SetHemiTfactor(float x,float y,float z,float w);
    void SetLitColor(const Fvector& c, const Fvector& dir);
    void SetDtParams(float x,float y,float z,float w);
    void SetDtParamsScale(float s);
    void SetParallax(float h);
    void SetAlphaRef(float a);
    void SetLModelLight(const Fvector& c, const Fvector& dir);
    void SetTriLOD(float lod);
    void SetTfactor(const Fvector4& v);
    void SetTreeXform(const Fmatrix& m);
    void SetTreeXformV(const Fmatrix& m);
    void SetTreeConsts(float x,float y,float z,float w);
    void SetTreeWave(const Fvector4& v);
    void SetTreeWind(const Fvector4& v);
    void SetTreeConstsOld(float x,float y,float z,float w);
    void SetTreeWaveOld(const Fvector4& v);
    void SetTreeWindOld(const Fvector4& v);
    void SetTreeCScale(float x,float y,float z,float w);
    void SetTreeCBias(float x,float y,float z,float w);
    void SetTreeCSun(float x,float y,float z,float w);
    void SetLMap(const Fmatrix& m);
    void SetShadow(const Fmatrix& m);
    void SetShadowSun(int idx, const Fmatrix& m);
    void SetLdynamic(const Fvector4& c, const Fvector4& p, const Fvector4& d);

    u32  NameHash(const char* n);

    bool OnSet(u32 h, const Fmatrix& A);
    bool OnSet(u32 h, const Fvector4& A);
    bool OnSet(u32 h, float A);
    bool OnSet(u32 h, int A);
    bool OnSetA(u32 h, u32 e, const Fmatrix& A);
    bool OnSetA(u32 h, u32 e, const Fvector4& A);

    // A constant's name either belongs to a fixed layout or it never will, so match it once
    // and cache the verdict: the long tail of blender/post-process constants then costs one
    // compare instead of walking the whole chain on every write.
    template<typename T>
    IC void OnSetCached(RHIShaderConstant* C, const T& A)
    {
        if (!C || C->fixed_id == 0) return;
        const bool hit = OnSet(C->name_hash, A);
        if (C->fixed_id < 0) C->fixed_id = hit ? 1 : 0;
    }
    template<typename T>
    IC void OnSetACached(RHIShaderConstant* C, u32 e, const T& A)
    {
        if (!C || C->fixed_id == 0) return;
        const bool hit = OnSetA(C->name_hash, e, A);
        if (C->fixed_id < 0) C->fixed_id = hit ? 1 : 0;
    }

    IC void OnSet(RHIShaderConstant* C, const Fmatrix& A) { OnSetCached(C, A); }
    IC void OnSet(RHIShaderConstant* C, const Fvector4& A) { OnSetCached(C, A); }
    IC void OnSet(RHIShaderConstant* C, float A) { OnSetCached(C, A); }
    IC void OnSet(RHIShaderConstant* C, int A) { OnSetCached(C, A); }
    IC void OnSetA(RHIShaderConstant* C, u32 e, const Fmatrix& A) { OnSetACached(C, e, A); }
    IC void OnSetA(RHIShaderConstant* C, u32 e, const Fvector4& A) { OnSetACached(C, e, A); }
}
