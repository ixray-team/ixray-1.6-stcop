// Hair for Skinned Mesh (Vasyan Shader Edition)
// IX-Ray 1.4
// Author: ForserX

#include "common.hlsli"
#include "skin.hlsli"

float4 env_wind;
Texture2D t_hair_mask;

void hair_wave_anim(float2 tc, float mask, inout float3 pos, inout float3 pos_old, float3 normal)
{
    float2 wind_dir = env_wind.xy;
    float wind_strength = env_wind.z * 0.5f + 0.15f;

    wind_dir = normalize(wind_dir);

    float time = timers.x;
    float phase = tc.y * 8.0f;

    // Волновая анимация
    float wave1 = sin(time * 2.0f + phase) * 0.15f;
    float wave2 = sin(time * 3.7f + phase * 1.3f) * 0.05f;
    float amplitude = (wave1 + wave2) * wind_strength * mask;

    float2 offset = wind_dir * amplitude;

    pos.xz += offset * 0.1f;

    float time_old = time - 0.016f;
    float wave1_old = sin(time_old * 2.0f + phase) * 0.15f;
    float wave2_old = sin(time_old * 3.7f + phase * 1.3f) * 0.05f;
    float amplitude_old = (wave1_old + wave2_old) * wind_strength * mask;
    float2 offset_old = wind_dir * amplitude_old;

    pos_old.xz += offset_old * 0.1f * 0.95f;
}

void hair_anim(in v_model I, out p_bumped_new O)
{
    float3 pos = I.P.xyz;
    float3 pos_old = I.P_old.xyz;
    float2 tc = I.tc.xy;

    float mask = t_hair_mask.SampleLevel(smp_nofilter, tc, 0).r;

    // ----------------- Hemi & Indoor Check -----------------
    float3 Nw = mul((float3x3)m_W, I.N);
    float3 hc_face = (dot(Nw, hemi_cube_pos_faces) > dot(-Nw, hemi_cube_neg_faces))
                 ? hemi_cube_pos_faces 
                 : -hemi_cube_neg_faces;

    float hemi_val = saturate(dot(hc_face, Nw));

    bool indoor = hemi_val < 0.25f;
    float wind_factor = indoor ? 0.0f : 1.0f;

    hair_wave_anim(tc, mask * wind_factor, pos, pos_old, I.N);

    float sun = L_material.y;
    O.tcdh = float4(tc.xy, hemi_val, sun);

    float3 Pe = mul(m_WV, float4(pos, 1.0));
    O.position = float4(Pe, 1.0f);

    float3 N = I.N * 2.0f;
#if defined(USE_BUMP) || defined(USE_TDETAIL_BUMP)
    float3 T = I.T * 2.0f;
    float3 B = I.B * 2.0f;
    float3x3 xform = mul((float3x3)m_WV, float3x3(
        T.x, B.x, N.x,
        T.y, B.y, N.y,
        T.z, B.z, N.z));
    O.M1 = xform[0]; O.M2 = xform[1]; O.M3 = xform[2];
#else
    N = mul((float3x3)m_WV, N);
    O.M1 = N.xxx; O.M2 = N.yyy; O.M3 = N.zzz;
#endif

    O.hpos = mul(m_WVP, float4(pos, 1.0));
    O.hpos_curr = O.hpos;
    O.hpos_old = mul(m_WVP_old, float4(pos_old, 1.0));
    O.hpos.xy += m_taa_jitter.xy * O.hpos.w;

    O.snow_mask = 0.0f;
}


// -------------------- Скининг --------------------
#if defined(SKIN_0)
void main(in v_model_skinned_0 I, out p_bumped_new O) { hair_anim(skinning_0(I), O); }
#elif defined(SKIN_1)
void main(in v_model_skinned_1 I, out p_bumped_new O) { hair_anim(skinning_1(I), O); }
#elif defined(SKIN_2)
void main(in v_model_skinned_2 I, out p_bumped_new O) { hair_anim(skinning_2(I), O); }
#elif defined(SKIN_3)
void main(in v_model_skinned_3 I, out p_bumped_new O) { hair_anim(skinning_3(I), O); }
#elif defined(SKIN_4)
void main(in v_model_skinned_4 I, out p_bumped_new O) { hair_anim(skinning_4(I), O); }
#else
void main(in v_model I, out p_bumped_new O) { hair_anim(I, O); }
#endif
