#ifndef R1_STATIC_HLSLI
#define R1_STATIC_HLSLI

float r1_fog(float3 wpos)
{
	return saturate(dot(float4(wpos, 1.0f), fog_plane));
}

float3 r1_v_hemi(float3 n)
{
	return L_hemi_color.xyz;
}

float3 r1_v_sun(float3 n)
{
	return L_sun_color.xyz * max(0.0f, dot(n, -L_sun_dir_w.xyz));
}

float3 r1_p_hemi(float2 tc)
{
	float4 t_lmh = s_hemi.Sample(smp_rtlinear, tc);
#ifdef USE_SOC_LIGHTING
	return dot(t_lmh.rgb, 1.0f / 3.0f);
#else
	return t_lmh.a;
#endif
}

float4 r1_sample_base(float2 tc)
{
    float4 color = s_base.Sample(smp_base, tc);
#ifdef USE_R1_ALPHA_TEST
    if (color.a <= m_AlphaRef)
        discard;
#endif
    return color;
}

float calc_fogging(float4 wpos)
{
    return r1_fog(wpos.xyz);
}

float2 calc_detail(float3 wpos)
{
    float fade = distance(wpos, eye_position) * dt_params.w;
    fade = min(fade * fade, 1.0f);
    return float2(1.0f - fade, 0.5f * fade);
}

float3 calc_reflection(float3 pos_w, float3 norm_w)
{
    return reflect(normalize(pos_w - eye_position), norm_w);
}

float3 calc_sun(float3 normal)
{
    return r1_v_sun(normal);
}

float3 calc_model_lq_lighting(float3 normal)
{
    return (normal.y * 0.5f + 0.5f) * L_dynamic_props.w * L_hemi_color.xyz
        + L_ambient.xyz + L_dynamic_props.xyz * calc_sun(normal);
}

float4 calc_model_lmap(float3 position)
{
    float3 clamped = clamp(position, m_plmap_clamp[0].xyz, m_plmap_clamp[1].xyz);
    float4 projected = mul(m_plmap_xform, float4(clamped, 1.0f));
    return projected.xyww;
}

float calc_cyclic(float x)
{
    float f = 1.4142f * sin(x * 3.14159f);
    return f * f - 1.0f;
}
float2 calc_xz_wave(float2 direction, float rigidity)
{
    return direction * rigidity;
}
Texture2D s_att;

struct vf_point
{
    float4 hpos : SV_POSITION;
    float2 tc0 : TEXCOORD0;
    float2 tc1 : TEXCOORD1;
    float2 tc2 : TEXCOORD2;
    float4 color : COLOR0;
};

struct vf_spot
{
    float4 hpos : SV_POSITION;
    float2 tc0 : TEXCOORD0;
    float4 tc1 : TEXCOORD1;
    float2 tc2 : TEXCOORD2;
    float4 color : COLOR0;
};

float4 calc_point(out float2 tc0, out float2 tc1, float4 position, float3 normal)
{
    float3 direction = normalize(position.xyz - Ldynamic_pos.xyz);
    float3 tc = (position.xyz - Ldynamic_pos.xyz) * Ldynamic_pos.w + 0.5f;
    tc0 = tc.xz;
    tc1 = tc.xy;
    return Ldynamic_color * dot(normal, -direction) * calc_fogging(position);
}

float4 calc_spot(out float4 tc0, out float2 tc1, float4 position, float3 normal)
{
    float4 projected = mul(L_dynamic_xform, position);
    tc0 = projected.xyww;
    tc1 = projected.z;
    float3 direction = normalize(position.xyz - Ldynamic_pos.xyz);
    return Ldynamic_color * dot(normal, -direction) * calc_fogging(position);
}
#endif
