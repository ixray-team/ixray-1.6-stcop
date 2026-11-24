#ifndef R1_STATIC_HLSLI
#define R1_STATIC_HLSLI

float r1_fog(float3 wpos)
{
	return saturate(dot(float4(wpos, 1.0f), fog_plane));
}

// By the depth of the view instead of a plane in the world: the HUD is drawn in a space of its own, where the
// world's plane fogs it by where the camera looks
float r1_fog_depth(float depth)
{
	return saturate(1.0f - fog_params.x - depth * fog_params.w);
}

float3 r1_v_hemi(float3 n)
{
	return L_hemi_color.xyz;
}

// r_lambert_factor: 1 is Lambert, 0.5 is half Lambert
float r1_lambert(float facing)
{
	float factor = L_sun_dir_w.w;
	return pow(saturate(facing * factor + 1.0f - factor), 1.0f / factor);
}

float3 r1_v_sun(float3 n)
{
	return L_sun_color.xyz * r1_lambert(dot(n, -L_sun_dir_w.xyz));
}

float3 r1_p_hemi(float2 tc)
{
	float4 t_lmh = SURFACE_MAP(s_hemi, smp_rtlinear, tc);
#ifdef USE_SOC_LIGHTING
	return dot(t_lmh.rgb, 1.0f / 3.0f);
#else
	return t_lmh.a;
#endif
}

// returns the samples of the pixel that pass the alpha test, as SV_Coverage takes them. Where the alpha crosses the
// reference inside the pixel the test is taken at every sample position
uint r1_alpha_coverage(float2 tc)
{
    float alpha = SURFACE_ALPHA(tc);
    float2 step_x = ddx(tc);
    float2 step_y = ddy(tc);
    uint samples = GetRenderTargetSampleCount();
    uint coverage = alpha > m_AlphaRef ? (1u << samples) - 1u : 0u;
    [branch]
    if (abs(alpha - m_AlphaRef) < fwidth(alpha)) {
        coverage = 0u;
        for (uint sample_idx = 0; sample_idx < samples; sample_idx++) {
            float2 at = GetRenderTargetSamplePosition(sample_idx);
            coverage |= uint(SURFACE_ALPHA_GRAD(tc + step_x * at.x + step_y * at.y, step_x, step_y) > m_AlphaRef) << sample_idx;
        }
    }
    return coverage;
}

float4 r1_sample_base(float2 tc)
{
    float4 color = SURFACE_MAP(s_base, smp_base, tc);
    // The depth pass makes the test by the samples it covers (USE_DEPTH_PREPASS), and the colour pass after it
    // (USE_DEPTH_PREPASSED) has no need of it
#if defined(USE_R1_ALPHA_TEST) && !defined(USE_DEPTH_PREPASS) && !defined(USE_DEPTH_PREPASSED)
    if (color.a <= m_AlphaRef)
        discard;
#endif
#if defined(USE_R1_ALPHA_TEST) && defined(USE_DEPTH_PREPASSED) && defined(SURFACE_CLUSTER_COVERAGE)
    // Under r_msaa the depth pass keeps the samples the cut covers, and the colour is read once, at the pixel's centre:
    // with a kept sample beside it the centre may lie in the hole, which a cut-out texture holds as black. The
    // colour is read wider there, and the hole's share of it, one minus the alpha, is divided out
    [branch] if (color.a <= m_AlphaRef) {
        float4 wide = SURFACE_MAP_AT(s_base, smp_base, tc, ddx(tc) * 4.0f, ddy(tc) * 4.0f);
        color = float4(wide.rgb / max(wide.a, 1.0f / 32.0f), 1.0f);
    }
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
Texture2D s_att : register(t28);

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
