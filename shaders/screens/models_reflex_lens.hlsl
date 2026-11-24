#include "common\common.hlsli"
#include "common\scene_model.hlsli"
#include "common\post_cb.hlsli"

struct vf
{
	float4 hpos : SV_Position;
    float2 tc0 : TEXCOORD0;
	float3 T : TEXCOORD1;
	float3 B : TEXCOORD2;
	float3 N : TEXCOORD3;
	float3 P : TEXCOORD4;
	nointerpolation float4 props : TEXCOORD5; // as L_dynamic_props
};

vf vs_main(PULL_INPUT)
{
    vf o;
    scene_model model = pull_scene_model(VertexId, DrawId);
    v_model v = model.vertex;

	o.hpos = scene_model_project(model, v.P);
    o.tc0 = v.tc.xy;
	o.T = scene_model_turn(model, v.T);
	o.B = scene_model_turn(model, v.B);
	o.N = scene_model_turn(model, v.N);
	o.P = scene_model_world(model, v.P);
	o.props = model.props;

    return o;
}

/*
	=====================================================================
	Addon      : Parallax Reflex Sights
	Link       : https://www.moddb.com/mods/stalker-anomaly/addons/parallax-reflex-sights
	Authors    : LVutner, party_50
	Date       : 06.02.2024
	Last Edit  : 15.05.2025
	=====================================================================
*/

TextureCube	s_env0 : register(t26);
TextureCube	s_env1 : register(t49);

float3 sample_sky(float3 dir)
{
	dir.y = (dir.y - max(cos(dir.x) * 0.65, cos(dir.z) * 0.65)) * 2.1;
	dir.y -= -0.35;

	float3 sky0 = s_env0.SampleLevel(smp_base, dir, 0).xyz;
	float3 sky1 = s_env1.SampleLevel(smp_base, dir, 0).xyz;

	return lerp(sky0, sky1, L_ambient.w);
}

float current_lum()
{
	float lum_min = 0.85;
	float lum_max = 3;
	// The exposure for linear light, as the scale of the old tone curve these limits were set for
	float lum = pow(DisplayExposure(s_tonemap.Load(int3(0, 0, 0)).x, tonemap_params.w), 2.2f);
	return clamp(1 - (lum - lum_min) / (lum_max - lum_min), 0, 1);
}

float3 sample_lens_normalmap(float2 tc, float radius)
{
	float2 xy = (tc - 0.5) * 2;
	return float3(xy, sign(radius) * sqrt(radius * radius - dot(xy, xy)));
}

float4 sample_reflections(float2 tc, float3x3 TBNw_inv, float3 w_pos, float3 w_nrm)
{
	float3 normalmap = sample_lens_normalmap(tc, 2);
    float3 lensnormal = normalize(float3(dot(normalmap, TBNw_inv[0]), dot(normalmap, TBNw_inv[1]), dot(normalmap, TBNw_inv[2])));

	float3 reflections = sample_sky(reflect(normalize(normalize(w_pos - eye_position)), lensnormal));
	
	float angle_factor = (dot(normalize(w_pos - eye_position), normalize(w_nrm)) + 1) / 2;
	
	float4 color_direct = s_base.Sample(smp_rtlinear, float2(tc.x / 2 + 0.5, tc.y / 2));
	float4 color_angled = s_base.Sample(smp_rtlinear, float2(tc.x / 2 + 0.5, tc.y / 2 + 0.5));
	float4 color = lerp(color_direct, color_angled, smoothstep(0, 0.5, angle_factor));
	reflections = saturate(reflections * color.rgb);
	
	return float4(reflections, current_lum() * smoothstep(0, 0.03, angle_factor) * color.a);
}

// calc_model_lq_lighting of r1_static.hlsli, which this shader doesn't include
float3 dirt_lighting(float3 normal, float4 props)
{
	float3 sun = L_sun_color.xyz * max(0.0f, dot(normal, -L_sun_dir_w.xyz));
	return (normal.y * 0.5f + 0.5f) * props.w * L_hemi_color.xyz + L_ambient.xyz + props.xyz * sun;
}

float4 sample_dirt(float2 tc, float3 w_nrm, float4 props)
{	
	float4 color = s_base.Sample(smp_rtlinear, float2(tc.x / 2, tc.y / 2 + 0.5));
	return float4(color.rgb * 2 * dirt_lighting(w_nrm, props), color.a);
}

float4 rgba_blend(float4 b, float4 a)
{
	float na = a.a + b.a * (1 - a.a);
	float3 nc = na > 0 ? (a.rgb * a.a + b.rgb * b.a * (1 - a.a)) / na : float3(0, 0, 0);
	return float4(nc, na);
}

float4 ps_main(vf I): SV_Target
{
	float4 lens = s_base.Sample(smp_rtlinear, I.tc0 / 2);
	lens.a *= current_lum();
	
	float4 reflections = sample_reflections(I.tc0, transpose(float3x3(I.T, I.B, I.N)), I.P, I.N);
	float4 dirt = sample_dirt(I.tc0, I.N, I.props);
	
	return rgba_blend(rgba_blend(lens, reflections), dirt);
}
