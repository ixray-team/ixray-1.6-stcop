#include "common\common.hlsli"
#include "common\scene_model.hlsli"

struct vf
{
    float2 tc0 : TEXCOORD0; // base
    float3 tc1 : TEXCOORD1; // view-space position
    float4 c0 : COLOR0; // color
    float4 hpos : SV_POSITION;
    float fog : FOG;
#ifdef SURFACE_CLUSTER
    // the rows of m_WV's turn, which a lens drawn from the scene has in no constant
    nointerpolation float3 view_x : TEXCOORD2;
    nointerpolation float3 view_y : TEXCOORD3;
    nointerpolation float3 view_z : TEXCOORD4;
#endif
};

vf vs_main(PULL_INPUT)
{
    vf o;
    scene_model model = pull_scene_model(VertexId, DrawId);
    v_model v = model.vertex;

    o.hpos = scene_model_project(model, v.P);
    o.tc0 = v.tc.xy;
    o.tc1 = scene_model_view(model, v.P);

    // calculate fade
    float3 dir_v = normalize(o.tc1);
    float3 norm_v = normalize(scene_model_view_turn(model, v.N.xyz));
    float fade = 1 - abs(dot(dir_v, norm_v));
    o.c0 = fade;
    o.fog = 1.0f - calc_fogging(scene_model_place(model, v.P));
#ifdef SURFACE_CLUSTER
    float3x3 view_turn = mul((float3x3)m_V, (float3x3)model.world);
    o.view_x = view_turn[0];
    o.view_y = view_turn[1];
    o.view_z = view_turn[2];
#endif

    o.hpos.xy += m_taa_jitter.xy * o.hpos.w;
    return o;
}

#include "screens\scope_ui.hlsli"
#include "screens\scope_lens.hlsli"

struct v2p
{
    float2 tc0 : TEXCOORD0;
    float3 tc1 : TEXCOORD1;
    float4 c0 : COLOR0;
    float4 hpos : SV_POSITION;
    float fog : FOG;
};

float parralax_fade(in float2 tc, in float3 Point)
{
    // Get edge vectors of the pixel triangle
    float3 dp1 = ddx_fine(Point);
    float3 dp2 = ddy_fine(Point);
	
    float2 duv1 = ddx_fine(tc);
    float2 duv2 = ddy_fine(tc);
	
	float3 N = normalize(cross(dp1, dp2));

    // Solve the linear system
    float3 dp2perp = cross(dp2, N);
    float3 dp1perp = cross(N, dp1);
	
    float3 T = dp2perp * duv1.x + dp1perp * duv2.x;
    float3 B = dp2perp * duv1.y + dp1perp * duv2.y;
    float invmax = rsqrt(max(dot(T, T), dot(B, B)));

	T *= invmax;
    B *= invmax;
	
	float3x3 TBN = float3x3
	(
		T.x, B.x, N.x,
		T.y, B.y, N.y,
		T.z, B.z, N.z
	);
	
	float3 viewDir = mul(transpose(TBN),-Point);
 	viewDir = normalize(viewDir);
	
	tc.xy = tc.xy - viewDir.xy/viewDir.z * 1.0f;
	return smoothstep(0.5, 0.4, length(tc.xy - 0.5));
}

float4 ps_main(v2p I) : SV_Target
{
    if (m_hud_params.y * m_hud_params.a < 0.0001f)
	{
		return 0.0f;
	}

	// Differentiate the interpolated position, avoiding quantization in the HUD depth range.
	float Fade = parralax_fade(I.tc0, I.tc1);

    float4 t_base = s_base.Sample(smp_base, I.tc0);
	t_base.xyz = detonemap(DisplayToTone(t_base.xyz));
	
    float4 t_vp2 = SampleScopeScene(I.tc0, I.hpos.xy);
	
	t_vp2.xyz = ApplyScopeUI(t_vp2.xyz, I.hpos.xy);

	float alpha = m_hud_params.y * m_hud_params.a;
	t_vp2 *= saturate(alpha * 2.0f - 1.0f) * Fade;
	
    float3 final = lerp(t_vp2.xyz, t_base.xyz, t_base.a);
    return float4(final.xyz, saturate(alpha * 2.0f));
}
