#include "common.hlsli"

#include "shadow.hlsli"
#include "metalic_roughness_ambient.hlsli"

struct v2p
{
    float2 tc : TEXCOORD0;
    float4 c : COLOR0;

    float3 tctexgen : TEXCOORD1;
    float4 hpos : SV_POSITION;
    float fog : FOG;
};

uniform float3 L_model_light_color;
uniform float3 L_model_light_dir;
uniform float3 L_sun_dir_e;

// Pixel
void main(v2p I, out IXRayForward O)
{
    float4 result = I.c * s_base.Sample(smp_base, I.tc);

#if defined(USE_SOFT_PARTICLES) && !defined(DISABLE_SOFT_PARTICLES)
    float3 Point = GbufferGetPoint(I.hpos.xy);
    float spaceDepth = Point.z - I.tctexgen.z;
    result *= Contrast(saturate(spaceDepth * 1.3f), 2.0f);
#endif
	
	
#ifdef USE_PARTICLES_LIGHT
	float3 Normal = normalize(-I.tctexgen);
	
	float3 Irradance = CompureDiffuseIrradance(Normal, hemi_cube_pos_faces.y) + L_ambient.xyz;
	
	int cascade_index;
	float3 smap_texcoord;
	
	float3 Pos = I.tctexgen - Normal * 0.3f * Hash(I.tctexgen * timers.x);
	bool is_in_bounds = calc_cascades(mul(m_invV, float4(Pos, 1.0f)).xyz, m_shadow_sun, cascade_index, smap_texcoord);

	float Shadow = 1.0;

	if(is_in_bounds)
	{
		Shadow = pcf_3x3(s_smap_sun, smp_smap, smap_texcoord, float2(SMAP_size, 1.0 / SMAP_size), 0.0, cascade_index);
	}
	
	Shadow *= dot(Normal, -L_sun_dir_e) * 0.25f + 0.75f;
	Irradance += L_sun_color.xyz * Shadow;
	
	float3 LocalLightDir = normalize(I.tctexgen.xyz - L_model_light_dir.xyz);
	
	Shadow = dot(Normal, -LocalLightDir) * 0.25f + 0.75f;
	Irradance += L_model_light_color.xyz * Shadow;
	
	result.xyz *= Irradance;
#endif

	result = lerp(fog_color, result, I.fog);
    O.Color.xyz = GammaToLinear(result.xyz);
	
#ifdef USE_PBR
	O.Color.w = result.w;
#else
	O.Color.w = GammaToLinear(result.w);
#endif
	
	clip(O.Color.w - EPS);
	
	O.Velocity = 0.0f;
}

