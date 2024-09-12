#include "common.hlsli"
#include "lmodel.hlsli"

#if SUN_QUALITY > 2
    #define USE_ULTRA_SHADOWS
#endif

#include "shadow.hlsli"
uniform float3 view_shadow_proj;

#include "metalic_roughness_light.hlsli"
#include "ScreenSpaceContactShadows.hlsl"

struct PSInput
{
    float4 hpos : SV_Position;
    float4 texcoord : TEXCOORD0;
};

Texture2DArray<float> s_smap_array;
float4x4 m_shadow_array[3];

float pcf_5x5(Texture2DArray<float> shadow_tex, SamplerComparisonState shadow_comp_sampler, float3 tc, float2 shadow_res, float bias, int index)
{
	tc.z -= bias;

	float2 uv = tc.xy * shadow_res.x;

    float2 base_uv = floor(uv.xy + 0.5);
    float2 st = (uv.xy + 0.5 - base_uv.xy);

    base_uv -= float2(0.5, 0.5);
    base_uv *= shadow_res.y;

	float uw0 = (4.0 - 3.0 * st.x);
	float uw1 = 7.0;
	float uw2 = (1.0 + 3.0 * st.x);

	float u0 = (3.0 - 2.0 * st.x) / uw0 - 2;
	float u1 = (3.0 + st.x) / uw1;
	float u2 = st.x / uw2 + 2.0;

	float vw0 = (4.0 - 3.0 * st.y);
	float vw1 = 7.0;
	float vw2 = (1.0 + 3.0 * st.y);

	float v0 = (3.0 - 2.0 * st.y) / vw0 - 2.0;
	float v1 = (3.0 + st.y) / vw1;
	float v2 = st.y / vw2 + 2.0;

	float sum = 0.0;
	sum += uw0 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u0, v0) * shadow_res.y, index), tc.z);
	sum += uw1 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u1, v0) * shadow_res.y, index), tc.z);
	sum += uw2 * vw0 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u2, v0) * shadow_res.y, index), tc.z);

	sum += uw0 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u0, v1) * shadow_res.y, index), tc.z);
	sum += uw1 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u1, v1) * shadow_res.y, index), tc.z);
	sum += uw2 * vw1 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u2, v1) * shadow_res.y, index), tc.z);

	sum += uw0 * vw2 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u0, v2) * shadow_res.y, index), tc.z);
	sum += uw1 * vw2 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u1, v2) * shadow_res.y, index), tc.z);
	sum += uw2 * vw2 * shadow_tex.SampleCmpLevelZero(shadow_comp_sampler, float3(base_uv + float2(u2, v2) * shadow_res.y, index), tc.z);

	return sum / 144.0;
}

float4 main(PSInput I) : SV_Target
{
    IXrayGbuffer O;
    GbufferUnpack(I.texcoord, I.hpos.xy, O);

    float3 Shift = O.Normal;

    if (O.SSS > 0.5f)
    {
        Shift *= dot(Ldynamic_dir, Shift) >= 0.0 ? -1.0f : 1.0f;
    }

    float4 Point = float4(O.Point.xyz, 1.f);
    Point.xyz += Shift * 0.025f;
	float Shadow = 1.0;
	int j;
	bool is_in_bounds = false;
	float3 texcoords;
	for(j = 0; j < 3; j++)
	{
		//Transform position into UV space
		float4 temp = mul(m_shadow_array[j], float4(Point.xyz, 1.0));
		texcoords = temp.xyz / temp.w;

		//Check the bounds
		if(all(abs(texcoords.xyz - 0.5) < 0.5))
		{
			is_in_bounds = true;
			break;
		}
	}

	if(is_in_bounds)
		Shadow = pcf_5x5(s_smap_array, smp_smap, texcoords, float2(SMAP_size, 1.0 / SMAP_size), 0.0, j);


#ifdef USE_LEGACY_LIGHT
    O.Roughness *= 1.0f - O.SSS;
#endif

    O.Normal = normalize(lerp(O.Normal, -Ldynamic_dir.xyz, O.SSS));

	//Fade. Fourth cascade
	if(j == 2)
	{
		// Far edge fading code
		float3 Factor = smoothstep(0.5f, 0.45f, abs(texcoords - 0.5f));

		float Fade = Factor.x * Factor.y * Factor.z;
		O.SSS *= Fade * Fade;
		
		Shadow = lerp(1.0, Shadow, Fade);
	}

    float3 Light = DirectLight(Ldynamic_color, Ldynamic_dir.xyz, O.Normal, O.PointReal.xyz, O.Color, O.Metalness, O.Roughness);

#if defined(USE_HUD_SHADOWS)
    if (O.Depth < 0.02f && dot(Shadow.xxx, Light.xyz) > 0.0001f )
    {
        RayTraceContactShadow(I.texcoord, O.PointHud, -Ldynamic_dir, Light);
    }
#endif

    Shadow *= sunmask(Point);

    return float4(Light * Shadow, Shadow);
}