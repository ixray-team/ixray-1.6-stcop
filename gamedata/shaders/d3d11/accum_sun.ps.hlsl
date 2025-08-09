#include "common.hlsli"
#include "metalic_roughness_light.hlsli"
#include "ScreenSpaceContactShadows.hlsl"
#include "shadow.hlsli"

struct PSInput
{
	float4 hpos : SV_POSITION;
	float2 texcoord : TEXCOORD0;
};

float4 main(PSInput I) : SV_Target
{
    IXrayGbuffer O;
    GbufferUnpack(I.texcoord, I.hpos.xy, O);

    float3 Shift = O.Normal;

    if (O.SSS > 0.0f)
    {
        Shift *= dot(Ldynamic_dir.xyz, Shift) >= 0.0 ? -1.0f : 1.0f;
    }

    float4 Point = float4(O.Point.xyz, 1.f);
    Point.xyz += Shift * 0.025f;

	//Sample cascades
	int cascade_index;
	float3 smap_texcoord;
	bool is_in_bounds = calc_cascades(mul(m_invV, Point).xyz, m_shadow_sun, cascade_index, smap_texcoord);

	float Shadow = 1.0;

	if(is_in_bounds)
		Shadow = shadow_sun(smap_texcoord, cascade_index);

	//Hozar's far cascade tricks!
	if(cascade_index == 2)
	{
		float3 Factor = smoothstep(0.5f, 0.45f, abs(smap_texcoord - 0.5f));
		float Fade = Factor.x * Factor.y * Factor.z;

		O.SSS *= 0.5f + 0.5f * Fade;	
		float FarShadow = dot(Ldynamic_dir.xyz, O.Normal.xyz);
		FarShadow = smoothstep(0.75f, 0.6f, FarShadow) * saturate(O.Hemi * 8.0f - 2.0f);
		Shadow = lerp(FarShadow, Shadow, Fade);
	}

	//Eval BRDF
    float3 Light = DirectLight(Ldynamic_color, Ldynamic_dir.xyz, O.Normal, O.View.xyz, O.Color, O.Metalness, O.Roughness, O.F0);
    Light += SimpleTranslucency(Ldynamic_color.xyz, Ldynamic_dir.xyz, O.Normal) * O.SSS * O.Color;

	//Trace SSS in first cascade
    if (O.Depth < 0.02f && dot(Shadow.xxx, Light.xyz) > 0.0001f)
        RayTraceContactShadow(I.texcoord, O.PointHud, Ldynamic_dir.xyz, Light);

    Shadow *= sunmask(Point);
    return float4(Light * Shadow, Shadow);
}