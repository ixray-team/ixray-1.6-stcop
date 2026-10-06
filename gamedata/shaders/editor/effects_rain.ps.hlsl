#include "common.hlsli"
#include "hmodel.hlsli"

struct v2p
{
    float2 Tex0 : TEXCOORD0;
    float3 World : TEXCOORD1;
    float4 Color : COLOR;
    float4 HPos : SV_POSITION;
};

float4 main(v2p I) : SV_Target
{
    float4 Color = s_base.Sample(smp_base, I.Tex0) * I.Color;

    float3 Normal = normalize(cross(ddy(I.World), ddx(I.World)));
    float3 Env0 = env_s0.SampleLevel(smp_rtlinear, Normal, 0.0f).xyz;
    float3 Env1 = env_s1.SampleLevel(smp_rtlinear, Normal, 0.0f).xyz;
    float3 Irradiance = L_hemi_color.xyz * lerp(Env0, Env1, L_hemi_color.w);

    Color.w *= clamp(dot(Irradiance, Irradiance) * 1.3f, 0.5f, 0.8f);
    Color.xyz = Irradiance;
    return Color;
}
