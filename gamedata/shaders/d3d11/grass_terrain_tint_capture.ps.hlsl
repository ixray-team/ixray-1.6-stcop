#include "common.hlsli"

struct GrassTerrainCapture
{
    float4 hpos : SV_POSITION;
    float2 uv : TEXCOORD0;
    float height : TEXCOORD1;
};

void main(GrassTerrainCapture I, out float4 Color : SV_Target0, out float Height : SV_Target1)
{
    float4 mask = s_mask.Sample(smp_base, I.uv);
    float sum = dot(mask, 1.0f);
    mask = sum > EPS_L ? mask / sum : float4(1.0f, 0.0f, 0.0f, 0.0f);
    float2 scale = dot(abs(dt_params.xy), 1.0f) > EPS_L ? dt_params.xy : float2(1.0f, 1.0f);
    float2 uv = I.uv * scale;
    float3 detail = s_dt_r.Sample(smp_base, uv).rgb * mask.x;
    detail += s_dt_g.Sample(smp_base, uv).rgb * mask.y;
    detail += s_dt_b.Sample(smp_base, uv).rgb * mask.z;
    detail += s_dt_a.Sample(smp_base, uv).rgb * mask.w;
    float3 albedo = s_base.Sample(smp_base, I.uv).rgb * detail * 2.0f;
    // Store premultiplied linear color and coverage so mip filtering cannot add a dark border.
    Color = float4(GammaToLinear(max(albedo, 0.0f)), 1.0f);
    Height = I.height;
}
