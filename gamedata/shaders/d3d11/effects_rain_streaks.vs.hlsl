#include "common.hlsli"

struct VSInput
{
    float4 P : POSITION;
    float4 Color : COLOR;
    float2 Tex0 : TEXCOORD0;
    float3 Motion : TEXCOORD1;
    float3 Side : TEXCOORD2;
};

struct v2p
{
    float2 Tex0 : TEXCOORD0;
    float3 Point : TEXCOORD1;
    float3 World : TEXCOORD2;

#ifndef DISABLE_MOTION_VECTORS
    float4 HPosCurr : TEXCOORD3;
    float4 HPosOld : TEXCOORD4;
#endif

    float4 Color : COLOR;
    float4 HPos : SV_POSITION;
};

static const float RainMinHalfWidth = 0.75f;

float2 DropRandom(float3 Motion)
{
    float Length = length(Motion);
    float Seed = Length > 1e-6f ? dot(Motion / Length, float3(12.9898f, 78.233f, 37.719f)) * 97.0f : 0.0f;
    return frac(sin(float2(Seed, Seed * 1.731f + 4.1f)) * float2(43758.5453f, 24634.6345f));
}

void main(in VSInput I, out v2p O)
{
    float2 Random = DropRandom(I.Motion);
    float3 Side = I.Side * lerp(0.6f, 1.4f, Random.x);

    float4 HPosAxis = mul(m_WVP, float4(I.P.xyz, 1.0f));
    float4 HPosEdge = mul(m_WVP, float4(I.P.xyz + Side, 1.0f));
    float2 EdgePx = (HPosEdge.xy / max(HPosEdge.w, 1e-3f) - HPosAxis.xy / max(HPosAxis.w, 1e-3f)) * 0.5f * scaled_screen_res.xy;
    float Grow = max(1.0f, RainMinHalfWidth / max(length(EdgePx), 1e-3f));

    float4 P = float4(I.P.xyz + Side * Grow, 1.0f);

    O.Point = mul(m_WV, P);
    O.HPos = mul(m_WVP, P);
    O.World = mul(m_W, P);

#ifndef DISABLE_MOTION_VECTORS
    O.HPosCurr = O.HPos;
    O.HPosOld = mul(m_VP_old, float4(O.World - I.Motion, 1.0f));
#endif

    O.HPos.xy += m_taa_jitter.xy * O.HPos.w;

    O.Tex0 = I.Tex0;
    O.Color = I.Color.bgra;
    O.Color.w = rsqrt(Grow) * lerp(0.55f, 1.0f, Random.y);
}
