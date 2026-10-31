/*
------------------------------------------------------------------
SMAA weight calculation pass
------------------------------------------------------------------
References:
https://github.com/iryoku/smaa
------------------------------------------------------------------
Back to the Roots
*/

#include "common.hlsli"

#define SMAA_HLSL_4_1

#define SMAA_RT_METRICS scaled_screen_res.zwxy

#define SMAA_PRESET_ULTRA

#include "smaa.hlsli"

Texture2D s_edgetex : register(t26);
Texture2D s_areatex : register(t25);
Texture2D s_searchtex : register(t28);

// Struct
struct p_smaa
{
    float4 hpos : SV_POSITION;
    float2 texcoord : TEXCOORD0; // Texture coordinates         (for sampling maps)
    float2 pixcoord : TEXCOORD1;
    float4 offset[3] : TEXCOORD2;
};

float4 main(p_smaa I) : SV_Target
{
    return SMAABlendingWeightCalculationPS(I.texcoord, I.pixcoord, I.offset, s_edgetex, s_areatex, s_searchtex, 0.0f);
};
