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

#if defined(SM_5) || defined(SM_4_1)
    #define SMAA_HLSL_4_1
#else
    #define SMAA_HLSL_4
#endif

uniform float4 scaled_screen_res;
#define SMAA_RT_METRICS scaled_screen_res.zwxy

#define SMAA_PRESET_ULTRA

#include "smaa.hlsli"

Texture2D s_edgetex;
Texture2D s_areatex;
Texture2D s_searchtex;

// Struct
struct p_smaa
{
    float4 hpos : SV_POSITION;
    float2 tc0 : TEXCOORD0; // Texture coordinates         (for sampling maps)
    float2 pixcoord : TEXCOORD1;
    float4 offset[3] : TEXCOORD2;
};

float4 main(p_smaa I) : SV_Target
{
    return SMAABlendingWeightCalculationPS(I.tc0, I.pixcoord, I.offset, s_edgetex, s_areatex, s_searchtex, 0.0f);
};
