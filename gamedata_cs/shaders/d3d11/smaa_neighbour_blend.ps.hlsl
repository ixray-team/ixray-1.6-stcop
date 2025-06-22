/*
------------------------------------------------------------------
SMAA neighbour blend pass
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

Texture2D s_blendtex;

struct p_smaa
{
    float4 hpos : SV_POSITION;
    float2 tc0 : TEXCOORD0; // Texture coordinates         (for sampling maps)
    float4 offset : TEXCOORD2;
};

float4 main(p_smaa I) : SV_Target
{
    return SMAANeighborhoodBlendingPS(I.tc0, I.offset, s_image, s_blendtex);
}
