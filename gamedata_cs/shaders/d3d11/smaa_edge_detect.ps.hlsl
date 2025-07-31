/*
------------------------------------------------------------------
SMAA edge detection pass
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
#define EDGE_DETECT_COLOR

#include "smaa.hlsli"

// Struct
struct p_smaa
{
    float4 hpos : SV_POSITION;
    float2 tc0 : TEXCOORD0; // Texture coordinates         (for sampling maps)
    float4 offset[3] : TEXCOORD1;
};

float4 main(p_smaa I) : SV_Target
{
#if defined(EDGE_DETECT_COLOR)
    return float4(SMAAColorEdgeDetectionPS(I.tc0, I.offset, s_image), 0.0f, 0.0f);
#else
    return float4(SMAALumaEdgeDetectionPS(I.tc0, I.offset, s_image), 0.0f, 0.0f);
#endif
}
