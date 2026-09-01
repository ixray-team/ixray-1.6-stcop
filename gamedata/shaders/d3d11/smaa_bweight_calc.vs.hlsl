#include "common.hlsli"

#if defined(SM_5) || defined(SM_4_1)
    #define SMAA_HLSL_4_1
#else
    #define SMAA_HLSL_4
#endif

#define SMAA_INCLUDE_VS 1
#define SMAA_RT_METRICS scaled_screen_res.zwxy

#include "smaa.hlsli"

// Struct
struct p_smaa
{
    float4 hpos : SV_POSITION;
    float2 texcoord : TEXCOORD0; // Texture coordinates         (for sampling maps)
    float2 pixcoord : TEXCOORD1;
    float4 offset[3] : TEXCOORD2;
};

// Vertex
p_smaa main(VSInputFullscreen I)
{
    p_smaa O;

    O.hpos = I.hpos;
    O.texcoord = I.texcoord;

    SMAABlendingWeightCalculationVS(I.texcoord, O.pixcoord, O.offset);

    return O;
}
