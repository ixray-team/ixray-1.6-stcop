#include "common.hlsli"

#if defined(SM_5) || defined(SM_4_1)
    #define SMAA_HLSL_4_1
#else
    #define SMAA_HLSL_4
#endif

#define SMAA_INCLUDE_VS 1
uniform float4 scaled_screen_res;
#define SMAA_RT_METRICS scaled_screen_res.zwxy

#include "smaa.hlsli"

// Struct
struct p_smaa
{
    float4 hpos : SV_POSITION;
    float2 texcoord : TEXCOORD0; // Texture coordinates         (for sampling maps)
    float4 offset : TEXCOORD1;
};

// Vertex
p_smaa main(VSInputFullscreen I)
{
    p_smaa O;

    O.hpos = I.hpos;
    O.texcoord = I.texcoord;

    SMAANeighborhoodBlendingVS(I.texcoord, O.offset);

    return O;
}
