#include "common.hlsli"

#ifndef ISAMPLE
    #define ISAMPLE 0
#endif

uniform float3x4 m_v2w;
uniform Texture2D s_half_depth;

#ifdef HDAO
    #define USE_HDAO 1
#endif

#if SSAO_QUALITY <= 3
    #include "ssao.ps.hlsl"
#else
    #ifndef USE_HDAO
        #define USE_HDAO
    #endif
#endif

#ifdef USE_HDAO
    #if SSAO_QUALITY > 3
        #include "ssao_hdao_new.ps.hlsl"
    #endif
    #define USE_HDAO_CODE
    #if SSAO_QUALITY <= 3
        #define g_f2RTSize (pos_decompression_params2.xy)

        #define g_txDepth s_position
        #define g_txNormal s_normal

        #include "ssao_hdao.ps.hlsl"
    #endif
#endif // USE_HDAO

struct _input
{
    float4 tc0 : TEXCOORD0; // tc.xy, tc.w = tonemap scale
    float2 tcJ : TEXCOORD1; // jitter coords
    float4 pos2d : SV_POSITION;
};

float4 main(_input I) : SV_Target0
{
    IXrayGbuffer O;
    GbufferUnpack(I.tc0.xy, I.pos2d.xy, O);

#ifdef USE_HDAO
    #if SSAO_QUALITY > 3
		float occ = calc_new_hdao(O.Point, O.Normal, I.tc0.xy, I.tcJ.xy, I.pos2d);
    #else
		float occ = calc_hdao(O.Point, O.Normal, I.tc0.xy, I.tcJ.xy, I.pos2d);
    #endif
#else
	float occ = calc_ssao(O.Point, O.Normal, I.tc0.xy, I.tcJ.xy, I.pos2d);
#endif

    return float4(occ, occ, occ, occ);
}

