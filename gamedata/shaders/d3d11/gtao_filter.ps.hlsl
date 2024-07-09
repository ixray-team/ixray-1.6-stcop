/*
        Guided GTAO filter

        References:
        - https://bartwronski.com/2019/09/22/local-linear-models-guided-filter/

        Author:
        - LVutner

        This is a classic guided box filter, accelerated with GatherRed.
        Because we read from (packed) UINT buffer, we can fetch occ&depth with single gather + some bitwise magic.

        ---IX-Ray Engine---
*/

#include "common.hlsli"

struct PSInput
{
    float4 hpos : SV_Position;
    float4 texcoord : TEXCOORD0;
};

Texture2D<uint> t_gtao_packed;

float main(PSInput I) : SV_Target
{
    int kernel = 7;
    int half_kernel = kernel / 2;

    // We only need a depth tap, no reason to unpack occ
    float center_tap = f16tof32(t_gtao_packed[I.hpos.xy] & 0x0000ffff);

    // Texture coordinates used for Gather4
    float2 gather_texcoord = (floor(I.texcoord * pos_decompression_params2.xy - 0.5) + 1.0) * pos_decompression_params2.zw;

    // Accumulated moments
    float4 x_x2_y_xy = (float4)0.0;

    [loop]
    for (int i = -half_kernel; i <= half_kernel; i += 2)
    {
        [loop]
        for (int j = -half_kernel; j <= half_kernel; j += 2)
        {
            // This is why I used UINT rendertarget ;)
#ifndef SM_5
			uint4 tap = 0; // GatherRed slow emulation (Hozar_2002) for DX10 fan`s
			int3 gather_texcoord_scaled = int3(float2(i, j) + gather_texcoord * pos_decompression_params2.xy, 0);
			
			tap.x = t_gtao_packed.Load(gather_texcoord_scaled, int2(1, 0)).x;
			tap.y = t_gtao_packed.Load(gather_texcoord_scaled, int2(1, 1)).x;
			tap.z = t_gtao_packed.Load(gather_texcoord_scaled, int2(0, 1)).x;
			tap.w = t_gtao_packed.Load(gather_texcoord_scaled, int2(0, 0)).x;
#else
            uint4 tap = t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2(i, j));
#endif

            // Unpack view-z and occlusion values
            float4 depth_tap = f16tof32(tap & 0x0000ffff);
            float4 occ_tap = f16tof32(tap >> 16);

            // Accumulate moments
            x_x2_y_xy += float4(dot(depth_tap, (1.0).xxxx), dot(depth_tap, depth_tap), dot(occ_tap, (1.0).xxxx), dot(occ_tap, depth_tap));
        }
    }

    // Weight the samples
    x_x2_y_xy *= rcp((kernel + 1) * (kernel + 1));

    float cyx = (x_x2_y_xy.w - x_x2_y_xy.x * x_x2_y_xy.z);
    float vx = (x_x2_y_xy.y - x_x2_y_xy.x * x_x2_y_xy.x) + 1e-4; // Avoid NaNs

    float beta = cyx * rcp(vx);
    float alpha = x_x2_y_xy.z - beta * x_x2_y_xy.x;

    // Final, filtered value
    return saturate(beta * center_tap + alpha);
}
