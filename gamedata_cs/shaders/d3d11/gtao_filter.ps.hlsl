/*
        Guided GTAO filter (8x8)

        References:
        - https://bartwronski.com/2019/09/22/local-linear-models-guided-filter/

        Author:
        - LVutner

        This is a classic guided box filter, accelerated with GatherRed.
        Because we read from (packed) UINT buffer, we can fetch occ&depth with single gather + some bitwise magic.

		Worth to mention - this filter is shifted, thus you'll need to compensate it in next pass

        ---IX-Ray Engine---
*/

#include "common.hlsli"

Texture2D<uint> t_gtao_packed;

float main(PSInputFullscreen I) : SV_Target
{
	//Texture coordinates used for Gather4 (this fixes grid-like artifacts)
	//https://www.reedbeta.com/blog/texture-gathers-and-coordinate-precision/
    float2 gather_texcoord = (floor(I.texcoord.xy * pos_decompression_params2.xy - 0.5f) + 1.0f) * pos_decompression_params2.zw;

    //Accumulated moments
    float4 x_x2_y_xy = (0.0).xxxx;

	//Good morning, GPU
	uint4 prefetched[16] = {
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2( 0,  0)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2( 0,  2)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2( 2,  2)),	
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2( 2,  0)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2(-2, -2)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2( 0, -2)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2(-2,  0)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2(-4, -4)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2(-2, -4)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2( 0, -4)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2( 2, -4)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2(-4, -2)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2(-4,  0)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2( 2, -2)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2(-4,  2)),
		t_gtao_packed.GatherRed(smp_nofilter, gather_texcoord, int2(-2,  2))
	};

    //We only need a depth tap, no reason to unpack occ
    float center_tap = f16tof32(t_gtao_packed[I.hpos.xy] & 0xFFFF);

    [loop]
    for(int i = 0; i < 16; i++)
    {
        //This is why I used UINT rendertarget ;)
        uint4 tap = prefetched[i];

        //Unpack view-z and occlusion values
        float4 depth_tap = f16tof32(tap & 0xFFFF);
        float4 occ_tap = f16tof32(tap >> 16);

        //Accumulate moments
        x_x2_y_xy += float4(dot(depth_tap, (1.0).xxxx), dot(depth_tap, depth_tap), dot(occ_tap, (1.0).xxxx), dot(occ_tap, depth_tap));
    }

    //Weight the samples
	x_x2_y_xy *= 0.015625;

    float cyx = (x_x2_y_xy.w - x_x2_y_xy.x * x_x2_y_xy.z);
    float vx = (x_x2_y_xy.y - x_x2_y_xy.x * x_x2_y_xy.x) + 1e-4; //Bias

    float beta = cyx * rcp(vx);
    float alpha = x_x2_y_xy.z - beta * x_x2_y_xy.x;

    //Final, filtered value
    return saturate(beta * center_tap + alpha);
}
