/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
// Stochastic SSR (Stachowiak, SIGGRAPH 2015): one level of the min-Z pyramid.
RWTexture2D<float> u_hiz_src : register(u0);
RWTexture2D<float> u_hiz_dst : register(u1);

// The sizes of the view's part of the two levels
#include "common\post_cb.hlsli"

[numthreads(8, 8, 1)]
void cs_main(uint2 DTid : SV_DispatchThreadID)
{
	uint2 SrcSize = uint2(HiZSizes.xy);
	uint2 DstSize = uint2(HiZSizes.zw);

	if (any(DTid >= DstSize))
	{
		return;
	}

	// No read leaves the view's part of the level: a level one texel wide has no second texel
	uint2 Src = DTid * 2;
	uint2 Last = SrcSize - 1;

	float MinZ = min(min(u_hiz_src[min(Src, Last)], u_hiz_src[min(Src + uint2(1, 0), Last)]), min(u_hiz_src[min(Src + uint2(0, 1), Last)], u_hiz_src[min(Src + uint2(1, 1), Last)]));

	// Odd source size: the last texel also covers the extra column and row
	bool ExtraX = (SrcSize.x & 1) && DTid.x == DstSize.x - 1;
	bool ExtraY = (SrcSize.y & 1) && DTid.y == DstSize.y - 1;

	if (ExtraX)
	{
		MinZ = min(MinZ, min(u_hiz_src[min(Src + uint2(2, 0), Last)], u_hiz_src[min(Src + uint2(2, 1), Last)]));
	}

	if (ExtraY)
	{
		MinZ = min(MinZ, min(u_hiz_src[min(Src + uint2(0, 2), Last)], u_hiz_src[min(Src + uint2(1, 2), Last)]));
	}

	if (ExtraX && ExtraY)
	{
		MinZ = min(MinZ, u_hiz_src[min(Src + uint2(2, 2), Last)]);
	}

	u_hiz_dst[DTid] = MinZ;
}
