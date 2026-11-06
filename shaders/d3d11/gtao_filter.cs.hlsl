/*
        Guided GTAO filter and upsample (5x5 texels of gtao_render)

        References:
        - https://bartwronski.com/2019/09/22/local-linear-models-guided-filter/

        Author:
        - LVutner

        Classic guided box filter: AO is fitted as a linear function of view-z over the window, then evaluated
        at the view-z of every full resolution pixel. Box sums are separable and shared by the group through LDS.

        ---IX-Ray Engine---
*/

#include "common.hlsli"

#ifdef USE_GTAO_HQ
	#define GTAO_SCALE 1
#else
	#define GTAO_SCALE 2
#endif

Texture2D<uint> t_gtao_packed : register(t25);
RWTexture2D<unorm float> u_occ : register(u0);

//View-z, view-z^2, AO, AO * view-z of a 12x12 tile, then the same summed over 5 columns
groupshared float4 Moments[12][12];
groupshared float4 RowSums[12][8];

[numthreads(8, 8, 1)]
void main(uint2 Gid : SV_GroupID, uint2 Tid : SV_GroupThreadID, uint GI : SV_GroupIndex)
{
	uint2 Size;
	t_gtao_packed.GetDimensions(Size.x, Size.y);

	int2 Origin = int2(Gid * 8) - 2;

	//View-z is clamped to the AO range (sky is 1000) and taken relative to the tile center, moments of raw view-z cancel out in fp32
	float Center = min(f16tof32(t_gtao_packed[min(Gid * 8 + 4, Size - 1)] & 0xFFFF), 60.0f);

	for (uint i = GI; i < 144; i += 64)
	{
		uint Tap = t_gtao_packed[clamp(Origin + int2(i % 12, i / 12), 0, int2(Size) - 1)];
		float Depth = min(f16tof32(Tap & 0xFFFF), 60.0f) - Center;
		float Occ = f16tof32(Tap >> 16);
		Moments[i / 12][i % 12] = float4(Depth, Depth * Depth, Occ, Occ * Depth);
	}

	GroupMemoryBarrierWithGroupSync();

	for (uint j = GI; j < 96; j += 64)
	{
		uint2 Row = uint2(j % 8, j / 8);
		RowSums[Row.y][Row.x] = Moments[Row.y][Row.x] + Moments[Row.y][Row.x + 1] + Moments[Row.y][Row.x + 2] + Moments[Row.y][Row.x + 3] + Moments[Row.y][Row.x + 4];
	}

	GroupMemoryBarrierWithGroupSync();

	float4 Mean = (RowSums[Tid.y][Tid.x] + RowSums[Tid.y + 1][Tid.x] + RowSums[Tid.y + 2][Tid.x] + RowSums[Tid.y + 3][Tid.x] + RowSums[Tid.y + 4][Tid.x]) * 0.04f;

	float Beta = (Mean.w - Mean.x * Mean.z) * rcp(Mean.y - Mean.x * Mean.x + 1e-4f);
	float Alpha = Mean.z - Beta * Mean.x;

	uint2 Pixel = (Gid * 8 + Tid) * GTAO_SCALE;

	[unroll]
	for (uint k = 0; k < GTAO_SCALE * GTAO_SCALE; ++k)
	{
		uint2 Target = Pixel + uint2(k % GTAO_SCALE, k / GTAO_SCALE);
		float Depth = min(GbufferGetViewZ(s_position.Load(int3(Target, 0)).x), 60.0f) - Center;
		u_occ[Target] = saturate(Beta * Depth + Alpha);
	}
}
