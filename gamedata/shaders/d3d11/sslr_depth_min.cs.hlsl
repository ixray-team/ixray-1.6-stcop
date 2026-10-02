#include "common.hlsli"

RWTexture2D<float> u_depth_min : register(u0);

[numthreads(8, 8, 1)]
void main(uint2 DTid : SV_DispatchThreadID)
{
	uint2 CoarseSize;
	u_depth_min.GetDimensions(CoarseSize.x, CoarseSize.y);

	if(any(DTid >= CoarseSize))
	{
		return;
	}

	uint2 FullSize;
	s_position.GetDimensions(FullSize.x, FullSize.y);

	// Every pixel a point inside this texel can resolve to, plus one pixel of margin
	int2 Begin = max(int2((DTid * FullSize) / CoarseSize) - 1, 0);
	int2 End = min(int2(((DTid + 1) * FullSize + CoarseSize - 1) / CoarseSize) + 1, int2(FullSize));

	float2 InvSize = rcp(float2(FullSize));
	float MinDepth = 1.0f;

	for(int y = Begin.y; y < End.y; y += 2)
	{
		for(int x = Begin.x; x < End.x; x += 2)
		{
			float4 Depth = s_position.GatherRed(smp_nofilter, float2(x + 1, y + 1) * InvSize);
			MinDepth = min(MinDepth, min(min(Depth.x, Depth.y), min(Depth.z, Depth.w)));
		}
	}

	u_depth_min[DTid] = MinDepth;
}
