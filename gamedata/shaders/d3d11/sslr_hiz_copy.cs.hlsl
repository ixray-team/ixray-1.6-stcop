#include "common.hlsli"

// Stochastic SSR (Stachowiak, SIGGRAPH 2015): level 0 of the min-Z pyramid.
// HUD depth is pushed to the far plane, world rays must pass through it.
RWTexture2D<float> u_hiz : register(u0);

[numthreads(8, 8, 1)]
void main(uint2 DTid : SV_DispatchThreadID)
{
	float Depth = s_position.Load(int3(DTid, 0)).x;
	u_hiz[DTid] = Depth < 0.02f ? 1.0f : Depth;
}
