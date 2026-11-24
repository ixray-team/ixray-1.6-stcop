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
#include "common\common.hlsli"

// Editor occlusion culling: farthest scene depth of every cell of a coarse grid over the viewport.
// The CPU reads the grid back and rejects boxes whose nearest point is behind it.
RWTexture2D<float> u_hiz : register(u0);

#include "common\extra_cb.hlsli"

[numthreads(8, 8, 1)]
void cs_main(uint2 DTid : SV_DispatchThreadID)
{
	uint2 GridSize;
	u_hiz.GetDimensions(GridSize.x, GridSize.y);

	if (any(DTid >= GridSize))
	{
		return;
	}

	uint2 ViewSize = (uint2)editor_hiz_params.xy;
	float2 Scale = float2(ViewSize) / float2(GridSize);

	// Every pixel the cell touches (conservative)
	uint2 P0 = (uint2)floor(float2(DTid) * Scale);
	uint2 P1 = min((uint2)ceil(float2(DTid + 1) * Scale), ViewSize);

	float MaxDepth = 0.0f;

	[loop]
	for (uint y = P0.y; y < P1.y; ++y)
	{
		[loop]
		for (uint x = P0.x; x < P1.x; ++x)
		{
			float Depth = s_position.Load(int3(x, y, 0)).x;

			// HUD depth (play in editor) does not hide the world
			MaxDepth = max(MaxDepth, Depth < 0.02f ? 1.0f : Depth);
		}
	}

	u_hiz[DTid] = MaxDepth;
}
