// Frostbite near-CoC tiling, adapted to conservative dilation and pixel-shader fullscreen helpers.
// https://media.gdcvault.com/gdc2018/presentations/Garcia_Kleber_CircularDepthOf.pdf
#include "common.hlsli"
#include "shared\post_cb.hlsli"

#ifdef DOF_TILE_DILATE
Texture2D<float> t_dof_tiles : register(t30);
#else
Texture2D<float4> t_metadata : register(t27);
#endif

float main(PSInputFullscreen I) : SV_Target
{
	int2 Tile = int2(I.hpos.xy);
	float Radius = 0.0f;
#ifdef DOF_TILE_DILATE
	float2 PixelScale = dof_downsample_params.xy / dof_rt_size.xy;
	int Reach = int(ceil(dof_blur_params.w / (8.0f * min(PixelScale.x, PixelScale.y)))) + 2;
	int2 First = max(Tile - Reach, 0);
	int2 Last = min(Tile + Reach, int2(dof_tile_params.xy) - 1);
	// Include a whole tile of padding for every receiver using this tile in bilinear sampling.
	float2 ReceiverMin = float2(Tile * 8 - 8);
	float2 ReceiverMax = float2(Tile * 8 + 16);
	[loop]
	for (int Y = First.y; Y <= Last.y; ++Y)
	{
		[loop]
		for (int X = First.x; X <= Last.x; ++X)
		{
			float SampleRadius = t_dof_tiles.Load(int3(X, Y, 0));
			float2 SourceMin = float2(int2(X, Y) * 8);
			float2 SourceMax = min(SourceMin + 8.0f, dof_rt_size.xy);
			float2 Distance = max(max(SourceMin - ReceiverMax, ReceiverMin - SourceMax), 0.0f) * PixelScale;
			// One reduced texel covers the integer downsample footprint and linear sampling support.
			if (length(Distance) <= SampleRadius + max(PixelScale.x, PixelScale.y))
			{
				Radius = max(Radius, SampleRadius);
			}
		}
	}
#else
	int2 First = Tile * 8;
	int2 Last = min(First + 8, int2(dof_rt_size.xy));
	[loop]
	for (int Y = First.y; Y < Last.y; ++Y)
	{
		[loop]
		for (int X = First.x; X < Last.x; ++X)
		{
			Radius = max(Radius, t_metadata.Load(int3(X, Y, 0)).z);
		}
	}
#endif
	return Radius;
}
