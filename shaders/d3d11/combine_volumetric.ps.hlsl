#include "common.hlsli"
Texture2D s_vollight : register(t25);

// The shafts are marched with one noise offset per pixel. A 3x3 tent removes it, and the depth weight keeps
// the light in front of the background from spreading over the silhouette of nearer geometry.
// 1 - depth is proportional to the reciprocal view depth and finite on the sky; the pass reads no constants
float4 main(PSInputFullscreen I) : SV_Target
{
	uint2 Size;
	s_vollight.GetDimensions(Size.x, Size.y);

	int2 Pixel = int2(I.hpos.xy);
	float CenterW = 1.0f - s_position.Load(int3(Pixel, 0)).x;

	// The centre tap always counts: a zero weight sum would write NaN into the scene
	float4 Sum = s_vollight.Load(int3(Pixel, 0)) * 4.0f;
	float WeightSum = 4.0f;

	[unroll]
	for (int Y = -1; Y <= 1; ++Y)
	{
		[unroll]
		for (int X = -1; X <= 1; ++X)
		{
			int2 Tap = Pixel + int2(X, Y);
			if ((X == 0 && Y == 0) || any(Tap < 0) || any(Tap >= int2(Size)))
			{
				continue;
			}

			float TapW = 1.0f - s_position.Load(int3(Tap, 0)).x;
			float Weight = float((2 - abs(X)) * (2 - abs(Y))) * saturate(1.0f - abs(TapW - CenterW) / (0.05f * CenterW + 1e-6f));

			Sum += s_vollight.Load(int3(Tap, 0)) * Weight;
			WeightSum += Weight;
		}
	}

	return Sum / WeightSum;
}
