#ifndef HEIGHTFIELD_SHADOW_COMMON_HLSLI
#define HEIGHTFIELD_SHADOW_COMMON_HLSLI

#include "pcf_filter.hlsli"

Texture2D<float> s_heightfield_shadow : register(t27);

// Preserve the existing CSM receiver bias; heightfield uses the actual world point.
float4 SunCascadeReceiver(IXRayGbuffer Surface)
{
	float3 Shift = Surface.Normal;
	if (Surface.SSS > 0.0f)
	{
		Shift *= dot(Ldynamic_dir.xyz, Shift) >= 0.0f ? -1.0f : 1.0f;
	}
	float4 Point = float4(Shift * 0.025f + Surface.Point * 0.999f, 1.0f);
	if (Surface.Depth < 0.02f)
	{
		Point.xyz = -Ldynamic_dir.xyz * 0.4f;
	}
	return Point;
}

float SunLastCascadeFade(float3 Coord)
{
	float3 Factor = smoothstep(0.5f, 0.49f, abs(Coord - 0.5f));
	return Factor.x * Factor.y * Factor.z;
}

// Overlap the last cascade in its light-space XY plane; retain the narrow depth guard.
float HeightfieldCascadeFade(float3 Coord)
{
	float3 Edge = abs(Coord - 0.5f);
	float2 XY = 1.0f - smoothstep(0.40f, 0.49f, Edge.xy);
	float Z = 1.0f - smoothstep(0.49f, 0.50f, Edge.z);
	return XY.x * XY.y * Z;
}

float2 HeightfieldGridPosition(float3 World)
{
	return (World.xz - HeightfieldWorldToGrid.xy) * HeightfieldWorldToGrid.zw;
}

float HeightfieldCoverage(float3 World)
{
	float2 Position = HeightfieldGridPosition(World);
	float2 Edge = min(Position, HeightfieldGrid.xy - 1.0f - Position);
	return smoothstep(0.0f, 4.0f, min(Edge.x, Edge.y));
}

float HeightfieldShadowTap(int2 Pixel, float ReceiverY)
{
	if (any(Pixel < 0) || any(Pixel >= int2(HeightfieldGrid.xy)))
	{
		return 1.0f;
	}
	return step(-s_heightfield_shadow.Load(int3(Pixel, 0)), ReceiverY);
}

float HeightfieldShadowVisibility(float3 World)
{
	float2 Position = HeightfieldGridPosition(World);
	float ReceiverY = World.y + HeightfieldParams.z;
	uint Filtering = uint(HeightfieldParams.w);
	[branch]
	if (Filtering > 0)
	{
		float4 Dimensions = float4(HeightfieldGrid.xy, rcp(HeightfieldGrid.xy));
		float3 Coord = float3((Position + 0.5f) * Dimensions.zw, -ReceiverY);
		[branch]
		if (Filtering == 1)
		{
			return pcf_3x3(s_heightfield_shadow, smp_smap, Coord, Dimensions, 0.0f);
		}
		[branch]
		if (Filtering == 2)
		{
			return pcf_5x5(s_heightfield_shadow, smp_smap, Coord, Dimensions, 0.0f);
		}
		return pcf_7x7(s_heightfield_shadow, smp_smap, Coord, Dimensions, 0.0f);
	}
	int2 Pixel = int2(floor(Position));
	float2 Weight = frac(Position);
	// The old four-tap PCF: compare first, then interpolate linear visibility.
	// Interpolating ceilings would invent slopes between roofs and the ground.
	return lerp(
		lerp(HeightfieldShadowTap(Pixel, ReceiverY), HeightfieldShadowTap(Pixel + int2(1, 0), ReceiverY), Weight.x),
		lerp(HeightfieldShadowTap(Pixel + int2(0, 1), ReceiverY), HeightfieldShadowTap(Pixel + int2(1, 1), ReceiverY), Weight.x), Weight.y);
}

#endif
