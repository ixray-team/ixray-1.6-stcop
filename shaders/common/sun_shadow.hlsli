#ifndef SHARED_SUN_SHADOW_HLSLI
#define SHARED_SUN_SHADOW_HLSLI

// The sun's cascades. By name, as common\lighting.hlsli: s_smap_sun, smp_smap, s_lmap, smp_linear, the sun's
// members of CBLight, m_invV, SMAP_size, SUN_QUALITY, and the filters of pcf_filter.hlsli

bool IsSunCaptureCoordValid(float3 Coord)
{
	const float Margin = 4.0f / SMAP_size;
	// vkd3d-shader, which provides D3DCompile on macOS and Linux, has no
	// isfinite intrinsic. NaN and Inf both fail this comparison, so it rejects
	// the same values as isfinite did.
	return all(abs(Coord) <= 3.402823466e+38F) && all(Coord.xy > Margin) && all(Coord.xy < 1.0f - Margin) && Coord.z > 0.0f && Coord.z < 1.0f;
}

// Correct the receiver for a changed sun direction, using the original capture.
// This bounded estimate cannot reconstruct moving or previously hidden blockers.
bool CorrectSunShadowCoord(float3 Original, float4x4 CaptureTransform, int Index, out float3 Corrected)
{
	Corrected = Original;
	if (SunCaptureDirectionDepth[Index].w <= 0.0f || !IsSunCaptureCoordValid(Original))
	{
		return false;
	}
	float3 Warp = SunReceiverWarpBias[Index].xyz;
	[branch]
	if (SunCacheParams.x < 2.0f || !any(Warp != 0.0f))
	{
		return true;
	}

	float3 TextureWarp = mul(CaptureTransform, float4(Warp, 0.0f)).xyz;
	float ReceiverDepth = Original.z - SunReceiverWarpBias[Index].w;
	[unroll]
	for (int Iteration = 0; Iteration < 2; ++Iteration)
	{
		int2 Base = int2(floor(Corrected.xy * SMAP_size - 0.5f));
		float BlockerSum = 0.0f;
		float BlockerCount = 0.0f;
		[unroll]
		for (int Y = 0; Y < 2; ++Y)
		{
			[unroll]
			for (int X = 0; X < 2; ++X)
			{
				float Depth = s_smap_sun.Load(int4(Base + int2(X, Y), Index, 0));
				if (Depth < 1.0f && Depth < ReceiverDepth - 1e-5f)
				{
					BlockerSum += Depth;
					BlockerCount += 1.0f;
				}
			}
		}
		float Distance = BlockerCount > 0.0f ? max(0.0f, ReceiverDepth - BlockerSum / max(BlockerCount, 1.0f)) * SunCaptureDirectionDepth[Index].w : 0.0f;
		Corrected = Original + Distance * TextureWarp;
		if (!IsSunCaptureCoordValid(Corrected))
		{
			return false;
		}
	}
	return true;
}

// A sample of the air takes the cached map as it is: the correction for a receiver's surface costs a fifth of the
// march and moves nothing in it
float SampleSunShaft(float3 Coord, int Cascade)
{
    Coord.z -= SunReceiverWarpBias[Cascade].w;
	return s_smap_sun.SampleCmpLevelZero(smp_smap, float3(Coord.xy, Cascade), Coord.z);
}

// Calculates cascade indices. Index 3 means no usable capture.
bool calc_cascades(in float3 position, in float4x4 smap_vp_matrix[3], inout int cascade_index, inout float3 smap_texcoord)
{
	cascade_index = 3;
	smap_texcoord = 0.0f;

	// HACK: FXC miscompiles the index when it is the counter of an unrolled loop with an early exit: later tests of
	// cascade_index against a constant are folded away. The counter stays local and the result is assigned from it
	[unroll]
	for (int Index = 0; Index < 3; ++Index)
	{
		[branch]
		if (cascade_index == 3)
		{
			//Transform position into UV space
			float4 temp = mul(smap_vp_matrix[Index], float4(position, 1.0));
			float3 coord = temp.xyz / temp.w;
			bool valid = SunCacheParams.x > 0.0f ? CorrectSunShadowCoord(coord, smap_vp_matrix[Index], Index, coord) : all(abs(coord - 0.5) < 0.5);
			if (valid)
			{
				cascade_index = Index;
				smap_texcoord = coord;
			}
		}
	}

	return cascade_index < 3;
}

//Sun shadows
float shadow_sun(float3 texcoord, int cascade_index)
{
	float bias = 1e-5;
	float2 smap_dims = float2(SMAP_size, 1.0 / SMAP_size);

#if SUN_QUALITY == 0 //Low
	return pcf_3x3(s_smap_sun, smp_smap, texcoord, smap_dims, bias, cascade_index);
#elif SUN_QUALITY == 1 //Medium
	return pcf_5x5(s_smap_sun, smp_smap, texcoord, smap_dims, bias, cascade_index);
#else //High
	return pcf_7x7(s_smap_sun, smp_smap, texcoord, smap_dims, bias, cascade_index);
#endif
}

float sunmask(float4 P)
{
    return 0.25f + 0.75f * s_lmap.SampleLevel(smp_linear, mul(m_sunmask, P).xy, 0.0).w;
}

// Where a surface is tested against the cascades: off the surface along its normal, which is turned to the lit side
// of a surface that scatters
float4 SunShadowPoint(IXRayGbuffer O, float3 Direction)
{
	float3 Shift = O.Normal;

	if (O.SSS > 0.0f)
	{
		Shift *= dot(Direction, Shift) >= 0.0 ? -1.0f : 1.0f;
	}

	return float4(Shift * 0.025f + O.Point.xyz * 0.999f, 1.0f);
}

// The sun's shadow at Point, in view space. Past the cascades the surface's own facing and sky visibility stand in,
// and its scattering fades. SUN_SHADOW_SIMPLE: nine taps and no blend between cascades, for passes that cannot
// afford the filter of the sun's own pass
float SunShadow(inout IXRayGbuffer O, float4 Point, float3 Direction)
{
	int cascade_index;
	float3 smap_texcoord;
	float Shadow = 1.0f;

	// The HUD's view has the eye at the origin: its points are moved to where the camera stands, or the shadow
	// would be that of the world's origin
	float3 world = mul(m_invV, Point).xyz + eye_world - eye_position;
	bool is_in_bounds = calc_cascades(world, m_shadow_sun, cascade_index, smap_texcoord);
#ifdef SUN_SHADOW_SIMPLE
	if (is_in_bounds) {
		Shadow *= pcf_3x3(s_smap_sun, smp_smap, smap_texcoord, float2(SMAP_size, 1.0 / SMAP_size), 1e-5, cascade_index);
	}
#else
	if (is_in_bounds) {
		Shadow *= shadow_sun(smap_texcoord, cascade_index);

		float2 edge = smoothstep(0.40f, 0.49f, abs(smap_texcoord.xy - 0.5f));
		float blend = max(edge.x, edge.y);

		[unroll] for (int next_index = 1; next_index < 3; ++next_index) {
			// A cascade is a column along the sun: looking towards the sun it also covers receivers far beyond its range,
			// where its texels are several times smaller than a pixel and the shadow aliases under the camera jitter
			if (next_index == cascade_index + 1) {
				blend = max(blend, smoothstep(0.75f, 1.0f, O.ViewDist * length(m_shadow_sun[next_index - 1][0].xyz)));
			}
			[branch] if (next_index == cascade_index + 1 && blend > 0.0f) {
				float4 next = mul(m_shadow_sun[next_index], float4(world, 1.0f));
				float3 coord = next.xyz / next.w;
				bool valid = SunCacheParams.x > 0.0f ? CorrectSunShadowCoord(coord, m_shadow_sun[next_index], next_index, coord) : all(abs(coord - 0.5f) < 0.5f);
				if (valid) {
					Shadow = lerp(Shadow, shadow_sun(coord, next_index), blend);
				}
			}
		}
	}
#endif

	if (cascade_index >= 2) {
		float3 Factor = smoothstep(0.5f, 0.49f, abs(smap_texcoord - 0.5f));
		float Fade = Factor.x * Factor.y * Factor.z;

		O.SSS *= 0.5f + 0.5f * Fade;
		float FarShadow = dot(Direction, O.Normal.xyz);
		FarShadow = smoothstep(0.75f, 0.6f, FarShadow) * saturate(O.Hemi * 8.0f - 2.0f);
		Shadow = lerp(FarShadow, Shadow, Fade);
	}

	return Shadow;
}

#endif
