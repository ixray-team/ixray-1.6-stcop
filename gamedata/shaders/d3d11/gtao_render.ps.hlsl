/*
		Ground-Truth *Based* Ambient Occlusion (unidirectional variant, no arccos)

		References:
		- Practical Real-Time Strategies for Accurate Indirect Occlusion [Jimenez et. al];
		- Screen space indirect lighting with visibility bitmask [Olivier Therrien, Yannick Levesque, Guillaume Gilet]
		- "GT-VBAO (uniformly weighted)" on ShaderToy [TinyTexel]

		Credits:
		- MartyMcFly (huge help with GTAO(VB) implementation and understading the concept of visibility bitmask)
		- TinyTexel (no-arccosine GTAO; main inspiration, https://www.shadertoy.com/view/4cdfzf)
		- Olivier Therrien (original bitmask implementation, + https://x.com/volfaze/status/1865481248929456639)

		Author:
		- LVutner

		---IX-Ray Engine---
*/

#include "common.hlsli"

#include "shared\post_cb.hlsli"

#ifdef USE_GTAO_HQ
	#define GTAO_SCALE 1
#else
	#define GTAO_SCALE 2
#endif

//View-z pyramid with prefiltered MIPs (depth_pyramid.cs.hlsl), HUD view-z is negative
Texture2D<float> s_ao_depth : register(t25);

//GbufferGetPointRealUnjitter from view-z, see GbufferGetViewZ
float3 TapPoint(float2 TexCoord, float ViewZ, bool Hud)
{
	float2 Scale = Hud ? m_invP_hud._11_22 : m_invP._11_22;
	return float3((TexCoord * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f)) * Scale * ViewZ, ViewZ);
}

float example_how_to_not_implement_gtao(float3 view_position, float3 view_normal, float2 texcoord, float2 jitter)
{
	//Few constants
	//TBD: Put everything into common header
	const float GTAO_PI = 3.1415927;
	const float GTAO_TAU = 6.2831854;	
	const float GTAO_HALF_PI = 1.5707964;
	const float GTAO_PI_RCP = 0.31830988148;
	const float GTAO_2_OVER_PI = 0.63661976296;

	//Settings
	int GTAO_DIRECTIONS = 3; //Direction count (3 is sufficient for low radii)
	int GTAO_STEPS = 4; //Step count
	float GTAO_RADIUS = 0.85; //World space radius (Keep it low. Cache-trasher. I am not joking.)
	float GTAO_NEG_1_OVER_RADIUSQR = -1.0 / (GTAO_RADIUS * GTAO_RADIUS); //Just for falloff. Hardcode it if you need to

	//Bias the position to avoid numerical issues
	//0.9992 would be OK even for vanilla view-z buffer
	view_position *= 0.9992;

	//View direction
	float3 view_direction = -normalize(view_position);

	//Screen-space radius (clamped)
	float screen_radius = min((GTAO_RADIUS * gtao_parameters) / view_position.z, 256.0);

	//Slice scale
	//Y flipped as in original GTAO paper, DirectX hello
	float2 slice_scale = pos_decompression_params2.zw * screen_radius * float2(1.0, -1.0);

	//Slice angle, we integrate AO over 2*PI
	float slice_angle = GTAO_TAU / float(GTAO_DIRECTIONS);

	//Accumulated occlusion and slice weight
	float2 occ_weight = (0.0).xx;

	for (int i = 0; i < GTAO_DIRECTIONS; i++)
	{
		float angle = (float(i) + jitter.x) * slice_angle;

		//Slice direction
		float3 slice_direction = float3(cos(angle), sin(angle), 0.0);

		//GTAO math
		float3 axis = cross(view_direction, slice_direction);
		float3 proj_normal = view_normal - axis * dot(view_normal, axis);
		float3 proj_tangent = cross(axis, proj_normal);
		float proj_normal_length = length(proj_normal);
		float sin_n = dot(proj_tangent, view_direction) * rcp(proj_normal_length);

		//Init horizon
		float max_horizon_cos = sin_n;

		//Find hot horizons in your area :flushed:
		for(int j = 0; j < GTAO_STEPS; j += 2)
		{
			//Ray increment
			float2 increment = (j + float2(0.0, 1.0) + jitter.yy) / GTAO_STEPS;

			//Squared for more detail in crevices...			
			increment *= increment;

			//le sample coords
			float4 s_texcoord = texcoord.xyxy + slice_direction.xyxy * slice_scale.xyxy * increment.xxyy;

			//Guard band
			if(dot(s_texcoord.zw - saturate(s_texcoord.zw), 1.0) != 0.0)
				break;

			//Fetch prefiltered view-z, coarser MIPs for farther taps keep them in cache (XeGTAO)
			float2 s_mip = clamp(log2(screen_radius * increment) - 3.3, 0.0, 4.0);
			float2 s_depth = {
				s_ao_depth.SampleLevel(smp_nofilter, s_texcoord.xy, s_mip.x).x,
				s_ao_depth.SampleLevel(smp_nofilter, s_texcoord.zw, s_mip.y).x
			};

			//1st tap
			//Manual unrolling, process 2 steps at the time
			{
				// Sample the view space position
				float3 s_vector = TapPoint(s_texcoord.xy, abs(s_depth.x), s_depth.x < 0.0);
				s_vector -= view_position; //Occlusion vector

				float s_vec_length = dot(s_vector, s_vector);
				float s_horizon = dot(s_vector, view_direction) * rsqrt(s_vec_length);

				//'Obscurance' term, basically a simple falloff known from HBAO/HBAO+. Just a MAD + saturate
				float falloff = saturate(s_vec_length * GTAO_NEG_1_OVER_RADIUSQR + 1.0);
				s_horizon = lerp(-1.0, s_horizon, falloff);

				max_horizon_cos = max(max_horizon_cos, s_horizon);
			}

			//2nd tap
			{
				float3 s_vector = TapPoint(s_texcoord.zw, abs(s_depth.y), s_depth.y < 0.0);
				s_vector -= view_position;

				float s_vec_length = dot(s_vector, s_vector);
				float s_horizon = dot(s_vector, view_direction) * rsqrt(s_vec_length);

				float falloff = saturate(s_vec_length * GTAO_NEG_1_OVER_RADIUSQR + 1.0);
				s_horizon = lerp(-1.0, s_horizon, falloff);

				max_horizon_cos = max(max_horizon_cos, s_horizon);
			}
		}

		//This is an approximation of importance sampling (Horizon remap is baked into equation)
		//Marty's MXAO uses smoothstep() which is a neat approximation (~2% error IIRC?).
		//Note: 1.0 + sinNm - c_horizon_cos is identical to uniformly weighted GTAO (See Jimenez et al presentation for details)
		max_horizon_cos = saturate(0.5 * sin(GTAO_HALF_PI * (1.0 + sin_n) - GTAO_HALF_PI * max_horizon_cos) + 0.5);

		//Accumulate
		//rcp(x) because we are supposed to weight samples by length of projected normal
		occ_weight += float2(1.0 - max_horizon_cos, 1.0) * proj_normal_length;
	}

    //Normalize
	occ_weight.x *= rcp(occ_weight.y);

	//Compensate for missing side...
	return saturate(1.0 - occ_weight.x * 2.0);
}

uint main(PSInputFullscreen I) : SV_Target
{
	//Half resolution renders the top-left pixel of every 2x2 quad, gtao_filter upsamples
	uint2 pixel = uint2(I.hpos.xy) * GTAO_SCALE;
	float2 texcoord = (pixel + 0.5) * pos_decompression_params2.zw;

	//Sample depth buffer
	float zbuffer = s_position.Load(int3(pixel, 0)).x;

	//Early exit
	if(zbuffer == 1.0)
		return asuint(f32tof16(1000.0)) | (asuint(f32tof16(1.0)) << 16);

	//Sample blue noise texture, animated only when TAA or an upscaler accumulates it (texture contains 32 frames)
	//Offset by 16 frames so the pattern is not the one SSLR uses this frame
	uint noise_frame = m_taa_jitter.z < 0.0f ? 0 : (uint(m_taa_jitter.w) + 16) % 32;
	float3 jitter_tex = s_blue_noise[uint3(uint2(I.hpos.xy) % 128, noise_frame)].xyz;

	//Unpack G-Buffer data...
	float3 Normal, Point;
	{
		Normal = s_normal.Load(int3(pixel, 0)).xyz;
		Normal = NormalDecode(Normal.xy * 2.0f - 1.0f);
		Point = TapPoint(texcoord, GbufferGetViewZ(zbuffer), zbuffer < 0.02f);
    }

	//Init. Don't render GTAO past 60 units, fade it out from 50. It will become a noisy mess...
	//View-pos is shifted towards view normal; this eliminates self-occlusion
	float occlusion = Point.z > 60.0 ? 1.0 : lerp(example_how_to_not_implement_gtao(Point + Normal * 0.0035, Normal, texcoord, jitter_tex.xy), 1.0, saturate(Point.z * 0.1 - 5.0));

	//Pack the data into R32_UINT (16 bits for depth, and 16 for occlusion)
	return asuint(f32tof16(Point.z)) | (asuint(f32tof16(occlusion)) << 16);

}
