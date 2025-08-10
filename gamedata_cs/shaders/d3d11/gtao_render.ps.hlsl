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

struct PSInput
{
	float4 hpos : SV_POSITION;
	float2 texcoord : TEXCOORD0;
};

float gtao_parameters; //Factor used to transform world space radius into screen space

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

	//Screen-space radius
	float screen_radius = (GTAO_RADIUS * gtao_parameters) / view_position.z;

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

			//Fetch z-buffer
			float2 s_depth = {
				s_position.SampleLevel(smp_nofilter, s_texcoord.xy, 0.0f).x,
				s_position.SampleLevel(smp_nofilter, s_texcoord.zw, 0.0f).x
			};

			//1st tap
			//Manual unrolling, process 2 steps at the time
			{
				// Sample the view space position
				float3 s_vector = GbufferGetPointRealUnjitter(s_texcoord.xy, s_depth.x);
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
				float3 s_vector = GbufferGetPointRealUnjitter(s_texcoord.zw, s_depth.y);
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

Texture3D s_blue_noise;
uint main(PSInput I) : SV_Target
{
	//Sample depth buffer
	float zbuffer = s_position.SampleLevel(smp_nofilter, I.texcoord.xy, 0.0f).x;

	//Early exit
	if(zbuffer == 1.0)
		return asuint(f32tof16(1000.0)) | (asuint(f32tof16(1.0)) << 16);

	//Sample blue noise texture
	//You can replace 0 with m_taa_jitter.w % 32 to animate it (texture contains 32 frames)
	float3 jitter_tex = s_blue_noise[uint3(uint2(I.hpos.xy) % 128, 0)].xyz;

	//Unpack G-Buffer data...
	float3 Normal, Point;
	{
		Normal = s_normal.SampleLevel(smp_nofilter, I.texcoord.xy, 0.0f).xyz;
		Normal = NormalDecode(Normal.xy);
		Point = GbufferGetPointRealUnjitter(I.texcoord.xy, zbuffer);
    }

	//Init. Don't render GTAO past 60 units. It will become a noisy mess...
	//View-pos is shifted towards view normal; this eliminates self-occlusion
	float occlusion = Point.z > 60.0 ? 1.0 : example_how_to_not_implement_gtao(Point + Normal * 0.0035, Normal, I.texcoord.xy, jitter_tex.xy);

	//Pack the data into R32_UINT (16 bits for depth, and 16 for occlusion)
	return asuint(f32tof16(Point.z)) | (asuint(f32tof16(occlusion)) << 16);
}