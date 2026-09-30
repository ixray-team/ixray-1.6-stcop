/*
		Simple TAA

		References:
		- https://gdcvault.com/play/1022970/Temporal-Reprojection-Anti-Aliasing-in
		- https://research.nvidia.com/labs/rtr/publication/yang2020survey/
		- https://github.com/iryoku/smaa
		- https://michaldrobot.com/2014/08/13/hraa-siggraph-2014-slides-available/
		- https://gpuopen.com/learn/optimized-reversible-tonemapper-for-resolve/
		- https://research.activision.com/publications/2020-03/dynamic-temporal-antialiasing-and-upsampling-in-call-of-duty
		- https://dl.acm.org/doi/10.1145/3681758.3697996

		Author:
		- LVutner

		---IX-Ray Engine---
*/

#include "common.hlsli"

Texture2D s_image_prev; //Previous rt_generic_0

//Settings...
#define TAA_BLEND_WEIGHT 0.9 //Blend weight
#define TAA_HISTORY_SHARPNESS 0.5 //Sharpness factor for history filtering

//don't touch it unless you KNOW what you're doing. 
float3 Lottes_Tonemap(float3 c)
{ 
	return saturate(c * rcp(max(c.x, max(c.y, c.z)) + 1.0));
}

float3 Lottes_Tonemap_Inverse(float3 c)
{
	return c * rcp(1.0 - max(c.x, max(c.y, c.z)));
}

static const int2 offset_3x3[9] =
{
	int2(-1, -1),
	int2(0, -1),
	int2(1, -1),
	int2(-1, 0),
	int2(0, 0),
	int2(1, 0),
	int2(-1, 1),
	int2(0, 1),
	int2(1, 1),
};

//From CoD presentation
float3 SMAABicubicFilter(
	float3 current_top,
	float3 current_bottom,
	float3 current_left,
	float3 current_right,
	float3 current_center,
	float3 previous_center,
	float2 f)
{
	float2 w = 0.8 * TAA_HISTORY_SHARPNESS * (f * f - f); //hardcoded sharpness, refer to slides
	float4 color = 
		float4(lerp(current_left, current_right, f.x), 1.0) * w.x +
		float4(lerp(current_top, current_bottom, f.y), 1.0) * w.y;

	color += float4((1.0 + color.w) * previous_center - color.w * current_center, 1.0);
	return color.xyz / color.w;
}

//Cheapest way to get 3x3 neighborhood of single channel texture
void get_3x3_depth(float2 texcoord, float2 gather_texcoord, inout float d_3x3[9])
{
	float4 d_gather0 = s_position.Gather(smp_nofilter, gather_texcoord);
	float4 d_gather1 = s_position.Gather(smp_nofilter, gather_texcoord, int2(-1, -1));

	d_3x3[0] = d_gather1.w;
	d_3x3[1] = d_gather1.z;
	d_3x3[2] = s_position.SampleLevel(smp_nofilter, texcoord, 0, int2(1, -1)).x;
	d_3x3[3] = d_gather1.x;
	d_3x3[4] = d_gather0.w; //d_gather1.y overlap
	d_3x3[5] = d_gather0.z;
	d_3x3[6] = s_position.SampleLevel(smp_nofilter, texcoord, 0, int2(-1, 1)).x;
	d_3x3[7] = d_gather0.x;
	d_3x3[8] = d_gather0.y;
}

void get_3x3_color(float2 texcoord, float2 gather_texcoord, inout float3 c_3x3[9])
{
	float4 c_gather0_r = s_image.GatherRed(smp_nofilter, gather_texcoord);
	float4 c_gather0_g = s_image.GatherGreen(smp_nofilter, gather_texcoord);
	float4 c_gather0_b = s_image.GatherBlue(smp_nofilter, gather_texcoord);

	c_3x3[0] = s_image.SampleLevel(smp_nofilter, texcoord, 0, int2(-1, -1)).xyz;
	c_3x3[1] = s_image.SampleLevel(smp_nofilter, texcoord, 0, int2(0, -1)).xyz;
	c_3x3[2] = s_image.SampleLevel(smp_nofilter, texcoord, 0, int2(1, -1)).xyz;
	c_3x3[3] = s_image.SampleLevel(smp_nofilter, texcoord, 0, int2(-1, 0)).xyz;
	c_3x3[4] = float3(c_gather0_r.w, c_gather0_g.w, c_gather0_b.w);
	c_3x3[5] = float3(c_gather0_r.z, c_gather0_g.z, c_gather0_b.z);
	c_3x3[6] = s_image.SampleLevel(smp_nofilter, texcoord, 0, int2(-1, 1)).xyz;
	c_3x3[7] = float3(c_gather0_r.x, c_gather0_g.x, c_gather0_b.x);
	c_3x3[8] = float3(c_gather0_r.y, c_gather0_g.y, c_gather0_b.y);
}

//you feel fancy? bake new coeffs. those are for "asphalt" scene
static const float3 kdop_axes[8] = 
{
	float3(0.997167, -0.054820, 0.051500),
	float3(0.043682, -0.727663, 0.684542),
	float3(-0.451553, 0.808335, -0.377751),
	float3(-0.003001, 0.615645, -0.788018),
	float3(0.130192, -0.063137, -0.989476),
	float3(0.775079, -0.584236, -0.240668),
	float3(0.589837, 0.171644, -0.789070),
	float3(0.898010, -0.436338, -0.056459)
};

float3 kdop_clipping(float3 mean, float3 prev_color, float3 colors[9], float gamma)
{
	float3 dir = prev_color - mean;
	
	float2 near_far = -10000.0;
	near_far.y = -near_far.x;

	[unroll]
	for(int a = 0; a < 8; a++)
	{
		float3 axis = kdop_axes[a];

		float2 moments = (0.0).xx;

		[unroll]
		for(int n = 0; n < 9; ++n)
		{
			float t = dot(colors[n], axis);
			moments += float2(t, t * t);
		}
		moments *= 1.0 / 9.0;

		float sigma = max(sqrt(moments.y - moments.x * moments.x), 1e-5);
		float proj_pos = dot(mean, axis);
		
		float2 extent;
		extent.x = min(moments.x - gamma * sigma, proj_pos);
		extent.y = max(moments.x + gamma * sigma, proj_pos);

		float inv_dir = 1.0 / dot(dir, axis);
		
		float2 t_01 = (extent.xy - proj_pos) * inv_dir;

		near_far.x = max(near_far.x, min(t_01.x, t_01.y));
		near_far.y = min(near_far.y, max(t_01.x, t_01.y));
	}

	if(near_far.x <= near_far.y && (near_far.x > 0.0f || near_far.y > 0.0f))
	{
		float t = saturate(near_far.x > 0.0f ? near_far.x : near_far.y);
		return mean + t * dir;
	}

	return mean;
}

float4 main(PSInputFullscreen I) : SV_Target
{
	//https://wojtsterna.blogspot.com/2018/02/directx-11-hlsl-gatherred.html
	float2 gather_texcoord = I.texcoord.xy + scaled_screen_res.zw * 0.5;

	//Fetch 3x3 depth neighborhood
	float d_3x3[9];	
	get_3x3_depth(I.texcoord.xy, gather_texcoord, d_3x3);

	//Fetch 3x3 color neighborhood
	float3 c_3x3[9];
	get_3x3_color(I.texcoord.xy, gather_texcoord, c_3x3);
	
	float3 mean = (float3)0.0;

	int2 depth_offset = int2(0, 0);
	float depth_closest = 1.0;

	[unroll]
	for (int i = 0; i < 9; i++)
	{
		c_3x3[i] = Lottes_Tonemap(c_3x3[i]);
		mean += c_3x3[i] * (1.0 / 9.0);

		float sampled_depth = d_3x3[i];

		//Find closest depth. Sign and initial value should be changed for reverse-z
		if(sampled_depth < depth_closest)
		{
			depth_closest = sampled_depth;
			depth_offset = offset_3x3[i];
		}
	}
	
	//Fetch motion vectors and reproject
	float2 motion_vector = s_velocity[clamp(I.hpos.xy + depth_offset, 0, scaled_screen_res.xy - 1)].xy * float2(0.5, -0.5);
	float2 reprojected_tc = I.texcoord.xy - motion_vector;

	//Early quit
	if(any(reprojected_tc != saturate(reprojected_tc)))
		return float4(Lottes_Tonemap_Inverse(c_3x3[4]), 0.0);

	//Fetch previous frame
	float3 p_4 = Lottes_Tonemap(s_image_prev.SampleLevel(smp_rtlinear, reprojected_tc, 0).xyz);

	//Spatio-temporal bicubic filter
	p_4 = SMAABicubicFilter(c_3x3[1], c_3x3[7], c_3x3[3], c_3x3[5], c_3x3[4], p_4, frac(reprojected_tc * scaled_screen_res.xy - 0.5));

	//K-DOP clipping; bigger window for static objects
	float gamma = 1.0 - min(1.0, length(motion_vector * scaled_screen_res.xy * 0.2)) + 0.5;

	p_4 = kdop_clipping(mean, p_4, c_3x3, gamma);

	//because yall still using rgba16, i don't care enough to change blending logic
	float3 reprojected_color = lerp(c_3x3[4], p_4, TAA_BLEND_WEIGHT);

	reprojected_color = Lottes_Tonemap_Inverse(reprojected_color);
	return float4(reprojected_color, 1.0);
}
