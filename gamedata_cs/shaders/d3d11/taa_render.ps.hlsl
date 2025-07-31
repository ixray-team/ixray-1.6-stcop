/*
		Simple TAA

		References:
		- https://gdcvault.com/play/1022970/Temporal-Reprojection-Anti-Aliasing-in
		- https://research.nvidia.com/labs/rtr/publication/yang2020survey/
		- https://github.com/iryoku/smaa
		- https://michaldrobot.com/2014/08/13/hraa-siggraph-2014-slides-available/
		- https://gpuopen.com/learn/optimized-reversible-tonemapper-for-resolve/
		- https://research.activision.com/publications/2020-03/dynamic-temporal-antialiasing-and-upsampling-in-call-of-duty

        Author:
        - LVutner

        ---IX-Ray Engine---
*/

#include "common.hlsli"

struct PSInput
{
    float4 hpos : SV_POSITION;
    float4 texcoord : TEXCOORD0;
};

Texture2D s_image_prev; //Previous rt_generic_0
float4 scaled_screen_res; //Render resolution

//Settings...
#define TAA_ALT_PATH //Different min-max estimation. Old path may be slower [todo: check]
#define TAA_BLEND_WEIGHT 0.925 //Blend weight
#define TAA_HISTORY_SHARPNESS 0.75 //Sharpness factor for history filtering
#define TAA_DEVIATION 1.75 //Deviation. 1.75 pix

//From Timothy Lottes
float3 Lottes_Tonemap(float3 c)
{
	return c / (max(c.r, max(c.g, c.b)) + 1.0);
}

float3 Lottes_Tonemap_Inverse(float3 c)
{
	return c / (1.0 - max(c.r, max(c.g, c.b)));
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

//SM_5 path, we save 1 sample
#ifdef SM_5
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
#endif

float4 main(PSInput I) : SV_Target
{
	//https://wojtsterna.blogspot.com/2018/02/directx-11-hlsl-gatherred.html
	float2 gather_texcoord = I.texcoord.xy + scaled_screen_res.zw * 0.5;

	//Fetch 3x3 depth neighborhood
	float d_3x3[9];	
	get_3x3_depth(I.texcoord.xy, gather_texcoord, d_3x3);

	//Fetch 3x3 color neighborhood
	float3 c_3x3[9];
	#ifdef SM_5
		get_3x3_color(I.texcoord.xy, gather_texcoord, c_3x3);
	#endif

	int2 depth_offset = int2(0, 0);
	float depth_closest = 1.0;

	#ifdef TAA_ALT_PATH
		float3 c_m = (0.0).xxx;
		float3 c_m2 = (0.0).xxx;
	#endif

	[unroll]
	for (int i = 0; i < 9; i++)
	{
		#ifdef SM_5
			c_3x3[i] = Lottes_Tonemap(c_3x3[i]);
		#else
			int2 offset_hpos = clamp(I.hpos.xy + offset_3x3[i], 0, scaled_screen_res.xy - 1);
			c_3x3[i] = Lottes_Tonemap(s_image[offset_hpos].xyz);
		#endif

		//Accumulate moments
		#ifdef TAA_ALT_PATH
			c_m += c_3x3[i] * (1.0 / 9.0);
			c_m2 += c_3x3[i] * c_3x3[i] * (1.0 / 9.0);
		#endif

		float sampled_depth = d_3x3[i];

		//Find closest depth. Sign and initial value should be changed for reverse-z
		if(sampled_depth < depth_closest)
		{
			depth_closest = sampled_depth;
			depth_offset = offset_3x3[i];
		}
	}

	//Get min and max color of 3x3 neighborhood
	#ifdef TAA_ALT_PATH
		//1.75 is for stability
		float3 c_stddev = sqrt(max(c_m2 - c_m * c_m, 0.0));
		float3 c_min = c_m - c_stddev * TAA_DEVIATION;
		float3 c_max = c_m + c_stddev * TAA_DEVIATION;
	#else
		//Soft window
		float3 c_min = min(c_3x3[0], min(c_3x3[1], min(c_3x3[2], min(c_3x3[3], min(c_3x3[4], min(c_3x3[5], min(c_3x3[6], min(c_3x3[7], c_3x3[8]))))))));
		c_min += min(c_3x3[1], min(c_3x3[3], min(c_3x3[4], min(c_3x3[5], c_3x3[7]))));
		c_min *= 0.5;

		float3 c_max = max(c_3x3[0], max(c_3x3[1], max(c_3x3[2], max(c_3x3[3], max(c_3x3[4], max(c_3x3[5], max(c_3x3[6], max(c_3x3[7], c_3x3[8]))))))));
		c_max += max(c_3x3[1], max(c_3x3[3], max(c_3x3[4], max(c_3x3[5], c_3x3[7]))));
		c_max *= 0.5;
	#endif

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

	//Clamp history
	p_4 = clamp(p_4, c_min, c_max);

	//SMAA-ish velocity weighting. Something better should be used...
	float2 p_motion_vector = s_velocity[reprojected_tc * scaled_screen_res.xy].xy * float2(0.5, -0.5);

	float2 mags = (0.0).xx;
	mags.x = sqrt(5.0 * length(motion_vector));
	mags.y = sqrt(5.0 * length(p_motion_vector));

	float delta = abs(mags.x * mags.x - mags.y * mags.y) * (1.0 / 5.0);
	float weight = TAA_BLEND_WEIGHT * saturate(1.0 - sqrt(delta) * 8.0);

	//Simple lerp is ok, RGBA16F lmao
	float3 reprojected_color = lerp(c_3x3[4], p_4, weight);

	reprojected_color = max(reprojected_color, 0.0);
	reprojected_color = Lottes_Tonemap_Inverse(reprojected_color);

	return float4(reprojected_color, 1.0);
}