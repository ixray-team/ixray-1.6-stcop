#include "common.hlsli"

uniform float4x4 m_invVP_old;
uniform float4 scaled_screen_res;

inline void get_3x3_depth(float2 texcoord, inout float d_3x3[9])
{
	float2 gather_texcoord = texcoord + scaled_screen_res.zw * 0.5;
	
	float4 d_gather0 = s_position.Gather(smp_nofilter, gather_texcoord);
	float4 d_gather1 = s_position.Gather(smp_nofilter, gather_texcoord, int2(-1, -1));

	d_3x3[0] = d_gather1.w;	d_3x3[1] = d_gather1.z;	
	d_3x3[3] = d_gather1.x;	d_3x3[4] = d_gather0.w;	
	d_3x3[5] = d_gather0.z;	d_3x3[7] = d_gather0.x;
	d_3x3[8] = d_gather0.y;
	
	d_3x3[2] = s_position.SampleLevel(smp_nofilter, texcoord, 0, int2(1, -1)).x;
	d_3x3[6] = s_position.SampleLevel(smp_nofilter, texcoord, 0, int2(-1, 1)).x;
}

inline void remap_hud_depth(inout float depth)
{
	if(depth < 0.02f)
	{
		depth = depth_unpack.z * rcp(depth * 50.0f - depth_unpack.w);
		depth = depth_unpack.x * rcp(depth) + depth_unpack.y;
	}
}

void main(in PSInputFullscreen I, out float Depth : SV_Target)
{	
	float d_3x3[9];
	get_3x3_depth(I.texcoord.xy, d_3x3);
	
	float mid_depth = d_3x3[4];
	float min_depth = min(d_3x3[0], min(d_3x3[1], min(d_3x3[2], min(d_3x3[3], min(d_3x3[4], min(d_3x3[5], min(d_3x3[6], min(d_3x3[7], d_3x3[8]))))))));
	float max_depth = max(d_3x3[0], max(d_3x3[1], max(d_3x3[2], max(d_3x3[3], max(d_3x3[4], max(d_3x3[5], max(d_3x3[6], max(d_3x3[7], d_3x3[8]))))))));
	
	float2 texcoord_old = I.texcoord.xy + s_velocity.SampleLevel(smp_nofilter, I.texcoord.xy, 0).xy * float2(-0.5f, 0.5f);
	float depth_old = 1.0f - s_image.SampleLevel(smp_rtlinear, saturate(texcoord_old.xy), 0.0f).x;
	
	float4 old_project = 1.0f; 
	
	old_project.z = depth_old;
	old_project.y = 1.0f - old_project.y;
	old_project.xy = texcoord_old.xy * 2.0f - 1.0f;
	
	old_project = mul(m_invVP_old, old_project);
	old_project = mul(m_VP, old_project);
	
 	depth_old = old_project.z / old_project.w;
	
	remap_hud_depth(min_depth);
	remap_hud_depth(max_depth);
	remap_hud_depth(mid_depth);
	
	depth_old = min_depth > depth_old || depth_old > max_depth ? mid_depth : depth_old;
	
	Depth = 1.0f - lerp(mid_depth, depth_old, 0.97f);
}

