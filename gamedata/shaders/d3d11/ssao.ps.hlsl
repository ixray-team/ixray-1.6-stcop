#ifndef SSAO_1231242112
#define SSAO_1231242112
#include "common.hlsli"

#ifndef SSAO_QUALITY

float calc_ssao(float3 P, float3 N, float2 tc, float2 tcJ, float4 pos2d)
{
    return 1.0;
}

#else // SSAO_QUALITY

Texture2D jitter0;
sampler smp_jitter;
float4 scaled_screen_res;
#define SSAO_RADIUS 0.8

float3 uv_to_eye(float2 uv, float eye_z)
{
    uv = uv * float2(2.0f, 2.0f) - float2(1.0f, 1.0f);
    return float3(uv * pos_decompression_params.xy, 1.0f) * eye_z;
}

/* 
	SSAO Нагло украдено у Sir Lancevrot (с его разрешения)
*/

float3 GetViewPos(float2 uv)
{
	float depth = s_position.SampleLevel(smp_nofilter, uv, 0).x;
    return uv_to_eye(uv, depth_unpack.x * rcp(depth - depth_unpack.y));
}

float doPBAO(float2 uv, float3 pos, float3 n, float invRad, float bias, float selfOcc)
{
	float3 p = GetViewPos(uv);
	float3 dist	= p - pos;
	
	float len = length(dist);
	float3 v = dist * rcp(len);
	
	float atten	= len * invRad;
	return max(-selfOcc, dot(n, v) - bias) * rcp(atten * atten + 1.0f);
}

float calc_ssao(float3 pos, float3 normal, float2 tc0, float2 tcJ, float4 pos2d)
{
	// define kernel
	float n = 0.0f;
	const float step = 0.875f;
	const float fScale = 0.57735f * 0.025f; 

	const float inv2 = 0.5f;
	const float inv5_3 = 0.188679245283f;
	const float inv8 = 0.125f;
	const float inv16 = 0.0625f;
	const float selfOcc = 0.0f; // range: 0.0f to 1.0f
	const float2 focalLen = rcp(pos_decompression_params.xy);

	const float3 arrKernel[8] =
	{
		float3( 1.0,  1.0,  1.0) * fScale * (n += step),
		float3(-1.0, -1.0, -1.0) * fScale * (n += step),
		float3(-1.0, -1.0,  1.0) * fScale * (n += step),
		float3(-1.0,  1.0, -1.0) * fScale * (n += step),
		float3(-1.0,  1.0,  1.0) * fScale * (n += step),
		float3( 1.0, -1.0, -1.0) * fScale * (n += step),
		float3( 1.0, -1.0,  1.0) * fScale * (n += step),
		float3( 1.0,  1.0, -1.0) * fScale * (n += step),
	};
	
	float2 jit_offset = 0.0f;
	sincos(m_taa_jitter.z * 6.283, jit_offset.x, jit_offset.y);
	
	float2 tc1 = (tc0 * scaled_screen_res.xy + jit_offset * 4.0f) * 0.015625f;
	float3 rotSample = jitter0.Sample(smp_jitter, tc1).xyz;
	rotSample = normalize(rotSample - 0.5f);

	pos = uv_to_eye(tc0, pos.z * 0.99f);
	
	// calculate angle bias
	float bias = 0.0;
	
	// calculate contrast
	float contrast = inv16 * rcp(1.0f - saturate(bias));

	// calculate radius
	float radius = SSAO_RADIUS * saturate(pos.z * inv5_3) * (1.0f + pos.z * inv8);
	float invRad = rcp(radius);
	
	float2 radius2D	= radius * focalLen * rcp(pos.z);
	float ao = 0.0f;

	// calculate ao
 	[unroll]
	for (int i = 0; i < 8; ++i) {
		float2 deltaUV = reflect(arrKernel[i], rotSample).xy * radius2D;		
		ao += doPBAO(tc0 + deltaUV, pos, normal, invRad, bias, selfOcc);
		ao += doPBAO(tc0 + deltaUV * inv2, pos, normal, invRad, bias, selfOcc);
	}

	ao = 1.0f - (ao * contrast + selfOcc);
	return ao * ao * ao;
}
#endif
#endif

