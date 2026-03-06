/* 
	Screen Space Ambient Occlusion by Sir Lancevrot
	Refactor by Hozar_2002 for IX-Ray Platform
*/

#include "common.hlsli"

Texture3D s_blue_noise;
float4 scaled_screen_res;

static const float3 arrKernel[8] =
{
	float3( 0.01263f, +0.01263f, +0.01263f),
	float3(-0.02526f, -0.02526f, -0.02526f),
	float3(-0.03789f, -0.03789f, +0.03789f),
	float3(-0.05052f, +0.05052f, -0.05052f),
	float3(-0.06315f, +0.06315f, +0.06315f),
	float3(+0.07578f, -0.07578f, -0.07578f),
	float3(+0.08841f, -0.08841f, +0.08841f),
	float3(+0.10104f, +0.10104f, -0.10104f)
};
	
inline void doPBAO(inout float ao, in float2 texcoord, in float3 pos, in float3 n, in float invRadSqr)
{
	float3 p = GbufferGetPointRealUnjitter(texcoord);
	float3 dist	= p - pos;
	
	float slen = dot(dist, dist) + EPS_S;
	float3 v = dist * rsqrt(slen);
	
	ao += max(0.0f, dot(n, v)) * rcp(slen * invRadSqr + 1.0f);
}

#define SSAO_RADIUS 0.8

float main(PSInputFullscreen I) : SV_Target0
{
    IXRayGbuffer O = (IXRayGbuffer)NULL;
    GbufferUnpack((uint2)I.hpos.xy, O);
	
	float2 rotSample = s_blue_noise[uint3(uint2(I.hpos.xy) % 128, uint(m_taa_jitter.w) % 32)].xyz;
	rotSample = normalize(rotSample - 0.5f);

	float3 pos = GbufferGetPointRealUnjitter(I.texcoord.xy, O.Depth) * 0.99f;

	// calculate radius
	float radius = SSAO_RADIUS * saturate(pos.z * 0.188679245283f) * (1.0f + pos.z * 0.125f);
	float invRadSqr = rcp(radius * radius);
	
	float2 radius2D	= radius * rcp(pos.z) * 1.2f;
	radius2D.x *= scaled_screen_res.y * scaled_screen_res.z;
	
	float ao = 0.0f;

	for (int i = 0; i < 8; ++i) 
	{
		float2 deltaUV = reflect(arrKernel[i].xy, rotSample) * radius2D;
		
		doPBAO(ao, I.texcoord.xy + deltaUV, pos, O.Normal, invRadSqr);
		doPBAO(ao, I.texcoord.xy + deltaUV * 0.5f, pos, O.Normal, invRadSqr);
	}

	ao = 1.0f - ao * 0.0625f;
	
	return ao * ao;
}

