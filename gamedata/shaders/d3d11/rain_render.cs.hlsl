#include "common.hlsli"
#include "shadow.hlsli"

Texture3D s_water;
Texture2D s_waterFall;

float4 RainDensity;
float4 RainFallof;
float4 WorldX;
float4 WorldZ;
float4 m_level_scale;

float3 GetNVNMap(Texture3D s_texture, float2 tc, float time)
{
    float4 water = s_texture.SampleLevel(smp_linear, float3(tc, time), 0) - 0.5;
    water.xyz = water.wyz;

    water.xyz *= 6;
    water.y = 0;

    return water.xyz;
}

float3 GetWaterNMap(Texture2D s_texture, float2 tc)
{
    float4 water = s_texture.SampleLevel(smp_linear, tc, 0);
    water.xyz = (water.xzy - 0.5) * 2;

    water.xyz *= 0.3;
    water.y = 0;

    return water.xyz;
}

RWTexture2D<float4> u_color : register(u0);
RWTexture2D<float4> u_normal : register(u1);
RWTexture2D<float4> u_surface : register(u2);

#define mirror(x) saturate(1.0 - abs(abs(x) - 1.0))

float3 GbufferGetPointReal(float2 TexCoord)
{
	float Depth = s_position.SampleLevel(smp_rtlinear, TexCoord, 0).x;
	return GbufferGetPointRealJitter(TexCoord, Depth);
}

float3 GetFlatNormal(float2 tc) 
{
	float3 center_point = GbufferGetPointReal(tc);
	
	float3 crosspoint0 = GbufferGetPointReal(tc + float2(pos_decompression_params2.z, 0.0f));
	float3 crosspoint1 = GbufferGetPointReal(tc + float2(0.0f, pos_decompression_params2.w));
	
	float3 crosspoint2 = GbufferGetPointReal(tc - float2(pos_decompression_params2.z, 0.0f));
	float3 crosspoint3 = GbufferGetPointReal(tc - float2(0.0f, pos_decompression_params2.w));
	
	float3 ddx_1 = crosspoint0 - center_point;
	float3 ddy_1 = crosspoint1 - center_point;
	
	float3 ddx_2 = center_point - crosspoint2;
	float3 ddy_2 = center_point - crosspoint3;
	
	if
	(
		tc.x <= pos_decompression_params2.z ||
		tc.x >= (1.0 - pos_decompression_params2.z) ||
		tc.y <= pos_decompression_params2.w ||
		tc.y >= (1.0 - pos_decompression_params2.w)
	) 
	{
		ddx_2 = ddx_1 = tc.x > 0.5 ? ddx_2 : ddx_1;
		ddy_2 = ddy_1 = tc.y > 0.5 ? ddy_2 : ddy_1;
	}
	else
	{
		ddx_1 = abs(ddx_1.z) > abs(ddx_2.z) ? ddx_2 : ddx_1;
		ddy_1 = abs(ddy_1.z) > abs(ddy_2.z) ? ddy_2 : ddy_1;
	}
	
	return normalize(cross(ddx_1, ddy_1));
}

// float4 main(float2 tc : TEXCOORD0, float2 tcJ : TEXCOORD1, float4 Color : COLOR, float4 pos2d : SV_POSITION) : SV_Target
[numthreads(8, 8, 1)]
void main(uint2 DTid : SV_DispatchThreadID, uint2 Gid : SV_GroupID, uint GI : SV_GroupIndex)
{
    //IXRayGbuffer O = (IXRayGbuffer)NULL;
    //GbufferUnpack((uint2)pos2d.xy, O);
	
	IXRayMaterial M = (IXRayMaterial)NULL;
	M.Depth = s_position[DTid];
	
	if(M.Depth > 0.9999f)
	{
		return;
	}
	
	float2 TexCoord = (DTid.xy + 0.5f) * pos_decompression_params2.zw;
	M.Point = GbufferGetPointRealJitter(TexCoord, M.Depth);
	
	IXRayGbufferPack O = (IXRayGbufferPack)NULL;
	
	O.Color = u_color[DTid];
	O.Normal = u_normal[DTid];
	O.Material = u_surface[DTid];
	
	GbufferUnpackMaterial(O, M);

    float4 P = float4(M.Point * 0.996f + M.Normal * 0.2f, 1.0f);
	float3 N = M.Normal;
	
    float4 PS = mul(m_shadow, P);
	
	P.xyz = M.Point;

    float3 WorldP = mul(m_sunmask, P);
    float3 WorldN = mul((float3x3)m_sunmask, N.xyz);

    // Read rain projection with some jetter. Also adding pixel normal
    // factor to jitter to make rain strips more realistic.
	
	//LVutner: We gonna reuse 3x3 filter
    float s = min(1.0f, 2.0f * shadow_rain(PS.xyz / PS.w)); 

#ifndef USE_LEGACY_LIGHT
	s *= saturate(M.Hemi * 10.0f);
#else
	s *= saturate(M.Hemi * 100.0f);
#endif

    //	Apply distance falloff
    // Using fixed fallof factors according to float16 depth coordinate precision.
    float fAtten = 1 - smoothstep(min(RainFallof.y - 15.0f, RainFallof.x), RainFallof.y, P.z);
    s *= fAtten * fAtten;
	//s *= 1.0f - O.SSS;

	float r = saturate(RainDensity.x);

    //	Apply rain density
    s *= r;

    float fIsUp = -dot(Ldynamic_dir.xyz, N.xyz);
    s *= saturate(fIsUp * 10.0f + 5.5);
	
	if(s < EPS)
	{
		return;
	}
	
    fIsUp = max(0, fIsUp);

    float fIsX = WorldN.x;
    float fIsZ = WorldN.z;

    float3 waterSplash = GetNVNMap(s_water, WorldP.xz, timers.x * 3.0);
    float3 tc1 = WorldP * 0.5f;

    float3 waterFallX = GetWaterNMap(s_waterFall, float2(tc1.z, tc1.y + RainDensity.y));
    float3 waterFallZ = GetWaterNMap(s_waterFall, float2(tc1.x, tc1.y + RainDensity.y));

    float2 IsDir = normalize(float2(fIsZ, fIsX));

    float WeaponAttenuation = M.Depth > 0.02f ? 1.0f : 0.0f;
    float ApplyNormalCoeff = s * WeaponAttenuation;

    float3 water = waterSplash * fIsUp;
    water += waterFallX.yxz * abs(fIsX);
    water += waterFallZ.zxy * abs(fIsZ);

    water.xyz = mul((float3x3)m_V, water.xyz);
    N += water.xyz * ApplyNormalCoeff;
	
#ifdef USE_LEGACY_LIGHT
    s *= dot(M.Color.xyz, float3(0.33, 0.33, 0.33));
	
    float ColorIntencity = 1.0f - sqrt(s);
    ColorIntencity = max(ColorIntencity, 0.5f);
	
	M.Color.xyz *= ColorIntencity;
	M.Gloss = saturate(0.8f * s + M.Gloss);
#else
	bool object_mask = M.MaterialID == OBJECT_ID || M.MaterialID == FOLIAGE_ID;
	
	[branch]
	if(!object_mask)
	{
		WorldP = mul(m_invV, P).xyz;
		float2 tc = WorldP.xz * m_level_scale.zw - m_level_scale.xy;	
	
		float3 Jitter = s_blue_noise[uint3(DTid % 128, uint(m_taa_jitter.w) % 32)].xyz;
		
		Jitter = Jitter - 0.5f;
		Jitter *= rcp(1024.0f);
	
		float mask = s_mask.SampleLevel(smp_linear, tc + Jitter.xy * 0.01f, 0).x;
		mask = lerp(0.0f, smoothstep(r, r * 0.9f, mask), saturate(r * 10.0f));
		
		float3 RainNormal = GetFlatNormal(TexCoord);
		fIsUp = -dot(Ldynamic_dir.xyz, RainNormal.xyz);
		mask *= smoothstep(0.6f, 0.8f, fIsUp);
		
		float F90 = saturate(dot(M.Color, 0.333f) * 50.0f);

		M.Color *= lerp(1.0f, 0.66f, s); 
		M.Roughness = lerp(M.Roughness, min(0.3f, M.Roughness), s); 
		M.Specular = lerp(M.Specular, max(0.2f * F90, M.Specular), s);
		
		M.Roughness = lerp(M.Roughness, 0.07f, mask);
		M.Specular = lerp(M.Specular, 0.4f, mask);
		
		M.Color.xyz *= lerp(1.0f, 0.2f, mask);
		M.Color.xyz += Jitter * 4.0f;
		
		N -= Ldynamic_dir.xyz * mask * 60.0f;
	}
#endif
	
	M.Normal = normalize(N);
	
	GbufferPack(O, M);
	
	u_color[DTid] = O.Color;
	u_normal[DTid] = O.Normal;
	u_surface[DTid] = O.Material;
}

