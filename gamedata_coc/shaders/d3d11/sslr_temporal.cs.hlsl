#include "common.hlsli"
#include "reflections.hlsli"
#include "metalic_roughness_ambient.hlsli"
#include "metalic_roughness_light.hlsli"

float IntersectAABB(float3 Dir, float3 Org, float3 Box) 
{
	float3 RcpDir = rcp(Dir);
	
	float3 TNeg = (Box - Org) * RcpDir;
	float3 TPos = -RcpDir * (Box + Org);
	
	return max(min(TNeg.x, TPos.x), max(min(TNeg.y, TPos.y), min(TNeg.z, TPos.z)));
}

float HistoryClamp(float3 History, float3 Filtered, float3 aabb_min, float3 aabb_max) 
{
	float3 Min = min(Filtered, min(aabb_min, aabb_max));
	float3 Max = max(Filtered, max(aabb_min, aabb_max));
	
	float3 Avg2 = Max + Min;
	float3 Dir = Filtered - History;
	
	float3 Org = History - Avg2 * 0.5f;
	float3 Scale = Max - Avg2 * 0.5f;
	
	return saturate(IntersectAABB(Dir, Org, Scale));
}

RWTexture2D<float4> u_sslr : register(u0);

[numthreads(8, 8, 1)]
void main(uint2 DTid : SV_DispatchThreadID, uint2 Gid : SV_GroupID, uint GI : SV_GroupIndex)
{
	//LVutner: Making my life easier.
	PSInputFullscreen I;
	I.hpos.xy = float2(DTid.xy) + 0.5; //half-pix
	I.hpos.zw = float2(0.0, 1.0);
	I.texcoord = I.hpos.xy * pos_decompression_params2.zw;

    IXRayGbuffer O = (IXRayGbuffer)NULL;
    GbufferUnpack((uint2)I.hpos.xy, O);
	
	float4 SSLR4 = s_image.SampleLevel(smp_nofilter, I.texcoord, 0);
	
	if(O.Depth >= 1.0f)
	{
		u_sslr[DTid.xy] = float4(SSLR4.xyz, O.Depth);
		return;
	}
	
	float4 SSLR0 = s_image.Load(int3(I.hpos.xy + int2(+1, +0), 0));
	float4 SSLR1 = s_image.Load(int3(I.hpos.xy + int2(-0, +1), 0));
	float4 SSLR2 = s_image.Load(int3(I.hpos.xy + int2(-1, -0), 0));
	float4 SSLR3 = s_image.Load(int3(I.hpos.xy + int2(-0, -1), 0));
	
	float4 SSLR5 = s_image.Load(int3(I.hpos.xy + int2(+1, +1), 0));
	float4 SSLR6 = s_image.Load(int3(I.hpos.xy + int2(-1, +1), 0));
	float4 SSLR7 = s_image.Load(int3(I.hpos.xy + int2(-1, -1), 0));
	float4 SSLR8 = s_image.Load(int3(I.hpos.xy + int2(-1, -1), 0));
	
	float4 SSLRBoxMinPos = min(SSLR0, min(SSLR2, min(SSLR6, SSLR8)));
	float4 SSLRBoxMaxPos = max(SSLR0, max(SSLR2, max(SSLR6, SSLR8)));
	
	float4 SSLRBoxMin = min(SSLR1, min(SSLR3, min(SSLR5, SSLR7)));
	float4 SSLRBoxMax = max(SSLR1, max(SSLR3, max(SSLR5, SSLR7)));
	
	SSLRBoxMin = min(SSLRBoxMin, SSLRBoxMinPos);
	SSLRBoxMax = max(SSLRBoxMax, SSLRBoxMaxPos);
	
	float4 SSLRMain = SSLR4;
	
	float3 Point = GbufferGetPointRealUnjitter(I.texcoord, O.Depth);
	float3 View = normalize(Point);
	
	float Fog = saturate(SSLRMain.w * fog_params.w + fog_params.x);
	float3 ReflectPoint = View.xyz * SSLRMain.w;
	
	float2 PrevDiffuseUV = I.texcoord.xy + s_velocity.SampleLevel(smp_nofilter, I.texcoord.xy, 0).xy * float2(-0.5f, 0.5f);
	SSLRMain.w = O.Depth;
	
    float4 SSLR_OldDiffyse = s_refl.SampleLevel(smp_rtlinear, PrevDiffuseUV.xy, 0.0f);
	SSLR_OldDiffyse = lerp(SSLRMain, SSLR_OldDiffyse, GetBorderAtten(PrevDiffuseUV));
	
#ifndef USE_LEGACY_LIGHT
	float4 PrevSpecularUV = mul(m_VP_old, float4(mul(m_invV, float4(ReflectPoint, 1.0f)).xyz, 1.0f));
	
	PrevSpecularUV.xy = PrevSpecularUV.xy / PrevSpecularUV.w * float2(0.5f, -0.5f) + 0.5f;	
	PrevSpecularUV.xy = (O.Roughness > 0.1f || O.Depth < 0.02f) ? PrevDiffuseUV.xy : PrevSpecularUV.xy;
	
    float3 SSLR_OldSpecular = s_refl.SampleLevel(smp_rtlinear, PrevSpecularUV.xy, 0.0f).xyz;
	SSLR_OldDiffyse.xyz = lerp(SSLR_OldDiffyse.xyz, SSLR_OldSpecular, GetBorderAtten(PrevSpecularUV));
#endif
	
	float SpecularFactor = HistoryClamp(SSLR_OldDiffyse.xyz, SSLRMain.xyz, SSLRBoxMin.xyz, SSLRBoxMax.xyz);	
	SSLR_OldDiffyse.xyz = lerp(SSLR_OldDiffyse.xyz, SSLRMain.xyz, SpecularFactor);

	float DepthClamp = 1.0f - saturate(50.0f * abs(SSLR_OldDiffyse.w - O.Depth));
	SSLRMain.xyz = lerp(SSLRMain.xyz, SSLR_OldDiffyse.xyz, DepthClamp * 0.98f);
	
	u_sslr[DTid.xy] = SSLRMain;
}

