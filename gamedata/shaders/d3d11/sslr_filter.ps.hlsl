#include "common.hlsli"
#include "reflections.hlsli"
#include "metalic_roughness_ambient.hlsli"
#include "metalic_roughness_light.hlsli"

struct PSInput
{
    float4 hpos : SV_POSITION;
    float2 texcoord : TEXCOORD0;
};

float IntersectAABB(float3 Dir, float3 Org, float3 Box) {
	float3 RcpDir = rcp(Dir);
	
	float3 TNeg = (Box - Org) * RcpDir;
	float3 TPos = -RcpDir * (Box + Org);
	
	return max(min(TNeg.x, TPos.x), max(min(TNeg.y, TPos.y), min(TNeg.z, TPos.z)));
}

float HistoryClamp(float3 History, float3 Filtered, float3 aabb_min, float3 aabb_max) {
	float3 Min = min(Filtered, min(aabb_min, aabb_max));
	float3 Max = max(Filtered, max(aabb_min, aabb_max));
	
	float3 Avg2 = Max + Min;
	float3 Dir = Filtered - History;
	
	float3 Org = History - Avg2 * 0.5f;
	float3 Scale = Max - Avg2 * 0.5f;
	
	return saturate(IntersectAABB(Dir, Org, Scale));
}

void sort(inout float4 a1, inout float4 a2) {
	float4 t = min(a1, a2);
	a2 = max(a1, a2);
	a1 = t;
}

float4 median3(float4 a1, float4 a2, float4 a3) {
	sort(a2, a3);
	sort(a1, a2);
	
	return min(a2, a3);
}

float4 median5(float4 a1, float4 a2, float4 a3, float4 a4, float4 a5) {
	sort(a1, a2);
	sort(a3, a4);
	sort(a1, a3);
	sort(a2, a4);
	
	return median3(a2, a3, a5);
}

float4 median9(float4 a1, float4 a2, float4 a3, float4 a4, float4 a5, float4 a6, float4 a7, float4 a8, float4 a9) {
	sort(a1, a2);
	sort(a3, a4);
	sort(a5, a6);
	sort(a7, a8);
	sort(a1, a3);
	sort(a5, a7);
	sort(a1, a5);
	
	sort(a3, a5);
	sort(a3, a7);
	sort(a2, a4);
	sort(a6, a8);
	sort(a4, a8);
	sort(a4, a6);
	sort(a2, a6);
	
	return median5(a2, a4, a5, a7, a9);
}

float4 main(PSInput I) : SV_Target
{
    IXrayGbuffer O;
    GbufferUnpack(I.texcoord.xy, I.hpos.xy, O);
	
	float4 SSLR4 = s_refl.SampleLevel(smp_nofilter, I.texcoord, 0);

	if(O.Depth >= 1.0f) {
		float4 Enviroment = CompureSpecularIrradance(O.View, 0.5f, 0.35f).xyzz;
		Enviroment.w = 0.0f;
		
		return Enviroment;
	}
	
	float4 SSLR0 = s_refl.SampleLevel(smp_nofilter, I.texcoord, 0, int2(+1, +0));
	float4 SSLR1 = s_refl.SampleLevel(smp_nofilter, I.texcoord, 0, int2(-0, +1));
	float4 SSLR2 = s_refl.SampleLevel(smp_nofilter, I.texcoord, 0, int2(-1, -0));
	float4 SSLR3 = s_refl.SampleLevel(smp_nofilter, I.texcoord, 0, int2(-0, -1));
	
	float4 SSLR5 = s_refl.SampleLevel(smp_nofilter, I.texcoord, 0, int2(+1, +1));
	float4 SSLR6 = s_refl.SampleLevel(smp_nofilter, I.texcoord, 0, int2(-1, +1));
	float4 SSLR7 = s_refl.SampleLevel(smp_nofilter, I.texcoord, 0, int2(-1, -1));
	float4 SSLR8 = s_refl.SampleLevel(smp_nofilter, I.texcoord, 0, int2(-1, -1));
	
	float4 SSLRMain = median9(SSLR0, SSLR1, SSLR2, SSLR3, SSLR4, SSLR5, SSLR6, SSLR7, SSLR8);
	float Lod = 0.0f;
	
	float L = O.ViewDist + length(SSLRMain.xyz);
	SSLRMain.xyz += O.PointReal.xyz;
	
	float4 EndProj;
	
	if(O.Depth < 0.02f) {
		EndProj = mul(m_P_hud, float4(SSLRMain.xyz, 1.0f));
		
#ifdef USE_OFFSCREEN_REFLECTIONS
		SSLRMain.xyz = reflect(O.View, O.Normal);
		O.Hemi = lerp(0.5f, 1.0f, O.Hemi);
		Lod = O.Roughness * 8.0f;
#endif
	} else {
		EndProj = mul(m_P, float4(SSLRMain.xyz, 1.0f));
	}
	
	EndProj.xy = EndProj.xy * rcp(EndProj.w) * float2(0.5f, -0.5f) + 0.5f;
	
	float2 vel = s_velocity.Sample(smp_rtlinear, EndProj.xy).xy * float2(0.5f, -0.5f);
	float2 PrevSpecularUV = saturate(EndProj.xy - vel);
	
	float4 Image = s_image.Sample(smp_rtlinear, PrevSpecularUV.xy);
	SSLRMain.w *= GetBorderAtten(PrevSpecularUV);
	Image.xyz = PopGamma(Image.xyz);
	Image.w = L;
	
#ifdef USE_JITTER_FOR_ENV
	O.Roughness = 0.0f;
#endif
	
	float4 Enviroment = CompureSpecularIrradance(SSLRMain.xyz, O.Hemi, O.Roughness).xyzz;
	Enviroment.w = fog_params.z;
	
#ifdef USE_OFFSCREEN_REFLECTIONS
	float4 Color = s_env.SampleLevel(smp_linear, SSLRMain.xyz, Lod);
	Color.xyz *= rcp(1.00001f - Color.xyz);
	Color.xyz = PopGamma(Color.xyz);
	
	Color.w = length(cubemap_depth_to_vector(SSLRMain.xyz, s_env_depth.SampleLevel(smp_linear, SSLRMain.xyz, 0.0f)));
	
	Image = lerp(Color, Image, SSLRMain.w);
	SSLRMain.w = 1.0f;
	
	O.Hemi = lerp(saturate(O.Hemi * 20), O.Hemi, saturate(Image.w * fog_params.w + fog_params.x));
#endif
	
	if(O.Depth < 0.02f) {
		L = Image.w;
	}
	
	float Fog = 1.0f - saturate(L * fog_params.w + fog_params.x);
	Enviroment = lerp(Enviroment, Image, SSLRMain.w * Fog);
	
	Enviroment.xyz *= rcp(1.0f + Enviroment.xyz);
	Enviroment.xyz = saturate(Enviroment.xyz);
	
	return Enviroment;
	
	// float4 FinalColor = 0.0f;
	// float FinalWeight = 0.0f;
	
	// static const int2 offs[9] =
	// {
		// int2(0, 0),
		// int2(0, 1),
		// int2(1, -1),
		// int2(-1, -1),
		// int2(-1, 0),
		// int2(0, -1),
		// int2(1, 0),
		// int2(-1, 1),
		// int2(1, 1)
	// };
	
	// // O.Roughness *= 0.1f;
	
	// float NdotV = saturate(dot(O.Normal, -O.View));
	
	// for(uint i = 0; i < 9; ++i) {
		// float2 offset = offs[i] * scaled_screen_res.zw;
		
		// float4 SSLR = s_image.SampleLevel(smp_rtlinear, saturate(I.texcoord.xy + offset), 0);
		// float3 Color = s_env.SampleLevel(smp_rtlinear, SSLR.xyz, 0.0f).xyz;
		// float S = length(SSLR.xyz);
		
		// float Fog = PushGamma(saturate(S * fog_params.w + fog_params.x));
		// Color = lerp(Color, fog_color, Fog); 
		
		// float3 L = normalize(SSLR.xyz - O.Point);
		// float3 H = normalize(L + O.View);
	
		// float NdotL = saturate(dot(O.Normal, -L));
		// float NdotH = saturate(dot(O.Normal, -H));
		
		// float D = DistributionGGX(NdotH, O.Roughness) + 0.0001f;
		// float G = GeometrySmithD(NdotL, NdotV, O.Roughness) + 0.0001f;
	
		// float weight = D * G * rcp(SSLR.w + 0.0001f);
		// FinalColor += float4(Color, S) * weight; FinalWeight += weight;
	// }
	
	// FinalColor *= rcp(FinalWeight);
	// return saturate(FinalColor);
	
	// return 0;
}

