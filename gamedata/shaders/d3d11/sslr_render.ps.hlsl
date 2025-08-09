#include "common.hlsli"
#include "reflections.hlsli"

struct PSInput
{
    float4 hpos : SV_POSITION;
    float2 texcoord : TEXCOORD0;
};

// TODO: Это можно упростить потом
float3 TangentToWorld(in float3 N, in float3 H)
{
    float3 UpVector = abs(N.z) < 0.999 ? float3(0.0, 0.0, 1.0) : float3(1.0, 0.0, 0.0);
    float3 T = normalize(cross(UpVector, N));
    float3 B = cross(N, T);
				 
    return normalize(T * H.x + B * H.y + N * H.z);
}

// Brian Karis, Epic Games "Real Shading in Unreal Engine 4"
float4 ImportanceSampleGGX(float3 N, float2 Xi, float Roughness)
{
    float m = Roughness * Roughness;
    float m2 = m * m;
		
    float Phi = 2 * PI * Xi.x;
				 
    float CosTheta = sqrt((1.0 - Xi.y) * rcp(1.0 + (m2 - 1.0) * Xi.y));
    float SinTheta = sqrt(abs(1.0 - CosTheta * CosTheta));
				 
    float3 H;
    H.x = SinTheta * cos(Phi);
    H.y = SinTheta * sin(Phi);
    H.z = CosTheta;
		
    float d = (CosTheta * m2 - CosTheta) * CosTheta + 1;
    float D = m2 / (PI * d * d);
    float pdf = D * CosTheta;
	
	pdf = max(0.0001f, pdf);
	H = TangentToWorld(N, H);
	
    return float4(H, pdf);
}

float4 main(PSInput I) : SV_Target
{
    IXrayGbuffer O;
    GbufferUnpack(I.texcoord.xy, I.hpos.xy, O);
	
	if(O.Depth >= 1.0f) {
		return float4(O.PointReal.xyz, 0.0f);
	}
	
	// O.Normal.xyz = normalize(cross(ddx(O.PointReal.xyz), ddy(O.PointReal.xyz)));
	// O.Normal.xyz = normalize(O.Normal.xyz);	
	
	float3 Enviroment = reflect(O.View, O.Normal) * fog_params.z;
	O.Roughness = O.Roughness * 0.85f + 0.15f;
	
#ifdef USE_VASYAN_CUTOFF
	// O.Roughness = min(O.Roughness, 0.5f);
#endif
	
#ifndef USE_JITTER_FOR_ENV
	if(O.Depth > 0.02f && O.Roughness > 1.5f) {
		return float4(Enviroment, 0.0f);
	}
#endif
	
	float3 ReflectPoint = GbufferGetPointRealUnjitter(I.texcoord.xy, O.Depth);
	float3 ViewVec = O.View;
	
	float2 Jitter = s_blue_noise[uint3(uint2(I.hpos.xy) % 128, uint(m_taa_jitter.w) % 32)].xy;
	Jitter.y *= 0.5f; // Bias like screen space stochastic reflections 2015
	
	// O.Normal.xyz = normalize(cross(ddx(O.PointReal.xyz), ddy(O.PointReal.xyz)));
	float4 H = ImportanceSampleGGX(O.Normal, Jitter, O.Roughness);
	
	// H.xyz = normalize(cross(ddx(O.PointReal.xyz), ddy(O.PointReal.xyz)));
	// H.xyz = O.Normal;
	
	float3 RefRef = reflect(ViewVec, H.xyz);
	
#ifdef USE_JITTER_FOR_ENV
	Enviroment = RefRef * fog_params.z;
	
	if(O.Depth > 0.02f && O.Roughness > 1.5f) {
		return float4(Enviroment, 0.0f);
	}
#endif
	
	float3 StartPoint = ReflectPoint * 0.996f;
	
	if(O.Depth >= 0.02f) {
		StartPoint += O.Normal * 0.015f;
		
#ifdef USE_OFFSCREEN_REFLECTIONS
		float4 VSLR = FastViewReflections(StartPoint, RefRef);
		Enviroment.xyz = lerp(Enviroment, VSLR.xyz, VSLR.w);
#endif
	}
	
	float4 SSLR = FastViewReflectionsSSR(StartPoint, RefRef, O.Depth < 0.02f);
	Enviroment.xyz = lerp(Enviroment, SSLR.xyz, SSLR.w);
	
	return float4(Enviroment.xyz - ReflectPoint.xyz, SSLR.w); //float4(StartPoint + L * RefRef, H.w);
}

