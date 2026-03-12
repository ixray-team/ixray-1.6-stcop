#include "common.hlsli"
#include "reflections.hlsli"
#include "metalic_roughness_ambient.hlsli"

Texture3D s_blue_noise;

// TODO: Это можно упростить потом
float3 TangentToWorld(in float3 N, in float3 H)
{
    float3 UpVector = abs(N.y) < 0.999f ? float3(0.0, 1.0, 0.0) : float3(0.0, 0.0, -1.0);
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
		
    float d = (CosTheta * m2 - CosTheta) * CosTheta + 1.0f;
    float D = m2 * rcp(d * d);
    float pdf = D * CosTheta;

	H = TangentToWorld(N, H);
	
    return float4(H, pdf);
}

//LVutner: UAVs. See CPP code
RWTexture2D<float4> u_sslr : register(u0);
RWTexture2D<float4> u_sslr_data : register(u1);

[numthreads(8, 8, 1)]
void main(uint3 DTid : SV_DispatchThreadID)
{
	//LVutner: Making my life easier.
	PSInputFullscreen I;
	I.hpos.xy = float2(DTid.xy) + 0.5; //half-pix
	I.hpos.zw = float2(0.0, 1.0);
	I.texcoord = I.hpos.xy * pos_decompression_params2.zw;

	IXrayGbuffer O;
    GbufferUnpack(I.texcoord.xy, I.hpos.xy, O);

	if(O.Depth >= 1.0f)
	{
		u_sslr[DTid.xy] = (0.0).xxxx;
		u_sslr_data[DTid.xy] = (0.0).xxxx;
		return;
	}

	//LVutner: Init
	float4 Final = (0.0).xxxx;
	float4 Point = (0.0).xxxx;	
	
	float3 ReflectPoint = GbufferGetPointRealUnjitter(I.texcoord.xy, O.Depth);
	float3 ViewVec = normalize(ReflectPoint);
	
	float2 Jitter = s_blue_noise[uint3(uint2(I.hpos.xy) % 128, uint(m_taa_jitter.w) % 32)].xy;
	
	Jitter.x = Jitter.x * 0.5f + 0.5f;
	Jitter.y *= 0.7f;
	
	float4 H = ImportanceSampleGGX(mul(m_invV, O.Normal.xyz), Jitter, O.Roughness);
	H.xyz = mul(m_V, H.xyz);
	
	float3 Reflection = reflect(ViewVec, H.xyz);
	
	if (dot(Reflection, O.Normal) < 0.0f)
    {
        Reflection = normalize(Reflection + O.Normal);
    }
	
	float3 StartPoint = ReflectPoint * 0.996f;
	Point.xyz = StartPoint + Reflection * fog_params.z;	
	
	bool isHUDRender = O.Depth < 0.02f;
	
#ifdef USE_OFFSCREEN_REFLECTIONS
	float4 VSLR = 0;
#endif

	if(!isHUDRender)
	{
		StartPoint += O.Normal * 0.025f;
		
#ifdef USE_OFFSCREEN_REFLECTIONS
		VSLR = FastViewReflections(mul(m_env_view, float4(StartPoint.xyz, 1.0f)).xyz, mul((float3x3)m_env_view, Reflection).xyz);
		Point.xyz = lerp(Point.xyz, mul(m_env_view_inv, float4(VSLR.xyz, 1.0f)).xyz, VSLR.w);
	} 
	else
	{
		VSLR.xyz = mul(m_env_view, float4(Point.xyz, 1.0f));
		Point.xyz = Reflection.xyz * s_env.SampleLevel(smp_linear, Point.xyz, 0.0f).w;
#endif
	}
	
	float4 SSLR = FastViewReflectionsSSR(StartPoint, Reflection, isHUDRender);
	
	float4 EndProj = mul(O.Depth < 0.02f ? m_P_hud : m_P, float4(SSLR.xyz, 1.0f));
	EndProj.xy = EndProj.xy * rcp(EndProj.w) * float2(0.5f, -0.5f) + 0.5f;
	
	float2 Velocity = s_velocity.SampleLevel(smp_nofilter, EndProj.xy, 0.0).xy * float2(0.5f, -0.5f);
	float2 PrevSpecularUV = saturate(EndProj.xy - Velocity.xy);
	
	Final = s_image.SampleLevel(smp_rtlinear, PrevSpecularUV.xy, 0.0);
	
#ifdef USE_OFFSCREEN_REFLECTIONS
	O.Hemi = isHUDRender ? 1.0f : saturate(O.Hemi * 3.0f);
#endif
	
	float4 Hemi = CompureSpecularIrradance(Reflection.xyz, O.Hemi, 0.0f).xyzz;
	SSLR.w *= GetBorderAtten(PrevSpecularUV);
	
#ifdef USE_OFFSCREEN_REFLECTIONS
	float3 Color = s_env.SampleLevel(smp_linear, VSLR.xyz, 0.0f);
	Color.xyz *= rcp(1.00001f - Color.xyz);
#else
	float3 Color = Hemi.xyz;
#endif
	
	Final.xyz = lerp(Color.xyz, Final.xyz, SSLR.w);
	Point.xyz = lerp(Point.xyz, SSLR.xyz, SSLR.w);
	Final.xyz = PopGamma(Final.xyz);
	
	Hemi.w = max(length(Point.xyz), length(StartPoint.xyz) + length(Point.xyz - StartPoint.xyz));
	Hemi.w = saturate(Hemi.w * fog_params.w + fog_params.x);
	
	Final.xyz = lerp(Final.xyz, Hemi.xyz, Hemi.w);
	Point.xyz = length(Point.xyz - StartPoint.xyz) * Reflection.xyz + ReflectPoint;
	
	Point.w = rcp(max(EPS_S, H.w));
	Final.xyz *= rcp(1.0f + Final.xyz);
	Final.xyz = saturate(Final.xyz);
	
	Final.w = isHUDRender;

	//LVutner: Write to UAVs
	u_sslr[DTid.xy] = Final;
	u_sslr_data[DTid.xy] = Point;
}