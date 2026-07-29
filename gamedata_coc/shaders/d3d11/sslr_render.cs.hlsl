#include "common.hlsli"
#include "reflections.hlsli"
#include "metalic_roughness_ambient.hlsli"
#include "metalic_roughness_light.hlsli"

//LVutner: UAVs. See CPP code
RWTexture2D<float4> u_sslr : register(u0);
RWTexture2D<float4> u_sslr_data : register(u1);

[numthreads(8, 8, 1)]
void main(uint2 DTid : SV_DispatchThreadID, uint2 Gid : SV_GroupID, uint GI : SV_GroupIndex)
{
    IXRayGbuffer O = (IXRayGbuffer)NULL;
    GbufferUnpack(DTid, O);

	//LVutner: Init
	float4 Final = (0.0).xxxx;
	float4 Point = (0.0).xxxx;
	
	float2 TexCoord = float2(DTid.xy + 0.5) * pos_decompression_params2.zw;
	
	float3 ReflectPoint = GbufferGetPointRealUnjitter(TexCoord, O.Depth);
	float3 ViewVec = normalize(ReflectPoint);
	
	if(O.Depth >= 1.0f)
	{
		Point.xyz = ViewVec * fog_params.z;
		Point.w = EPS_S;
		
		Final.xyz = CompureSpecularIrradance(ViewVec.xyz, 1.0f, 0.2f).xyz;
		
		Final.xyz *= rcp(1.0f + Final.xyz);
		Final.xyz = saturate(Final.xyz);
		Final.w = 0.0f;
	
		u_sslr[DTid.xy] = Final;
		u_sslr_data[DTid.xy] = Point;
		
		return;
	}
	
	float2 Jitter = s_blue_noise[uint3(DTid % 128, uint(m_taa_jitter.w) % 32)].xy;

	//LVutner: VNDF is biased, cause I don't want random fireflies
	float4 H;
	
#ifndef USE_LEGACY_LIGHT
	H.xyz = sample_vndf_isotropic(O.Normal, -ViewVec, Jitter * float2(1.0, 0.7), O.Roughness * O.Roughness);
	H.w = pdf_vndf_isotropic(O.Normal, -ViewVec, reflect(ViewVec, H.xyz), O.Roughness * O.Roughness);
#else
	H.xyz = O.Normal;
	H.w = EPS;
#endif
	
	float3 Reflection = reflect(ViewVec, H.xyz);
	
	if (dot(Reflection, O.Normal) < 0.0f)
    {
       Reflection = normalize(Reflection + O.Normal);
    }
	
	float3 StartPoint = ReflectPoint;
	
#ifdef USE_OFFSCREEN_REFLECTIONS
	float ReflectDist = s_env_dist.SampleLevel(smp_linear, StartPoint.xyz, 0.0f).x;
	
	ReflectDist /= max(EPS, length(StartPoint));
	StartPoint *= min(1.0f, ReflectDist);
#endif
	
	StartPoint *= 0.996f;
	
	Point.xyz = StartPoint + Reflection * fog_params.z;	
	
	bool isHUDRender = O.Depth < 0.02f;

	StartPoint += !isHUDRender ? O.Normal * 0.025f : 0.0f;
	float4 SSLR = FastViewReflectionsSSR(StartPoint, Reflection, isHUDRender);
	
	float4 EndProj = mul(O.Depth < 0.02f ? m_P_hud : m_P, float4(SSLR.xyz, 1.0f));
	EndProj.xy = EndProj.xy * rcp(EndProj.w) * float2(0.5f, -0.5f) + 0.5f;
	
	EndProj.xy += s_velocity.SampleLevel(smp_rtlinear, EndProj.xy, 0).xy * float2(-0.5f, 0.5f);
	SSLR *= GetBorderAtten(EndProj.xy);
	
	Final = s_image.SampleLevel(smp_rtlinear, EndProj.xy, 0.0);
	
	if(!isHUDRender)
	{
#ifdef USE_OFFSCREEN_REFLECTIONS
		if(SSLR.w < 1.0f)
		{
			float4 VSLR = FastViewReflections(StartPoint.xyz, Reflection.xyz);
			Point.xyz = lerp(Point.xyz, VSLR.xyz, VSLR.w);
		}
	} 
	else
	{
		Point.xyz = Reflection.xyz * s_env_dist.SampleLevel(smp_linear, Point.xyz, 0.0f).x;
#endif
	}
	
#ifdef USE_OFFSCREEN_REFLECTIONS
	O.Hemi = 1.0f; // isHUDRender ? 1.0f : saturate(O.Hemi * 3.0f);
#endif
	
	float4 Hemi = CompureSpecularIrradance(Reflection.xyz, O.Hemi, 0.0f).xyzz;
	
	if(SSLR.w < 1.0f)
	{
#ifdef USE_OFFSCREEN_REFLECTIONS
		float3 Color = s_env.SampleLevel(smp_linear, Point.xyz, 0.0f);
#else
		float3 Color = Hemi.xyz;
#endif

		Final.xyz = lerp(Color.xyz, Final.xyz, SSLR.w);
	}
	
	Point.xyz = lerp(Point.xyz, SSLR.xyz, SSLR.w);
	Final.xyz = LinearToGamma(Final.xyz);
	
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