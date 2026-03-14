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

//https://auzaiffe.wordpress.com/2024/04/15/vndf-importance-sampling-an-isotropic-distribution/
float3 sample_vndf_isotropic(float3 n, float3 wi, float2 u, float alpha)
{
    // decompose the floattor in parallel and perpendicular components
    float3 wi_z = -n * dot(wi, n);
    float3 wi_xy = wi + wi_z;
 
    // warp to the hemisphere configuration
    float3 wiStd = -normalize(alpha * wi_xy + wi_z);
 
    // sample a spherical cap in (-wiStd.z, 1]
    float wiStd_z = dot(wiStd, n);
    float z = 1.0 - u.y * (1.0 + wiStd_z);
    float sinTheta = sqrt(saturate(1.0f - z * z));
    float phi = (2.0 * PI) * u.x - PI;
    float x = sinTheta * cos(phi);
    float y = sinTheta * sin(phi);
    float3 cStd = float3(x, y, z);
 
    // reflect sample to align with normal
    float3 up = float3(0, 0, 1.000001); // Used for the singularity
    float3 wr = n + up;
    float3 c = dot(wr, cStd) * wr / wr.z - cStd;
 
    // compute halfway direction as standard normal
    float3 wmStd = c + wiStd;
    float3 wmStd_z = n * dot(n, wmStd);
    float3 wmStd_xy = wmStd_z - wmStd;

    return normalize(alpha * wmStd_xy + wmStd_z);
}

float pdf_vndf_isotropic(float3 n, float3 wi, float3 wo, float alpha)
{
    float alphaSquare = alpha * alpha;
    float3 wm = normalize(wo + wi);
    float zm = dot(wm, n);
    float zi = dot(wi, n);
    float nrm = rsqrt((zi * zi) * (1.0f - alphaSquare) + alphaSquare);
    float sigmaStd = (zi * nrm) * 0.5f + 0.5f;
    float sigmaI = sigmaStd / nrm;
    float nrmN = (zm * zm) * (alphaSquare - 1.0f) + 1.0f;
    return alphaSquare / (PI * 4.0f * nrmN * nrmN * sigmaI);
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

	float4 H;
	H.xyz = sample_vndf_isotropic(O.Normal, -ViewVec, Jitter * float2(1.0, 0.7), O.Roughness * O.Roughness);
	H.w = pdf_vndf_isotropic(O.Normal, -ViewVec, reflect(ViewVec, H.xyz), O.Roughness * O.Roughness);
	
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