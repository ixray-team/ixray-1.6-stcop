#include "common.hlsli"
#include "reflections.hlsli"
#include "metalic_roughness_ambient.hlsli"
#include "metalic_roughness_light.hlsli"

struct PSInput
{
    float4 hpos : SV_POSITION;
    float2 texcoord : TEXCOORD0;
};

#define VSGI_SAMPLES 30

float4 main(PSInput I) : SV_Target
{
    IXrayGbuffer O;
    GbufferUnpack(I.texcoord.xy, I.hpos.xy, O);	

	if(O.Depth >= 1.0f) {
		return float2(0.0f, O.Depth).xxxy;
	}

	float3 ViewD = -O.View; float3 ViewR, ViewN = float2(0.0, 1.0).xyx;
	
    ViewR = normalize(cross(ViewN, ViewD));
    ViewN = normalize(cross(ViewD, ViewR));
	
	float4 Jitter = s_blue_noise[uint3(uint2(I.hpos.xy) % 128, uint(m_taa_jitter.w) % 32)] - 0.5f;
	
	float2 Direction = 0.0f;
	sincos(Jitter.y * 6.2831853f, Direction.x, Direction.y);

	float3 LastSample = O.PointReal;
	float Step = rcp(VSGI_SAMPLES + 2);
	float L = Step * 0.5f;
	
	float4 Global = 0.0f;
	Global.w = 0.00001f;
	
	float MaxDelta = -1.0f;
	
	for(uint i = 1; i <= VSGI_SAMPLES; ++i)
	{
		L += Step * lerp(0.5f, 1.5f, Hash(dot(sin(LastSample.xyz * timers.x), float3(12.989, 42.364, 78.233))));
		
		float3 Sample = UnPackNormalVector(Direction * L);
		Sample = Sample.x * ViewR + Sample.y * ViewN + Sample.z * ViewD;
		
		float envDepth = s_env_depth.SampleLevel(smp_rtlinear, Sample, 0).x;
		
		float4 EndProj = mul(m_P, float4(Sample.xyz, 1.0f));
		EndProj.xy = EndProj.xy * rcp(EndProj.w) * float2(0.5f, -0.5f) + 0.5f;
					
		float2 vel = s_velocity.Sample(smp_rtlinear, EndProj.xy).xy * float2(0.5f, -0.5f);
		float2 PrevSpecularUV = saturate(EndProj.xy - vel);
		float Fade = 1.0f * (Sample.z > 0.0f ? GetBorderAtten(EndProj.xy) * GetBorderAtten(PrevSpecularUV) : 0.0f);
		float3 Point = GbufferGetPointRealUnjitter(EndProj.xy);		
		Sample = cubemap_depth_to_vector(Sample, envDepth);
		
		float ErrFade = 1.0f - saturate(length(Point - Sample) * 9.0f - 0.2f);
		Sample = lerp(Sample, Point, Fade);
		
		float3 Normal = normalize(cross(ddx(Sample), ddy(Sample)));
		
		float3 LDir = Sample - O.PointReal;
		float Shadow = dot(LDir, LDir);
		float Scale = rsqrt(Shadow);
		float Delta = dot(LDir * Scale, O.Normal);
		float Weight = length(Sample); //length(LastSample - Sample);
		
		if(Delta >= MaxDelta && rcp(Scale) > 0.01f) {
			float3 envColor = s_env.SampleLevel(smp_rtlinear, Sample, 0).xyz;
			envColor *= rcp(1.00001f - envColor);
			envColor.xyz = PopGamma(envColor.xyz);

			float3 Image = s_image.Sample(smp_rtlinear, PrevSpecularUV.xy).xyz;
			Image.xyz = PopGamma(Image.xyz);
			
			Image = lerp(Image, envColor, ErrFade * 0.1f);
			envColor = lerp(envColor, Image, Fade);
		
			float Fog = 1.0f - saturate(dot(Normal, -LDir * Scale) * 100);
			Fog *= 1.0f - saturate(rcp(Scale) * fog_params.w + fog_params.x);
			
			Global.xyz += rcp(Shadow + 1.0f) * envColor * Weight * saturate(Delta) * Fog;
			MaxDelta = Delta;
				
		}
		
		Global.w += Weight;	
		LastSample = Sample;
	}
	
	Global.xyz *= rcp(1.0f + Global.xyz);
	return float4(Global.xyz, O.Depth);
}

