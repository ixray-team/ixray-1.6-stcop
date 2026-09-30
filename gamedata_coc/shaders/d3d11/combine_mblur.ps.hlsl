#include "common.hlsli"

#define MBLUR_SAMPLES 6

inline void SampleImage(inout float4 Final, in float2 SampleUV, in float CenterDepth)
{
	float SampleWeight = s_position.SampleLevel(smp_rtlinear, SampleUV, 0).x;
	SampleWeight = saturate(1.0f - 8.0f * abs(SampleWeight - CenterDepth) * rcp(max(SampleWeight, CenterDepth)));
 	SampleWeight *= GetBorderAtten(SampleUV);
	
	float3 Color = s_image.SampleLevel(smp_rtlinear, SampleUV, 0).xyz;
	Color *= rcp(1.0f + Color);
	
	Final.xyz += SampleWeight * Color;
	Final.w += SampleWeight;
}

void main(in PSInputFullscreen I, out float3 Color : SV_Target)
{	
	float2 Vel = 0;
	
	Vel += s_velocity.SampleLevel(smp_rtlinear, I.texcoord.xy + float2(+0.5f, -0.5f) * mblur_params.zw, 0).xy;
	Vel += s_velocity.SampleLevel(smp_rtlinear, I.texcoord.xy + float2(+0.5f, +0.5f) * mblur_params.zw, 0).xy;
	Vel += s_velocity.SampleLevel(smp_rtlinear, I.texcoord.xy + float2(-0.5f, -0.5f) * mblur_params.zw, 0).xy;
	Vel += s_velocity.SampleLevel(smp_rtlinear, I.texcoord.xy + float2(-0.5f, +0.5f) * mblur_params.zw, 0).xy;
	
	// AVG velocty with "centered" inf
	Vel *= 0.25f * float2(-0.5f, 0.5f);
	
	float CenterDepth = s_position.SampleLevel(smp_rtlinear, I.texcoord.xy, 0).x;
	
	float4 Final = s_image.SampleLevel(smp_rtlinear, I.texcoord.xy, 0);
	Final.xyz *= rcp(1.0f + Final.xyz);
	
	Final.w = 1.0f;
	
	float PixelSize = max(mblur_params.z, mblur_params.w);
	float VelSize = dot(Vel, Vel); PixelSize *= PixelSize;
	
	float Step = mblur_params.x * rcp(MBLUR_SAMPLES);
	float L = 0.0f;
	
	// Add some noise to get a "dirty" result
	Step *= Hash(I.texcoord.xy * m_taa_jitter.z * mblur_params.y) * 0.2f + 0.9f;
	
	if(VelSize > PixelSize)
	{
		[loop]
		for(uint i = 0; i < MBLUR_SAMPLES; ++i)
		{
			L += Step;
			
			SampleImage(Final, saturate(I.texcoord.xy + Vel * L), CenterDepth);
		 	SampleImage(Final, saturate(I.texcoord.xy - Vel * L), CenterDepth);
		}
	}
	
	Color = Final.xyz * rcp(Final.w);
	Color = saturate(Color);
	
	Color *= rcp(max(0.00001f, 1.0f - Color));
}

