#include "common.hlsli"

uniform float4 mblur_params;
#define MBLUR_SAMPLES 6.0f

inline void SampleImage(inout float4 Final, in float2 SampleUV, in float CenterDepth)
{
	float SampleWeight = s_position.SampleLevel(smp_rtlinear, SampleUV, 0).x;
	SampleWeight = saturate(1.0f - 8.0f * abs(SampleWeight - CenterDepth) * rcp(max(SampleWeight, CenterDepth)));
 	SampleWeight *= GetBorderAtten(SampleUV);
	
	Final.xyz += SampleWeight * s_image.SampleLevel(smp_rtlinear, SampleUV, 0).xyz;
	Final.w += SampleWeight;
}

void main(in PSInputFullscreen I, out float4 Color : SV_Target)
{	
	float2 Vel = 0;
	
	Vel += s_velocity.SampleLevel(smp_rtlinear, I.texcoord.xy + float2(+0.5f, -0.5f) * mblur_params.zw, 0);
	Vel += s_velocity.SampleLevel(smp_rtlinear, I.texcoord.xy + float2(+0.5f, +0.5f) * mblur_params.zw, 0);
	Vel += s_velocity.SampleLevel(smp_rtlinear, I.texcoord.xy + float2(-0.5f, -0.5f) * mblur_params.zw, 0);
	Vel += s_velocity.SampleLevel(smp_rtlinear, I.texcoord.xy + float2(-0.5f, +0.5f) * mblur_params.zw, 0);
	
	// AVG velocty with "centered" inf
	Vel *= 0.25f * float2(-0.5f, 0.5f);
	
	// Add some noise to get a "dirty" result
	Vel *= Hash(I.texcoord.xy * m_taa_jitter.z * mblur_params.z) * 0.1f + 0.95f;
	
	float CenterDepth = s_position.SampleLevel(smp_rtlinear, I.texcoord.xy, 0).x;
	
	float4 Final = s_image.SampleLevel(smp_rtlinear, I.texcoord.xy, 0);
	Final.w = 1.0f;
	
	float PixelSize = max(mblur_params.z, mblur_params.w);
	float VelSize = dot(Vel, Vel); PixelSize *= PixelSize;
	
	float Step = mblur_params.x * rcp(MBLUR_SAMPLES);
	float L = 0.0f;
	
	if(VelSize > PixelSize)
	{
		[unroll(MBLUR_SAMPLES)]
		for(uint i = 0; i < MBLUR_SAMPLES; ++i)
		{
			L += Step;
			
			SampleImage(Final, saturate(I.texcoord.xy + Vel * L), CenterDepth);
		 	SampleImage(Final, saturate(I.texcoord.xy - Vel * L), CenterDepth);
		}
	}
	
	Color = Final * rcp(Final.w);
}

