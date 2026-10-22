#include "common.hlsli"

#include "metalic_roughness_ambient.hlsli"

struct v2p
{
    float2 Tex0 : TEXCOORD0;
    float3 Point : TEXCOORD1;
	float3 World : TEXCOORD2;
	
    float4 Color : COLOR;
    float4 HPos : SV_POSITION;
};

void main(in v2p I, out IXRayForward O)
{
	O = (IXRayForward)0;
	O.Color = s_base.Sample(smp_base, I.Tex0) * I.Color;
	O.Color.xyz = O.Color.xyz * 1.0f;
 
    //float3 normal = normalize(cross(ddy(I.World), ddx(I.World)));
    float2 P = (I.Tex0 * 2.0f - 1.0f) * 0.7f;
    float3 normal = float3(P.x, -P.y, -sqrt(saturate(1.0f - dot(P, P))));
	float3 irradiance = CompureDiffuseIrradance(normal, float3(0.5, 0.5, 0.5));

	O.Color.xyz = irradiance * irradiance;

    float3 HalfVector = float3(-0.32f, 0.41f, -0.86f);
    float Specular = pow(saturate(dot(normal, HalfVector)), 32.0f);
   
    float2 ScreenUV = I.HPos.xy * scaled_screen_res.zw;
    float3 Bloom = s_bloom.SampleLevel(smp_rtlinear, ScreenUV, 0).rgb;

    float3 ViewDir = normalize(-I.Point);
    float NdotV = saturate(dot(normal, ViewDir));
    float Grazing = 1.0f - NdotV;
    float Grazing2 = Grazing * Grazing;
    float Fresnel = saturate(0.02f + 0.98f * Grazing2 * Grazing2 * Grazing);
    
    O.Color.rgb = Fresnel * Bloom * 1.1f;
    
    //O.Color.xyz += Specular * float3(1.0f, 0.95f, 0.85f) * 3.0f;
    //O.Color.xyz = Fresnel * 1.0; //Specular * 1.f; //float3(I.Tex0.xy, 0.0f);
    
#ifdef USE_LEGACY_LIGHT
	O.Color.w *= min(max(dot(irradiance, irradiance) * 1.3, 0.5f), 0.8f);
#else
    O.Color.w *= 0.9f;
#endif
    
#ifndef DISABLE_MOTION_VECTORS
	O.Velocity = 0.0f;
#endif
}

