/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
#include "common\common.hlsli"

struct v2p
{
    float2 Tex0 : TEXCOORD0;
    float3 Point : TEXCOORD1;
	float3 World : TEXCOORD2;

    float4 Color : COLOR;
    float4 HPos : SV_POSITION;
};

// Vertex
void vs_main(PULL_INPUT, out v2p O)
{
	RingVertex V = PullRing(VertexId);
	float4 P = float4(V.P, 1.0f);

	O.Point = mul(m_WV, P);
    O.HPos = mul(m_WVP, P);
	O.World = mul(m_W, P);
	
    O.HPos.xy += m_taa_jitter.xy * O.HPos.w;
	
    O.Tex0 = V.UV;
    O.Color = V.C;
}

#include "common\metalic_roughness_ambient.hlsli"

void ps_main(in v2p I, out IXRayForward O)
{
	O = (IXRayForward)0;
    float2 ScreenUV = I.HPos.xy * scaled_screen_res.zw;
    float3 Bloom = s_bloom.SampleLevel(smp_rtlinear, ScreenUV, 0).rgb;
    float3 Scene = s_image.SampleLevel(smp_rtlinear, ScreenUV, 0).rgb;
    
    float2 RefrUV = 1.0f - ScreenUV;
    
    //float Noise = Hash(ScreenUV.xy); // use later
    float Radius = 0.05f; // use noise later - need to provide noise per droplet from backend
    float2 P = (I.Tex0 - 0.5f) / Radius; //I.Tex0 * 2.0f - 1.0f;    
    float Rsq = dot(P, P);
    float Edge = max(fwidth(Rsq), 1e-4f);
    float Coverage = 1.0f - smoothstep(0.5f, 1.0f, Rsq);
    
    float3 normal = float3(P.y, sqrt(saturate(1.0f - Rsq)), P.x);
    float3 NormalQuad = normalize(cross(ddy(I.World), ddx(I.World)));
    float h = sqrt(saturate(1.0f - Rsq));
    float3 Ph = I.World + NormalQuad * h * 0.05f; // 0.05f is the radius of the droplet
    float3 DropletNormal = normalize(cross(ddy(Ph), ddx(Ph)));
	
    float3 irradiance = CompureDiffuseIrradance(normal, I.Color.xyz);
    float3 reflected = CompureSpecularIrradance(reflect(-I.Point.xyz, normal), I.Color.xyz, 0.0);
    //O.Color.xyz = irradiance * irradiance;

    //float3 HalfVector = float3(-0.32f, 0.41f, -0.86f);
    //float Specular = pow(saturate(dot(normal, HalfVector)), 32.0f);
  
    float NdotV = saturate(-NormalQuad.z);
    float Grazing = 1.0f - NdotV;
    float Grazing2 = Grazing * Grazing;
    float Fresnel = saturate(0.02f + 0.98f * Grazing2 * Grazing2 * Grazing);
    

    
    //float2 RefrUV = NormalQuad.xy * float2(20.0f, -20.0f) * NdotV * scaled_screen_res.zw;
    
    float3 Refracted = 1.0 * s_image.SampleLevel(smp_rtlinear, RefrUV, 0).rgb; // ScreenUV + RefrUV
    
    float3 Reflected = reflected + Bloom * 1.0f;
    
    float ReflWeight = Fresnel;
    float RefrWeight = 1.0f - Fresnel;
    
    float Opacity = ReflWeight * RefrWeight;
    
    //O.Color.xyz += Specular * float3(1.0f, 0.95f, 0.85f) * 3.0f;
    //O.Color.xyz = Fresnel * 1.0; //Specular * 1.f; //float3(I.Tex0.xy, 0.0f);
    
    O.Color.rgb = (reflected * ReflWeight + Refracted * RefrWeight) / max(Opacity, 1e-5f);
    //O.Color.rgb *= O.Color.rgb;
    O.Color.a = Coverage * Opacity;

	O.Color.w *= min(max(dot(irradiance, irradiance) * 1.3, 0.5f), 0.8f);

#ifndef DISABLE_MOTION_VECTORS
	O.Velocity = 0.0f;
#endif
}
