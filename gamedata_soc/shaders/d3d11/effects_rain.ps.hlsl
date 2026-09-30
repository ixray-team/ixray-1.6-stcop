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
	O.Color.xyz = GammaToLinear(O.Color.xyz * 0.8f);
 
    float3 normal = normalize(cross(ddy(I.World), ddx(I.World)));
	float3 irradiance = CompureDiffuseIrradance(normal, float3(1, 1, 1));

	O.Color.w *= min(max(dot(irradiance, irradiance) * 1.3, 0.5f), 0.8f);
	O.Color.xyz = irradiance * irradiance;
#ifndef DISABLE_MOTION_VECTORS
	O.Velocity = 0.0f;
#endif
}

