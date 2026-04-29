#include "common.hlsli"

struct v2p
{
    float2 Tex0 : TEXCOORD0;
    float3 Point : TEXCOORD1;
	
    float4 Color : COLOR;
    float4 HPos : SV_POSITION;
};

void main(in v2p I, out IXRayForward O)
{
	O.Color = s_base.Sample(smp_base, I.Tex0) * I.Color;
	O.Color.xyz = detonemap(O.Color.xyz * 0.8f);

#ifndef DISABLE_MOTION_VECTORS
	O.Velocity = 0.0f;
#endif
}

