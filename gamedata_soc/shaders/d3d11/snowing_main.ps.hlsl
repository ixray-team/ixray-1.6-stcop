#include "common.hlsli"
#include "sload.hlsli"

struct PSInput
{
	float4 hpos : SV_POSITION;
	float2 texcoord : TEXCOORD0;
};

RWTexture2D<unorm float4> u_color : register(u0);
RWTexture2D<unorm float4> u_normal : register(u1);
RWTexture2D<unorm float4> u_surface : register(u2);

void main(PSInput _I)
{	
	return;
}

