#include "common.hlsli"

float4 main(float4 C : COLOR0) : SV_Target
{
    return PushGamma(C);
}

