#include "common.hlsli"

float4 main(float4 P : POSITION) : SV_POSITION
{
    return mul(m_WVP, P);
}


