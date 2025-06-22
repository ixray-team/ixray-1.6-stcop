#include "common.hlsli"
#include "sload.hlsli"

struct p_particle
{
    float4 color : COLOR0;
};

float4 main(p_particle II) : SV_Target0
{
    discard;
    return II.color;
}

// THIS SHADER SHOD BE DELEATED OR FIXED
