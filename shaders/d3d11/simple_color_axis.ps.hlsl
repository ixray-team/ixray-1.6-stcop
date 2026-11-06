#include "common.hlsli"
#include "common_iostructs.hlsli"

// Editor helper geometry (axis, pivot): vertex colour only
float4 main(p_TL I) : SV_Target
{
    return I.Color;
}
