#include "common\common.hlsli"
#include "common\common_iostructs.hlsli"

// Editor helper geometry (axis, pivot): vertex colour only
float4 ps_main(p_TL I) : SV_Target
{
    return I.Color;
}
