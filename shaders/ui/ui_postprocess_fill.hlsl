#include "common\common.hlsli"

// An icon's place in the interface's icon target before the icon is drawn: transparent, as the composite reads it
float4 ps_main(PSInputFullscreen input) : SV_Target
{
    return (float4)0;
}
