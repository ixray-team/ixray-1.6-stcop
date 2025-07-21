#include "common.hlsli"

float4 main(p_shadow I) : COLOR0
{
#ifdef USE_AREF
    float4 C = tex2D(s_base, I.tc0);
    clip(C.w - def_aref);
#endif

#ifdef USE_HWSMAP
    return 0;
#else
    return I.depth;
#endif
}
