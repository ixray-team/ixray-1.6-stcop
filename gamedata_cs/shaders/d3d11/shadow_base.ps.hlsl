#include "common.hlsli"

float4 main(p_shadow I) : SV_Target
{
#ifdef USE_AREF
    float4 C = s_base.Sample(smp_linear, I.tc0);
    clip(C.w - def_aref);
#elif USE_TRANSPARENT
    float4 C = s_base.Sample(smp_linear, I.tc0);
	
#ifdef USE_LEGACY_LIGHT
	C.xyz = 0.2f + C.xyz * 0.8f;
#else
	C.xyz = 0.04f + C.xyz * C.xyz * 0.96f;
#endif

	C *= 1.0f - C.w;
	
	return C;
#endif

    return 0;
}

