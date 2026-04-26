/*
Made by Papa Doenitz for IX-ray engine 2026-03-05
CC BY-NC-SA 4.0 Lisence https://creativecommons.org/licenses/by-nc-sa/4.0/

Credits goes to:
Bruno Opsenica https://bruop.github.io/exposure/
Krzysztof Narkowicz https://knarkowicz.wordpress.com/2016/01/09/automatic-exposure/
The Real MJP https://mynameismjp.wordpress.com/2011/08/10/average-luminance-compute-shader/
*/

#include "common.hlsli"

float main(PSInputFullscreen I) : SV_Target
{
    float3 Color = s_image.Sample(smp_rtlinear, I.texcoord.xy).rgb;
    float Final = dot(Color, LUMINANCE_VECTOR);
	
#ifndef USE_CLASSIQUE_TONEMAP
    Final = max(Final, 0.001); // just protec low before log
    Final = log2(Final);
#endif

    return Final;
}

