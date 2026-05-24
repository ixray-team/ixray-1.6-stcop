/*
Made by Papa Doenitz for IX-ray engine 2026-03-05
CC BY-NC-SA 4.0 Lisence https://creativecommons.org/licenses/by-nc-sa/4.0/

Credits goes to:
Bruno Opsenica https://bruop.github.io/exposure/
Krzysztof Narkowicz https://knarkowicz.wordpress.com/2016/01/09/automatic-exposure/
The Real MJP https://mynameismjp.wordpress.com/2011/08/10/average-luminance-compute-shader/
*/

#include "common.hlsli"

uniform Texture2D b_image;
float4 adapt_params;

float main(PSInputFullscreen I) : SV_Target
{   
    float res = 0.f;
    float2 center = I.texcoord.xy ;
    float3 a = b_image.Sample(smp_rtlinear, float2 (center.x, center.y)).rgb;
    res = dot(a, LUMINANCE_VECTOR);
    res = clamp(res, 0.0065, 2.0); // just protec low before log, slightly shifts final exposure lower
    res = log2(res);

    return res;
}
