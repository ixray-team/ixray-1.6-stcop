#include "common.hlsli"

Texture2D s_base0;
Texture2D s_base1;
Texture2D s_noise;

uniform float4 c_brightness;

// Pixel
float4 main(p_postpr I) : SV_Target
{
    float3 t_0 = saturate(s_base0.Sample(smp_rtlinear, I.Tex0.xy).xyz);
    float3 t_1 = saturate(s_base1.Sample(smp_rtlinear, I.Tex1.xy).xyz);
    float3 image = (t_0 + t_1) * 0.5f;
	
    float gray = dot(image, I.Gray.xyz);
    image = lerp(gray, image, I.Gray.w);

    float3 t_noise = s_noise.Sample(smp_linear, I.Tex2.xy).xyz;
    float3 noised = image * t_noise * 2.0f;
	
    image = lerp(noised, image, I.Color.w);
    image = (image * I.Color.xyz + c_brightness.xyz) * 2.0f;

    image = deband_color(image, I.Tex0.xy);
    return float4(image, 1.0h);
}

