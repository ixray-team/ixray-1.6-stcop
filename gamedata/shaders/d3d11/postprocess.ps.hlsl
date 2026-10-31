#include "common.hlsli"
#include "scope_postprocess.hlsli"

Texture2D s_base0 : register(t49);
Texture2D s_base1 : register(t50);
Texture2D s_noise : register(t26);


// Pixel
float4 main(p_postpr I) : SV_Target
{
    float3 t_0 = PreparePostprocessColor(s_base0.Sample(smp_rtlinear, I.Tex0.xy).xyz);
    float3 t_1 = PreparePostprocessColor(s_base1.Sample(smp_rtlinear, I.Tex1.xy).xyz);
    float3 image = (t_0 + t_1) * 0.5f;
	
    float gray = dot(image, I.Gray.xyz);
    image = lerp(gray, image, I.Gray.w);

    float3 t_noise = s_noise.SampleLevel(smp_linear, I.Tex2.xy, 0).xyz;
    float3 noised = image * t_noise * 2.0f;
	
    image = lerp(noised, image, I.Color.w);
    image = (image * I.Color.xyz + c_brightness.xyz) * 2.0f;

    return float4(RestorePostprocessColor(image), 1.0h);
}

