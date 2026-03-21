#include "common.hlsli"
#include "mblur.hlsli"
#include "dof.hlsli"

Texture3D s_lut;

float4 autoexposure_params; // x - ps_r2_autoexposure_key, y - ps_r2_autoexposure_min, z - ps_r2_autoexposure_max, w - ps_r2_autoexposure_bias
float4 bloom_params; // x - ps_r2_bloom_amount, y - ps_r2_bloom_desaturation, z - ps_r2_bloom_tint_amount
float4 tonemap_params; // x - ps_r2_tonemap_compression, y - ps_r2_tonemap_desaturation, z - ps_r2_tonemap_crossfeed
float4 bloom_tint; // x - ps_r2_bloom_tint_color.r, y - ps_r2_bloom_tint_color.g, z - ps_r2_bloom_tint_color.b
//float4 fog_color;
/*
constants buffer descr:
    autoexposure_key - commonly used value for middle gray, used as anchor point for exposure calculation, UE uses 0.148f, can be tweaked
    autoexposure_min - minimum exposure in f-stops, can be tweaked
    autoexposure_max - maximum exposure in f-stops, can be tweaked
    autoexposure_bias - preexposure bias in f-stops, can be used to tweak overall brightness
    bloom_amount - strength of bloom effect, can be tweaked
    bloom_desaturation - how much bloom should be desaturated, 0 - no desaturation, 1 - full desaturation, can be tweaked
    bloom_tint_amount - how much bloom should be tinted, 0 - no tint, 1 - full tint, can be tweaked
    bloom_tint_color - color to tint bloom with, can be tweaked
    tonemap_compression - how much to compress highlights, higher value means later compression (longer linear part), can be tweaked
    tonemap_desaturation - how much to desaturate highlights, can be tweaked
    tonemap_crossfeed - how much to mix color channels, can be tweaked
    tonemap_vibrance - how much to boost vibrance, can be tweaked
*/

//#define USE_NEW_ADAPT
//#define USE_NEW_BLOOM_TONEMAP
//#define USE_CROSSFEED
//#define USE_VIBRANCE
//#define USE_LUT_TEXTURE

float exposure(float ev100) {
    return 1.0 / (pow(2.0, ev100) * 1.2);
}


float3 main(PSInputFullscreen I) : SV_Target
{
    float3 Color = max(0.0f, dof(I.texcoord));
    float Depth = 1.0f - s_position.Sample(smp_nofilter, I.texcoord.xy);
    float3 Point = GbufferGetPointRealUnjitter(I.texcoord, Depth);
    float3 wPoint = mul(m_invV, float4(Point, 1.0));
    float ViewDist = length(Point);
    float4 Bloom = 0.f;
    float Exposure = 0.f;
    float Fog;
    
#ifdef USE_NEW_ADAPT // new adapt should work fine with vanilla tonemapping operator
    float adaptation_mult = 1.0f; // just in case we want to tweak the avg ratio
    // LogLumAvg = in log space, can be lower than 0, dont saturate or clamp it
    float LogLumAvg = s_adapt[uint2(0, 0)]; //s_adapt.Sample(smp_nofilter, 0.5f).x; //s_adapt.Load(uint3(0, 0, 0)).x;
    Exposure = log2(autoexposure_params.x) - LogLumAvg * adaptation_mult;
    Exposure += autoexposure_params.w;
    Exposure = clamp(Exposure, autoexposure_params.y, autoexposure_params.z); // clip exposre to some reasonable range, can be tweaked or removed
    Exposure = exp2(Exposure);
#else //USE_NEW_ADAPT
    Exposure = s_tonemap.Sample(smp_nofilter, float2(0.5f, 0.5f)).x;
#endif
      
#ifndef NEW_FOGGIN
    Fog = (saturate(ViewDist * fog_params.w + fog_params.x));
#else  //NEW_FOGGIN
    float a = 1.0f;
    float b = 0.001f;
    float denom = a - exp(-b * (fog_params.z - fog_params.y));
    Fog = (a - exp(-b * (ViewDist)));
    Fog = smoothstep(0.f, 1.f, saturate(Fog));
#endif

#ifndef USE_NEW_BLOOM_TONEMAP // new bloom and tonemap will require using new adapt  
    Bloom = s_bloom.Sample(smp_rtlinear, I.texcoord);
    #ifdef USE_CGIM_BLOOM_TWEAK 
	    //Bloom = BrokeBloom(Bloom);
    #endif //USE_CGIM_BLOOM_TWEAK  
    Color = tonemap(Color, Exposure);
    Color = combine_bloom(Color, Bloom).xyz;
#else   //USE_NEW_BLOOM_TONEMAP_FOG
    Bloom = max(1e-5, n_bloom.Sample(smp_rtlinear, I.texcoord));
    float Bloom_Luma = Luminance(Bloom.rgb);
    float Color_Luma = Luminance(Color.rgb);
    //Bloom *= Color_Luma / (Bloom_Luma + 1e-5);
    float3 Bloom_Desat = lerp(Bloom.rgb, Bloom_Luma.xxx, bloom_params.y);
    float Tint_Luma = max(Luminance(bloom_tint.rgb), 1e-4);
    float3 Tinted_Bloom = Bloom_Desat * bloom_tint.rgb / Tint_Luma;
    Bloom.rgb = lerp(Bloom_Desat, Tinted_Bloom, bloom_params.z);
    Color.rgb += bloom_params.x * Bloom.rgb;
    float3 temp_f_col = max(fog_color, 0.004.xxx);
    float LumaTemp = Luminance(temp_f_col);
    float3 Fog_Bloom = Bloom_Desat * temp_f_col / max(LumaTemp, 1e-4).xxx;
    temp_f_col *= Bloom_Luma / max(LumaTemp, 1e-4);
    // experimental - will return later
    //Color = lerp(Color, Fog_Bloom, Fog);

    Color *= Exposure;
    Color.rgb = CommerceToneMapping(Color.rgb, tonemap_params.x, tonemap_params.y);
    //Color.rgb = Uncharted2Tonemap(Color.rgb);
    //Color.rgb = 1.f - exp(-Color.rgb);

#ifdef USE_CROSSFEED
    Color.rgb = Crossfeed(Color.rgb, tonemap_params.z);
#endif
    
#ifdef USE_VIBRANCE
    Color.rgb = Vibrance(Color.rgb, tonemap_params.w);
#endif
/*    
#ifdef USE_CGIM_COLOR_TWEAK // didnt test with new tonemap
	Color = Uncharted2Tonemap(Color);
#endif
	
#ifdef USE_LUT_TEXTURE // didnt test
 	Color = s_lut.Sample(smp_rtlinear, saturate(Color)).xyz;
#endif
*/

    Color.rgb = PopGamma(saturate(Color.rgb));
#endif //USE_NEW_BLOOM_TONEMAP_FOG

    
    //Color.rgb = wPoint.xyz / (wPoint.xyz + 1000.0);
    //Color.rgb = Color.rgb * 0.5 + 0.5;
	return float4(Color.rgb, 1.0f);
}

