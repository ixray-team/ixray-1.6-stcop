#include "common.hlsli"
#include "mblur.hlsli"
#include "dof.hlsli"

Texture3D s_lut;

float4 autoexposure_params; // x - ps_r2_autoexposure_key, y - ps_r2_autoexposure_min, z - ps_r2_autoexposure_max, w - ps_r2_autoexposure_bias
float4 bloom_params; // x - ps_r2_bloom_amount, y - ps_r2_bloom_desaturation, z - ps_r2_bloom_tint_amount
float4 tonemap_params; // x - ps_r2_tonemap_compression, y - ps_r2_tonemap_desaturation, z - ps_r2_tonemap_crossfeed
float4 bloom_tint; // x - ps_r2_bloom_tint_color.r, y - ps_r2_bloom_tint_color.g, z - ps_r2_bloom_tint_color.b
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

float3 main(PSInputFullscreen I) : SV_Target
{
    float3 Color = max(0.0f, dof(I.texcoord));
    float4 Bloom = s_bloom.Sample(smp_rtlinear, I.texcoord);
	
    float Exposure = s_tonemap.Load(uint3(0, 0, 0)).x;
      
#ifndef USE_NEW_BLOOM // new bloom and tonemap will require using new adapt  
    #ifdef USE_CGIM_BLOOM_TWEAK 
	    Bloom = BrokeBloom(Bloom);
    #endif
	
    Color.xyz = Color.xyz + Bloom.xyz * 0.1666f * bloom_params.x;
	Color.xyz *= rcp(bloom_params.x + 1.0f);
#else
    Bloom = s_bloom.Sample(smp_rtlinear, I.texcoord);

    float Bloom_Luma = Luminance(Bloom.rgb);
    float3 Bloom_Desat = lerp(Bloom.rgb, Bloom_Luma.xxx, bloom_params.y);
	
    float Tint_Luma = max(Luminance(bloom_tint.rgb), 1e-4);
    float3 Tinted_Bloom = Bloom_Desat * bloom_tint.rgb / Tint_Luma;
	
    Bloom.rgb = lerp(Bloom_Desat, Tinted_Bloom, bloom_params.z);
	
    Color.rgb += bloom_params.x * Bloom.rgb;
#endif
    
#ifdef USE_NEW_ADAPT 
	// new adapt should work fine with vanilla tonemapping operator
    float adaptation_mult = 1.0f; // just in case we want to tweak the avg ratio
    // LogLumAvg = in log space, can be lower than 0, dont saturate or clamp it
    Exposure = log2(autoexposure_params.x) - Exposure * adaptation_mult;
    Exposure += autoexposure_params.w;
    Exposure = clamp(Exposure, autoexposure_params.y, autoexposure_params.z); // clip exposre to some reasonable range, can be tweaked or removed
    Exposure = exp2(Exposure);

    Color *= Exposure;
	
    Color.rgb = CommerceToneMapping(Color.rgb, tonemap_params.x, tonemap_params.y);
    Color.rgb = LinearToGamma(Color.rgb);
#else //USE_NEW_ADAPT
    Color = tonemap(Color, Exposure);
#endif

#ifdef USE_CROSSFEED
    Color.rgb = Crossfeed(Color.rgb, tonemap_params.z);
#endif
    
#ifdef USE_VIBRANCE
    Color.rgb = Vibrance(Color.rgb, tonemap_params.w);
#endif
    
#ifdef USE_CGIM_COLOR_TWEAK
	Color = Uncharted2Tonemap(Color);
#endif
	
#ifdef USE_LUT_TEXTURE
 	Color = s_lut.Sample(smp_rtlinear, saturate(Color)).xyz;
#endif
    
	return Color;
}

