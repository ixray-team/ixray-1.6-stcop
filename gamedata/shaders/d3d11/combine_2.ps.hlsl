#include "common.hlsli"
#include "mblur.hlsli"
#include "dof.hlsli"
#include "autoexposure.hlsli"

Texture3D s_lut;
Texture2D s_bloom_compute;
Texture2D<float> s_tonemap_compute;

float4 bloom_params; // x - ps_r2_bloom_amount, y - ps_r2_bloom_desaturation, z - ps_r2_bloom_tint_amount, w - use compute bloom
float4 tonemap_params; // x - ps_r2_tonemap_compression, y - ps_r2_tonemap_desaturation, z - ps_r2_tonemap_crossfeed
float4 bloom_tint; // x - ps_r2_bloom_tint_color.r, y - ps_r2_bloom_tint_color.g, z - ps_r2_bloom_tint_color.b
/*
constants buffer descr:
    autoexposure_params - shared EV100 contract from autoexposure.hlsli
    bloom_amount - strength of bloom effect, can be tweaked
    bloom_desaturation - how much bloom should be desaturated, 0 - no desaturation, 1 - full desaturation, can be tweaked
    bloom_tint_amount - how much bloom should be tinted, 0 - no tint, 1 - full tint, can be tweaked
    bloom_tint_color - color to tint bloom with, can be tweaked
    tonemap_compression - how much to compress highlights, higher value means later compression (longer linear part), can be tweaked
    tonemap_desaturation - how much to desaturate highlights, can be tweaked
    tonemap_crossfeed - how much to mix color channels, can be tweaked
    tonemap_vibrance - how much to boost vibrance, can be tweaked
*/

#define USE_COMPUTE_ADAPTATION // Comment out to use the old adaptation path.
#define USE_GT7_LUT // Comment out to compare with the previous tonemapper.
#define USE_NEW_ADAPT
#define USE_NEW_BLOOM_TONEMAP
#define USE_CROSSFEED
#define USE_VIBRANCE
//#define USE_LUT_TEXTURE

#ifdef USE_COMPUTE_ADAPTATION
#include "tonemap_lut.hlsli"
#endif

float3 main(PSInputFullscreen I) : SV_Target
{
    float3 Color = max(0.0f, dof(I.texcoord));
    float4 Bloom;
        [branch]
        if (bloom_params.w > 0.5f)
            Bloom = s_bloom_compute.Sample(smp_rtlinear, I.texcoord);
        else
            Bloom = s_bloom.Sample(smp_rtlinear, I.texcoord);

    #ifdef USE_COMPUTE_ADAPTATION
        float Exposure = s_tonemap_state.Load(int3(0, 0, 0));
    #else
        float adaptedEV100 = s_tonemap.Load(uint3(0, 0, 0)).x;
        float Exposure = ExposureMultiplierFromEV100(CameraEV100(adaptedEV100));
    #endif
      
    #ifndef USE_NEW_BLOOM // new bloom and tonemap will require using new adapt  
        #ifdef USE_CGIM_BLOOM_TWEAK 
	        //Bloom = BrokeBloom(Bloom);
        #endif
	
        //Color.xyz = Color.xyz + Bloom.xyz * 0.1666f * bloom_params.x;
	    //Color.xyz *= rcp(bloom_params.x + 1.0f);
    #else


        float Bloom_Luma = Luminance(Bloom.rgb);
        float3 Bloom_Desat = lerp(Bloom.rgb, Bloom_Luma.xxx, bloom_params.y);
	
        float Tint_Luma = max(Luminance(bloom_tint.rgb), 1e-4);
        float3 Tinted_Bloom = Bloom_Desat * bloom_tint.rgb / Tint_Luma;
	
        Bloom.rgb = lerp(Bloom_Desat, Tinted_Bloom, bloom_params.z);
	
        Color.rgb += bloom_params.x * Bloom.rgb;
    #endif
    
    #if defined(USE_GT7_LUT) && defined(USE_COMPUTE_ADAPTATION)
        Color.rgb = LinearToGamma(TonemapLUT(Color.rgb));
    #elif defined(USE_NEW_ADAPT) || defined(USE_COMPUTE_ADAPTATION)
        Color *= Exposure;
	
        //Color.rgb = 1.0 - exp(-1.0 * Color.rgb); //CommerceToneMapping(Color.rgb, tonemap_params.x, tonemap_params.y);
        //Color.rgb = LinearToGamma(Color.rgb);
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
	    //Color = Uncharted2Tonemap(Color);
    #endif
	
    #ifdef USE_LUT_TEXTURE
 	    //Color = s_lut.Sample(smp_rtlinear, saturate(Color)).xyz;
    #endif
    
	return Color;
}

