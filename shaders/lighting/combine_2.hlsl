#include "common\common.hlsli"
#include "common\mblur.hlsli"
#include "postfx\dof.hlsli"

Texture3D s_lut : register(t25);

#include "common\post_cb.hlsli"

// The share of the tone curve's shoulder each channel takes by itself
static const float TonemapChannelMix = 0.5f;

float3 ps_main(PSInputFullscreen I) : SV_Target
{
    float3 Color = max(0.0f, dof(I.texcoord));
    // Low shading quality sums its light in gamma and has no bloom: it is exposed and curved as the linear frame is
#ifdef USE_SURFACE_CLASSIC
    Color = pow(Color, 2.2f);
#else
    // bloom_params.x: the bloom amount, zero without bloom. The bloom was built from this frame's own colours, which
    // already stand where the other frame's do after it makes room for its bloom: it is added, and nothing is taken
    // away for it
    [branch]
    if (bloom_params.x > 0.0f)
    {
        Color += s_bloom.Sample(smp_rtlinear, I.texcoord).rgb * 0.1666f * bloom_params.x;
    }
#endif
    Color *= exp2(bloom_params.w);

    // The exposure and its tone curve are set up for display values: the stops, the toe and where the curve starts
    // to compress are all counted in gamma. s_tonemap: the scene's average log2 luminance; tonemap_params.w: the bias
    float Exposure = DisplayExposure(s_tonemap.Load(uint3(0, 0, 0)).x, tonemap_params.w);
#ifdef USE_LEGACY_LIGHT
    Color = LegacyToneMapping(pow(max(Color, 0.0f), 1.0f / 2.2f) * Exposure);
#else
    Color = CommerceToneMapping(pow(max(Color, 0.0f), 1.0f / 2.2f) * Exposure, tonemap_params.x, tonemap_params.y, TonemapChannelMix, tonemap_params.z);
#endif

// Low shading quality shows the baked frame as it is lit: no grading either
#ifndef USE_SURFACE_CLASSIC
#ifdef USE_CGIM_COLOR_TWEAK
	Color = Uncharted2Tonemap(Color);
#endif
	
#ifdef USE_LUT_TEXTURE
 	Color = s_lut.Sample(smp_rtlinear, saturate(Color)).xyz;
#endif
#endif

    // spp_params.x: the saturation, one leaves the colours as they are
    Color.xyz = max(lerp(dot(Color.xyz, 0.33f).xxx, Color.xyz, spp_params.x), 0.0f);

    [branch]
    if (spp_params.y > 0.5f)
    {
        Color.xyz *= 1.0f - saturate(distance(I.texcoord / view_uv.xy, float2(0.5f, 0.5f)));
    }
    
	return Color;
}
