#include "common.hlsli"
#include "mblur.hlsli"
#include "dof.hlsli"

Texture3D s_lut;

float3 main(PSInputFullscreen I) : SV_Target
{
    float3 Color = max(0.0f, dof(I.texcoord));
    float4 Bloom = s_bloom.Sample(smp_rtlinear, I.texcoord);

#ifdef USE_CGIM_BLOOM_TWEAK
	Bloom = BrokeBloom(Bloom);
#endif
	
    float Scale = s_tonemap.Sample(smp_nofilter, float2(0.5f, 0.5f)).x;
    Color = tonemap(Color, Scale);

    Color = combine_bloom(Color, Bloom).xyz;

#ifdef USE_CGIM_COLOR_TWEAK
	Color = Uncharted2Tonemap(Color);
#endif
	
#ifdef USE_LUT_TEXTURE
 	Color = s_lut.Sample(smp_rtlinear, saturate(Color)).xyz;
#endif

	return Color;
}

