#include "common_iostructs.hlsli"

Texture2D<float> s_static_shadow : register(t25);
#include "shared\post_cb.hlsli"

float main(PSInputFullscreen Input) : SV_Depth
{
	int2 SourcePixel = int2(Input.hpos.xy) - int2(shadow_cache_destination.xy);
	return s_static_shadow.Load(int3(SourcePixel, 0));
}
