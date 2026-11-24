#include "common\common_iostructs.hlsli"

Texture2D<float> s_static_shadow : register(t25);
#include "common\post_cb.hlsli"

float ps_main(PSInputFullscreen Input) : SV_Depth
{
	// z: how many texels of the cached depth one texel of the tile covers
	int2 SourcePixel = int2((Input.hpos.xy - shadow_cache_destination.xy) * shadow_cache_destination.z);
	return s_static_shadow.Load(int3(SourcePixel, 0));
}
