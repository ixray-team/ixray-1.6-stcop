#pragma once

#ifndef TEX_POINT_ATT
#define TEX_POINT_ATT	"internal\\internal_light_attpoint"
#endif
#ifndef TEX_SPOT_ATT
#define TEX_SPOT_ATT	"internal\\internal_light_attclip"
#endif

IC void r1_tex(CBlender_Compile& C, const char* name, const char* texture, bool = false, bool = false)
{
	C.r_dx10Texture(name, texture);
}

IC void r1_tex(CBlender_Compile& C, const char* name, const shared_str& texture, bool = false, bool = false)
{
	C.r_dx10Texture(name, texture.c_str());
}
