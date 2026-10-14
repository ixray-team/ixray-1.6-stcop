#pragma once

#ifndef TEX_POINT_ATT
#define TEX_POINT_ATT	"internal\\internal_light_attpoint"
#endif
#ifndef TEX_SPOT_ATT
#define TEX_SPOT_ATT	"internal\\internal_light_attclip"
#endif

IC void r1_tex(CBlender_Compile& C, const char* name, const char* texture, bool clf = false, bool projective = false)
{
	C.r_dx10Texture(name, texture);
	C.r_dx10Sampler(clf ? "smp_rtlinear" : "smp_base");
	(void)projective;
}

IC void r1_tex(CBlender_Compile& C, const char* name, const shared_str& texture, bool clf = false, bool projective = false)
{
	r1_tex(C, name, texture.c_str(), clf, projective);
}
