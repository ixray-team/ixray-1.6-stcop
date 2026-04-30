#pragma once
#undef LP_DEFAULT
enum
{
	LP_DEFAULT			= 0,
 	LP_dont_rgb			= (1<<1),
	LP_dont_hemi		= (1<<2),
	LP_dont_sun			= (1<<3),
};

#include "../xrForms/CompilersUI.h"
static u32 LGetCurrentFlags()
{
	return	(gCompilerMode.LC_SkipStaticMap ? LP_dont_rgb : 0) | (gCompilerMode.LC_NoSun ? LP_dont_sun : 0);
}