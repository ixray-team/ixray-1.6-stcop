#pragma once
#undef LP_DEFAULT
enum
{
	LP_DEFAULT			= 0,
	LP_UseFaceDisable	= (1<<0),
	LP_dont_rgb			= (1<<1),
	LP_dont_hemi		= (1<<2),
	LP_dont_sun			= (1<<3),
};