#pragma once

#include "../../xrCore/_color.h"
#include "../../xrCore/_types.h"

struct SVGTintRGBA
{
	u8 r{ 255 };
	u8 g{ 255 };
	u8 b{ 255 };
	u8 a{ 255 };

	u32 PackKey() const
	{
		return color_argb(a, r, g, b);
	}

	bool IsWhite() const
	{
		return r == 255 && g == 255 && b == 255 && a == 255;
	}

	void SetFromColourDword(u32 argb)
	{
		a = static_cast<u8>(color_get_A(argb));
		r = static_cast<u8>(color_get_R(argb));
		g = static_cast<u8>(color_get_G(argb));
		b = static_cast<u8>(color_get_B(argb));
	}
};

enum class ESVGLoadResult : u8
{
	Success = 0,
	PathTooLong,
	FileOpenFailed,
	ParseFailed,
};

