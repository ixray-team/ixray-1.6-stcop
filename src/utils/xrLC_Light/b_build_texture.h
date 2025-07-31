#pragma once
#include "../../Layers/xrRender/ETextureParams.h" 

struct b_BuildTexture :
	public b_texture
{
	STextureParams THM;

	IC void	SetHasSurface(BOOL val)
	{
		THM.flags.set(STextureParams::flHasSurface, val);
	}

	IC BOOL HasSurface()const
	{
		return THM.flags.is(STextureParams::flHasSurface);
	}

	u32& Texel(u32 x, u32 y)
	{
		return pSurface[y * dwWidth + x];
	}

	void Vflip()
	{
		R_ASSERT(pSurface);
		for (u32 y = 0; y < dwHeight / 2; y++)
		{
			u32 y2 = dwHeight - y - 1;
			for (u32 x = 0; x < dwWidth; x++)
			{
				u32		t = Texel(x, y);
				Texel(x, y) = Texel(x, y2);
				Texel(x, y2) = t;
			}
		}
	}
};