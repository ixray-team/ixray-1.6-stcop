#include "StdAfx.h"
#include "../../Layers/xrRender/ETextureParams.h"

u32* Build32MipLevel(u32& _w, u32& _h, u32& _p, u32* pdwPixelSrc, STextureParams* fmt, float blend)
{
	R_ASSERT(pdwPixelSrc);
	R_ASSERT((_w % 2) == 0);
	R_ASSERT((_h % 2) == 0);
	R_ASSERT((_p % 4) == 0);

	u32	dwDestPitch = (_w / 2) * 4;
	u32* pNewData = xr_alloc<u32>((_h / 2) * dwDestPitch);
	u8* pDest = (u8*)pNewData;
	u8* pSrc = (u8*)pdwPixelSrc;

	float	mixed_a = (float)u8(fmt->fade_color >> 24);
	float	mixed_r = (float)u8(fmt->fade_color >> 16);
	float	mixed_g = (float)u8(fmt->fade_color >> 8);
	float	mixed_b = (float)u8(fmt->fade_color >> 0);

	float	inv_blend = 1.f - blend;
	for (u32 y = 0; y < _h; y += 2) {
		u8* pScanline = pSrc + y * _p;
		for (u32 x = 0; x < _w; x += 2) {
			u8* p1 = pScanline + x * 4;
			u8* p2 = p1 + 4;				if (1 == _w)	p2 = p1;
			u8* p3 = p1 + _p;				if (1 == _h)  p3 = p1;
			u8* p4 = p2 + _p;				if (1 == _h)  p4 = p2;
			float	c_r = float(u32(p1[0]) + u32(p2[0]) + u32(p3[0]) + u32(p4[0])) / 4.f;
			float	c_g = float(u32(p1[1]) + u32(p2[1]) + u32(p3[1]) + u32(p4[1])) / 4.f;
			float	c_b = float(u32(p1[2]) + u32(p2[2]) + u32(p3[2]) + u32(p4[2])) / 4.f;
			float	c_a = float(u32(p1[3]) + u32(p2[3]) + u32(p3[3]) + u32(p4[3])) / 4.f;

			if (fmt->flags.is(STextureParams::flFadeToColor)) {
				c_r = c_r * inv_blend + mixed_r * blend;
				c_g = c_g * inv_blend + mixed_g * blend;
				c_b = c_b * inv_blend + mixed_b * blend;
			}
			if (fmt->flags.is(STextureParams::flFadeToAlpha))
				c_a = c_a * inv_blend + mixed_a * blend;

			float	A = (c_a + c_a / 8.f);
			int _r = int(c_r);	clamp(_r, 0, 255);	*pDest++ = u8(_r);
			int _g = int(c_g);	clamp(_g, 0, 255);	*pDest++ = u8(_g);
			int _b = int(c_b);	clamp(_b, 0, 255);	*pDest++ = u8(_b);
			int _a = int(A);	clamp(_a, 0, 255);	*pDest++ = u8(_a);
		}
	}
	_w /= 2; _h /= 2; _p = _w * 4;
	return pNewData;
}