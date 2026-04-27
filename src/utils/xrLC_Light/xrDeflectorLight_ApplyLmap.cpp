#include "stdafx.h"
#include "xrDeflector.h"
#include "../xrDXT/xrDXT.h"

// Compression test
IC u32	rms_diff(u32 a, u32 b)
{
	if (a > b)	return a - b;
	else		return b - a;
}
 
// Это при сжатии используется
bool	__stdcall rms_test_compress(lm_layer& lm, u32 w, u32 h, u32 rms)
{
	if ((w <= 1) || (h <= 1))	return false;

	// scale down(lanczos3) and up (bilinear, as video board) //.
	xr_vector<u32>	pOriginal_base;	lm.Pack(pOriginal_base);
	xr_vector<u32>	pScaled_base;	pScaled_base.resize(w * h);
	xr_vector<u32>	pRestored_base;	pRestored_base.resize(lm.width * lm.height);
	xr_vector<u32>	pOriginal_hemi;	lm.Pack_hemi(pOriginal_hemi);
	xr_vector<u32>	pScaled_hemi;	pScaled_hemi.resize(w * h);
	xr_vector<u32>	pRestored_hemi;	pRestored_hemi.resize(lm.width * lm.height);

	try
	{
		// rgb + sun
		DXTUtils::Filter::Process(&*pScaled_base.begin(), w, h, &*pOriginal_base.begin(), lm.width, lm.height, DXTUtils::Filter::imf_lanczos3);
		DXTUtils::Filter::Process(&*pRestored_base.begin(), lm.width, lm.height, &*pScaled_base.begin(), w, h, DXTUtils::Filter::imf_filter);

		// hemi
		DXTUtils::Filter::Process(&*pScaled_hemi.begin(), w, h, &*pOriginal_hemi.begin(), lm.width, lm.height, DXTUtils::Filter::imf_lanczos3);
		DXTUtils::Filter::Process(&*pRestored_hemi.begin(), lm.width, lm.height, &*pScaled_hemi.begin(), w, h, DXTUtils::Filter::imf_filter);
	}
	catch (...)
	{
		clMsg("* ERROR: imf_Process");
		return	false;
	}

	// compare them
	const u32 limit = 254 - BORDER;
	for (u32 y = 0; y < lm.height; y++)
	{
		u32		offset = y * lm.width;
		u8* scan_mark = (u8*)&*(lm.marker.begin() + offset);		//.
		u32* scan_lmap_base = (u32*)&*(pOriginal_base.begin() + offset);
		u32* scan_rest_base = (u32*)&*(pRestored_base.begin() + offset);
		u32* scan_lmap_hemi = (u32*)&*(pOriginal_hemi.begin() + offset);
		u32* scan_rest_hemi = (u32*)&*(pRestored_hemi.begin() + offset);

		for (u32 x = 0; x < lm.width; x++)
		{
			if (scan_mark[x] >= limit)
			{
				u32 pixel_base = scan_lmap_base[x];
				u32 pixel_r_base = scan_rest_base[x];
				u32 pixel_hemi = scan_lmap_hemi[x];
				u32 pixel_r_hemi = scan_rest_hemi[x];
				if (rms_diff(color_get_R(pixel_r_base), color_get_R(pixel_base)) > rms)			return false;
				if (rms_diff(color_get_G(pixel_r_base), color_get_G(pixel_base)) > rms)			return false;
				if (rms_diff(color_get_B(pixel_r_base), color_get_B(pixel_base)) > rms)			return false;
				if (rms_diff(color_get_A(pixel_r_base), color_get_A(pixel_base)) > rms)			return false;
				if (rms_diff(color_get_R(pixel_r_hemi), color_get_R(pixel_hemi)) > ((rms * 4) / 3))	return false;
			}
		}
	}
	return	true;
}

// Это при проверке используется
bool	__stdcall rms_test(lm_layer& lm, u32 _r, u32 _g, u32 _b, u32 _s, u32 _h, u32 rms)
{
	u32 x, y;
	for (y = 0; y < lm.height; y++)
	{
		for (x = 0; x < lm.width; x++)
		{
			u32	offset = y * lm.width + x;
			if (lm.marker[offset] >= 254)
			{
				u8			r, g, b, s, h;
				lm.Pixel(offset, r, g, b, s, h);

				u32 r_rms = rms_diff(_r, r);
				u32 g_rms = rms_diff(_g, g);
				u32 b_rms = rms_diff(_b, b);
 				u32 s_rms = rms_diff(_s, s);
 				u32 h_rms = rms_diff(_h, h);

				if (r_rms > rms)			return false;
				if (g_rms > rms)			return false;
				if (b_rms > rms)			return false;
				if (s_rms > rms)			return false;
				if (h_rms > ((rms * 4) / 3))	return false;
			}
		}
	}
	return true;
}

u32	__stdcall rms_average(lm_layer& lm, base_color_c& C)
{
	u32 x, y, _count = 0;

	for (y = 0; y < lm.height; y++)
	{
		for (x = 0; x < lm.width; x++)
		{
			u32	offset = y * lm.width + x;
			if (lm.marker[offset] >= 254)
			{
				base_color_c	cc;
				lm.surface[offset]._get(cc);
				C.add(cc);
				_count++;
			}
		}
	}

	return	_count;
}

bool	compress_Zero(lm_layer& lm, u32 rms)
{
	// Average color
	base_color_c	_c;
	u32				_count = rms_average(lm, _c);

	if (0 == _count)
	{
		Msg("* ERROR: Lightmap not calculated (W: %u | H: %u)", lm.width, lm.height);
		return	FALSE;
	}
	else
		_c.scale(_count);

	// Compress if needed
	u8	_r = u8_clr(_c.rgb.x); //.
	u8	_g = u8_clr(_c.rgb.y);
	u8	_b = u8_clr(_c.rgb.z);
	u8	_s = u8_clr(_c.sun);
	u8	_h = u8_clr(_c.hemi);
	if (rms_test(lm, _r, _g, _b, _s, _h, rms))
	{
		u32		c_x = BORDER * 2;
		u32		c_y = BORDER * 2;
		base_color ccc;		ccc._set(_c);
		lm.surface.assign(c_x * c_y, ccc);
		lm.marker.assign(c_x * c_y, 255);
		lm.height = 0;
		lm.width = 0;
		return true;
	}
	return false;
}

bool	compress_RMS(lm_layer& lm, u32 rms, u32& w, u32& h)
{
	// *** Try to bilinearly filter lightmap down and up
	w = 0, h = 0;
	if (lm.width >= 2)
	{
		w = lm.width / 2;
		if (!rms_test_compress(lm, w, lm.height, rms))
		{
			// 3/4
			w = (lm.width * 3) / 4;
			if (!rms_test_compress(lm, w, lm.height, rms))
				w = 0;
		}
		else
		{
			// 1/4
			u32 nw = (lm.width * 1) / 4;
			if (rms_test_compress(lm, nw, lm.height, rms))
				w = nw;
		}
	}

	if (lm.height >= 2)
	{
		h = lm.height / 2;
		if (!rms_test_compress(lm, lm.width, h, rms))
		{
			// 3/4
			h = (lm.height * 3) / 4;
			if (!rms_test_compress(lm, lm.width, h, rms))
				h = 0;
		}
		else
		{
			// 1/4
			u32 nh = (lm.height * 1) / 4;
			if (rms_test_compress(lm, lm.width, nh, rms))
				h = nh;
		}
	}
	if (w || h)
	{
		if (0 == w)
			w = lm.width;
		if (0 == h)
			h = lm.height;
		//		clMsg	("* RMS: [%d,%d] => [%d,%d]",lm.width,lm.height,w,h);
		return true;
	}
	return false;
}
