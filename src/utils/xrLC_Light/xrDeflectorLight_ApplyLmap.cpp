#include "stdafx.h"
#include "xrDeflector.h"
#include "../xrDXT/xrDXT.h"

// Borders

void GET(const base_color& surface_color, const u8 marker, u32 ref, u32& count, base_color_c& dst)
{
	if (marker <= ref)		return;

	base_color_c		C;
	surface_color._get(C);
	dst.add(C);
	count++;
}

void GET(const lm_layer& lm, int x, int y, u32 ref, u32& count, base_color_c& dst)
{
	// wrap pixels
	if (x < 0) return;
	else if (x >= (int)lm.width)	return;
	if (y < 0) return;
	else if (y >= (int)lm.height)	return;

	// summarize
	u32		id = y * lm.width + x;
	if (lm.marker[id] <= ref)		return;

	base_color_c		C;
	lm.surface[id]._get(C);
	dst.add(C);
	count++;
}
 
struct lm_line
{
	buffer_vector<base_color>& surface;
	buffer_vector<u8>& marker;
	u32 y;
	u32 height;
	lm_line(buffer_vector<base_color>& surf_buf, buffer_vector<u8>& mark_buf) :
		surface(surf_buf), marker(mark_buf), y(u32(-1)), height(u32(-1))
	{
	}

	void save(int _y, const lm_layer& lm)
	{
		y = _y;
		height = lm.height;

		{
			xr_vector<base_color>::const_iterator from = lm.surface.begin() + y * lm.width;
			xr_vector<base_color>::const_iterator to = from + lm.width;
			surface.assign(from, to);
		}

		{
			xr_vector<u8>::const_iterator from = lm.marker.begin() + y * lm.width;
			xr_vector<u8>::const_iterator to = from + lm.width;
			marker.assign(from, to);
		}
	}
};

void GET(const lm_line& l, int x, u32 width, u32 ref, u32& count, base_color_c& dst)
{
	if (x < 0) return;
	else if (x >= (int)width)			return;
	if (l.y < 0) return;
	else if (l.y >= (int)l.height)	return;

	// summarize
	u32		id = x;
	if (l.marker[id] <= ref)		return;

	base_color_c		C;
	l.surface[id]._get(C);
	dst.add(C);
	count++;
}

BOOL NEW_ApplyBorders(lm_layer& lm, u32 ref)
{
	bool			bNeedContinue = false;

	buffer_vector<base_color>	buf_surf_line0(_alloca(lm.width * sizeof(base_color)), lm.width);
 	buffer_vector<base_color>	buf_surf_line1(_alloca(lm.width * sizeof(base_color)), lm.width);
 	buffer_vector<u8>			buf_marker_line0(_alloca(lm.width * sizeof(u8)), lm.width);
 	buffer_vector<u8>			buf_marker_line1(_alloca(lm.width * sizeof(u8)), lm.width);


	lm_line line0(buf_surf_line0, buf_marker_line0);
	lm_line line1(buf_surf_line1, buf_marker_line1);

	try {
		//lm_layer	result	= lm;

		lm_line* l_0 = &line0;
		lm_line* l_1 = &line1;

		for (int y = 0; y < (int)lm.height; y++) {

			l_0->save(y, lm);

			std::swap(l_0, l_1);

			lm_line& line = *l_0;

			base_color sv_color0;
			sv_color0._set(-1, -1, -1);

			u8		   sv_marker0 = u8(-1);
			for (int x = 0; x < (int) lm.width; x++)
			{
				base_color sv_color = sv_color0;
				u8		   sv_marker = sv_marker0;
				sv_color0 = lm.surface[y * lm.width + x];
				sv_marker0 = lm.marker[y * lm.width + x];
				
				if (lm.marker[y * lm.width + x] == 0)
				{
					base_color_c	clr;
					u32			C = 0;
					if (y > 0)
					{
						GET(line, x - 1, lm.width, ref, C, clr);
						GET(line, x, lm.width, ref, C, clr);
						GET(line, x + 1, lm.width, ref, C, clr);
					}

					if (x > 0)
						GET(sv_color, sv_marker, ref, C, clr);



					GET(lm, x + 1, y, ref, C, clr);

					GET(lm, x - 1, y + 1, ref, C, clr);
					GET(lm, x, y + 1, ref, C, clr);
					GET(lm, x + 1, y + 1, ref, C, clr);

					if (C) 
					{
						clr.scale(C);
						lm.surface[y * lm.width + x]._set(clr);
						lm.marker[y * lm.width + x] = u8(ref);
 
						bNeedContinue = TRUE;
					}

				}
			}
		}
 	}
	catch (...)
	{
		clMsg("* ERROR: ApplyBorders");
	}
	return bNeedContinue;
}
 
thread_local CStatTimer tApplyBorders;
thread_local CStatTimer tRmsTests;
thread_local CStatTimer tRmsTestsZero;


BOOL ApplyBorders(lm_layer& lm, u32 ref)
{
	tApplyBorders.Begin();
	bool ret =  NEW_ApplyBorders(lm, ref);
	tApplyBorders.End();
 	return ret;
}

void GetApplyStats()
{
	tApplyBorders.FrameEnd();
	tRmsTests.FrameEnd();
	tRmsTestsZero.FrameEnd();

	Msg("TH[%u] ApplyBorder: %u ms | Rms: %u ms | Zero: %u ms",
		GetCurrentThreadId(),
		tApplyBorders.GetElapsed_ms(),
		tRmsTests.GetElapsed_ms(),
		tRmsTestsZero.GetElapsed_ms() 
	);

	tApplyBorders.FrameStart();
	tRmsTests.FrameStart();
	tRmsTestsZero.FrameStart();
}

// Compression test

IC u32	rms_diff(u32 a, u32 b)
{
	if (a > b)	return a - b;
	else		return b - a;
}
 
// Это при сжатии используется
BOOL	__stdcall rms_test_compress(lm_layer& lm, u32 w, u32 h, u32 rms)
{
	CScopeTimer T(tRmsTests);

	if ((w <= 1) || (h <= 1))	return FALSE;

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
		return	FALSE;
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
				if (rms_diff(color_get_R(pixel_r_base), color_get_R(pixel_base)) > rms)			return FALSE;
				if (rms_diff(color_get_G(pixel_r_base), color_get_G(pixel_base)) > rms)			return FALSE;
				if (rms_diff(color_get_B(pixel_r_base), color_get_B(pixel_base)) > rms)			return FALSE;
				if (rms_diff(color_get_A(pixel_r_base), color_get_A(pixel_base)) > rms)			return FALSE;
				if (rms_diff(color_get_R(pixel_r_hemi), color_get_R(pixel_hemi)) > ((rms * 4) / 3))	return FALSE;
			}
		}
	}
	return	TRUE;
}

// Это при проверке используется
BOOL	__stdcall rms_test(lm_layer& lm, u32 _r, u32 _g, u32 _b, u32 _s, u32 _h, u32 rms)
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

				if (r_rms > rms)			return FALSE;
				if (g_rms > rms)			return FALSE;
				if (b_rms > rms)			return FALSE;
				if (s_rms > rms)			return FALSE;
				if (h_rms > ((rms * 4) / 3))	return FALSE;
			}
		}
	}
	return TRUE;
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

BOOL	compress_Zero(lm_layer& lm, u32 rms)
{
	CScopeTimer T(tRmsTestsZero);

	// Average color
	base_color_c	_c;
	u32				_count = rms_average(lm, _c);

	if (0 == _count)
	{
		u32 AnyMarker = 0;
		for (int y = 0; y < lm.height; y++)
		{
			for (int x = 0; x < lm.width; x++)
			{
				u32	offset = y * lm.width + x;
				if (lm.marker[offset] > 0)
				{
					AnyMarker++;
				}
			}
		}
 
		Msg("* ERROR: Lightmap not calculated (W: %u | H: %u) | Markers Has: %u", lm.width, lm.height, AnyMarker);
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
		return TRUE;
	}
	return FALSE;
}

BOOL	compress_RMS(lm_layer& lm, u32 rms, u32& w, u32& h)
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
		return TRUE;
	}
	return FALSE;
}
