#include "stdafx.h"
#include "lm_layer.h"
#include "../xrForms/CompilersUI.h"

void lm_layer::Pack		(xr_vector<u32>& dest)const	
{
	dest.resize			(width*height);
 	xr_vector<u32>::iterator		W = dest.begin();
 	for (auto& S : surface)
	{
		base_color_c	C;  S._get(C);
 		u8	_r	= u8_clr(C.rgb.x);
		u8	_g	= u8_clr(C.rgb.y);
		u8	_b	= u8_clr(C.rgb.z);
		u8	_d	= u8_clr(C.sun);
		*W++  = color_rgba(_r,_g,_b,_d);
	}
}

void lm_layer::Pack_hemi	(xr_vector<u32>& dest)const	//.
{
	dest.resize			(width*height);
  	xr_vector<u32>::iterator		W=dest.begin	();
	for (auto& S : surface)
	{
		base_color_c	C;	S._get(C);
		u8	_d	= u8_clr	(C.sun);
		u8	_h	= u8_clr	(C.hemi);
		if (gCompilerMode.LC_legacyLM)
			*W++	= color_rgba(_h,_h,_h,_d);
		else
			*W++	= color_rgba(_d,_d,_d,_h);
	}
}

void lm_layer::Pixel	(u32 ID, u8& r, u8& g, u8& b, u8& s, u8& h)
{
	xr_vector<base_color>::iterator I = surface.begin()+ID;
	base_color_c	c;	I->_get(c);
	r	= u8_clr(c.rgb.x);
	g	= u8_clr(c.rgb.y);
	b	= u8_clr(c.rgb.z);
	s	= u8_clr(c.sun);
	h	= u8_clr(c.hemi);
}

bool	lm_layer::similar			( const lm_layer &layer, float eps/* =EPS*/ ) const
{
	if( marker.size() != layer.marker.size() ) return false;

	for( u32 i = 0; i< marker.size(); ++i )
	{
		if( marker[i]!=layer.marker[i] )
		{
			return false;
		}
	}
	if( surface.size() != layer.surface.size() )
		return false;
	for( u32 i = 0; i < surface.size(); ++i )
	{
		if( !surface[i].similar( layer.surface[i], EPS ) )
		{
			Msg("sufface diff id: %d", i);
			return false;
		}
	}

	return width ==  layer.width &&
		   height == layer.height;

}

// Apply borders (Новая релизация)
bool lm_layer::ApplyBorders(u32 ref)
{
	bool bNeedContinue = false;

	// Копия surface для чтения (читаем старые значения)
	xr_vector<base_color> src = surface;

	// Сбор соседей
	xr_vector<base_color*> neighbors;

	base_color_c out;
	u32 idx, nidx;

	// Проходим по всем пикселям
	for (u32 y = 0; y < height; ++y)
	{
		for (u32 x = 0; x < width; ++x)
		{
			idx = y * width + x;

			// только пустые пиксели
			if (marker[idx] != 0) continue;

			neighbors.clear();
			for (int dy = -1; dy <= 1; ++dy)
			{
				int ny = (int)y + dy;
				if (ny < 0 || ny >= (int)height) continue;

				for (int dx = -1; dx <= 1; ++dx)
				{
					int nx = (int)x + dx;
					if (nx < 0 || nx >= (int)width) continue;
					if (dx == 0 && dy == 0) continue; // сам пиксель

					nidx = ny * width + nx;
					if (marker[nidx] > ref)
						neighbors.push_back(&src[nidx]);
				}
			}

 			if (neighbors.size() == 0) continue;
 			out.clear_color();

			// суммирование RGB, Hemi, Sun (float x5 )
			for (auto* n : neighbors)
			{
				base_color_c C;
				n->_get(C);
				out.add(C);
			}

			// усредняем
			out.scale(neighbors.size());

			// геттер/сеттер base_color
			surface[idx]._set(out);
			marker[idx] = u8(std::min(ref, 255u));

			bNeedContinue = true;
		}
	}
		 
	return bNeedContinue;
}
 