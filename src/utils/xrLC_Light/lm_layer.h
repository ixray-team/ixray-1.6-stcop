#pragma once

#include "base_color.h"

#define BORDER 4
 

struct XRLC_LIGHT_API  lm_layer
{
	u32						width;
	u32						height;
	xr_vector<base_color>	surface;
	xr_vector<u8>			marker;
 
public:
	void					create			(u32 w, u32 h)
	{
		width				= w;
		height				= h;
		u32		size		= w*h;
		surface.clear();	surface.resize	(size);
		marker.clear();		marker.assign	(size,0);
	}
	void					destroy			()
	{
		width=height		= 0;
		surface.clear();
		marker.clear();
	}
	u32						Area			()						{ return (width+2*BORDER)*(height+2*BORDER); }
	void					Pixel			(u32 ID, u8& r, u8& g, u8& b, u8& s, u8& h);
	void					Pack			(xr_vector<u32>& dest)const;
	void					Pack_hemi		(xr_vector<u32>& dest)const;
 
	bool					similar			( const lm_layer &D, float eps =EPS ) const;
							lm_layer()				{ width=height=0; }

	// se7kills Подсчитать Размер
	size_t					memory_lmap()
	{
		size_t lm_surface = surface.capacity() * sizeof(base_color);
		size_t lm_marker = marker.capacity() * sizeof(u8);
 		return lm_surface + lm_marker + sizeof(*this); // + Собственный размер
	}
};