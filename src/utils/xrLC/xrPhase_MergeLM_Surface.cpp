#include "stdafx.h" 
#include "xrPhase_MergeLM_Surface.h"
#include "../xrForms/CompilersUI.h"

extern float MAX_GRID_SPACE_WRITE  = 0.95f;	// 90% НАПОЛНЕНИЯ LMAP
// Surfaces

void SurfacePlacePerpixel::RecalcY()
{
	u32 _Y = 0;
	while (occupied_y[_Y] > SurfaceGrid * MAX_GRID_SPACE_WRITE)
	{
		_Y++;
	}
	StartYPos = _Y;

	u32 total_occupied = 0;
	for (u32 y = 0; y < SurfaceGrid ; ++y)
	{
		total_occupied += occupied_y[y];
	}

	FilledSize = total_occupied;
 	FilledPercent = u32 ( float( float(total_occupied) / float(SurfaceGrid * SurfaceGrid) ) * 100.0f);
}

void SurfacePlacePerpixel::_InitSurface_tbb()
{
	StartYPos   = 0;
	SurfaceGrid = gCompilerMode.LC_sizeLmaps;
	surface_tbb = xr_alloc<u8>(SurfaceGrid * SurfaceGrid);
	FillMemory(surface_tbb, SurfaceGrid * SurfaceGrid, 0);

	occupied_y = xr_alloc<u16>(SurfaceGrid);
	FillMemory(occupied_y, SurfaceGrid, 0);
}

bool SurfacePlacePerpixel::_rect_register_tbb(L_rect& R, lm_layer* D)
{
	csLMMerge.Enter();

	bool isCanRegister = Place_Perpixel_tbb(R, D);
	if (isCanRegister)
	{
		u8* lm = &*(D->marker.begin());
		u32		s_x = D->width + 2 * BORDER;
		u32		s_y = D->height + 2 * BORDER;

		// Normal (and fastest way)
		for (u32 y = 0; y < s_y; y++)
		{
			u32 _Y = y + R.a.y;

			BYTE* P = surface_tbb + _Y * SurfaceGrid + R.a.x;	// destination scan-line
			u8* S = lm + y * s_x;
			for (u32 x = 0; x < s_x; x++, P++, S++)
			{
				if (*S >= alpha_ref)
				{
					*P = 255;
					occupied_y[_Y] += 1;
				}
			}
		}
	}
	csLMMerge.Leave();

	return isCanRegister;
}
 
bool SurfacePlacePerpixel::Place_Perpixel_tbb(L_rect& R, lm_layer* D)
{
	u8* lm = &*(D->marker.begin());
	u32	s_x = D->width + 2 * BORDER;
	u32	s_y = D->height + 2 * BORDER;

	// Normal
	for (u32 y = 0; y < s_y; y++)
	{
		BYTE* P = surface_tbb + (y + R.a.y) * SurfaceGrid + R.a.x;	// destination scan-line
		u8* S = lm + y * s_x;
		for (u32 x = 0; x < s_x; x++, P++, S++)
		{
			if ((*P) && (*S >= alpha_ref))
				return false;
		}
 	}
 
	// It's OK to place it
	return true;
}

bool SurfacePlacePerpixel::rect_place_full(L_rect& r, lm_layer* D)
{
	int SizeX = r.b.x; int SizeY = r.b.y;
 	int x_max = SurfaceGrid - SizeX; int y_max = SurfaceGrid - SizeY;
 	int y_max_line = SurfaceGrid * MAX_GRID_SPACE_WRITE;

	L_rect R;	
	for (int _Y = StartYPos; _Y < y_max; _Y++)
	{	
		// Нет Места под заливку
		if (SurfaceGrid - occupied_y[_Y] < SizeX)    continue;
		if (occupied_y[_Y] > y_max_line)			 continue;
  
		BYTE* temp_surf = surface_tbb + _Y * SurfaceGrid;
 
		// remainder part
		for (int _X = 0; _X < x_max; _X++)
		{
			R.init(_X, _Y, _X + SizeX, _Y + SizeY);
			if (Place_Perpixel_tbb(R, D) && _rect_register_tbb(R, D))
			{
  				r.set(R);
				return TRUE;
			}
		}
	}
	return FALSE;
}

SurfacePlacePerpixel placer_perpixel;
 