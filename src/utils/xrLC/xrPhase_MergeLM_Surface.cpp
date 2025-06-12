#include "stdafx.h" 
#include "xrPhase_MergeLM_Surface.h"
#include <immintrin.h>
#include <intrin.h>

#define MAX_GRID_SPACE_WRITE 0.85f	// 80% НАПОЛНЕНИЯ LMAP
// Surfaces

void SurfacePlacePerpixel::RecalcY()
{
	u32 _Y = 0;
	while (occupied_y[_Y] > SurfaceGrid * MAX_GRID_SPACE_WRITE)
	{
		_Y++;
	}
 	StartYPos = _Y;
}

void SurfacePlacePerpixel::_InitSurface_tbb()
{
	StartYPos   = 0;
	SurfaceGrid = getLMSIZE();
	surface_tbb = xr_alloc<u8>(SurfaceGrid * SurfaceGrid);
	FillMemory(surface_tbb, SurfaceGrid * SurfaceGrid, 0);

	occupied_y = xr_alloc<u16>(SurfaceGrid);
	FillMemory(occupied_y, SurfaceGrid, 0);
}

void SurfacePlacePerpixel::_rect_register_tbb(L_rect& R, lm_layer* D)
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
 
bool SurfacePlacePerpixel::Place_Perpixel_tbb(L_rect& R, lm_layer* D)
{
	u8* lm = &*(D->marker.begin());
	u32	s_x = D->width + 2 * BORDER;
	u32	s_y = D->height + 2 * BORDER;

	// Normal
	const auto mm_alpha_ref256	= _mm256_set1_epi8(alpha_ref);	 
	const auto mm_zero256		= _mm256_setzero_si256();		 

	const auto mm_alpha_ref		= _mm_set1_epi8(alpha_ref);	 
	const auto mm_zero			= _mm_setzero_si128();		 

	for (u32 y = 0; y < s_y; y++)
	{
		if (s_x >= 32 && CPU::ID.hasFeature(CPUFeature::AVX) )
		{
			u8* P = surface_tbb + (y + R.a.y) * SurfaceGrid + R.a.x;	// destination scan-line
			u8* S = lm + y * s_x;

 			u32 x = 0;
			// Проходим по 32 байт за итерацию
 			for (x = 0; x < s_x - 32; x += 32, P += 32, S += 32)
			{
				auto mm_reg_s	= _mm256_loadu_si256((__m256i*) S);
				auto mm_reg_p	= _mm256_loadu_si256((__m256i*) P);
				auto mm_max		= _mm256_max_epu8(mm_reg_s, mm_alpha_ref256);
				auto mm_cmp		= _mm256_cmpeq_epi8(mm_max, mm_alpha_ref256);
				auto mm_andn	= _mm256_andnot_si256(mm_cmp, mm_reg_p);
				auto mm_sad		= _mm256_sad_epu8(mm_andn, mm_zero256);

				__m128i lower_128 = _mm256_castsi256_si128(mm_sad); // взять младшие 128 бит
				if (_mm_cvtsi128_si32(lower_128))
					return false;
			}
			 
			// Оставшееся 
			for (; x < s_x; x++, P++, S++)
			{
				if ((*P) && (*S >= alpha_ref))
					return false;
			}
		}
		else if (s_x >= 16 && CPU::ID.hasFeature(CPUFeature::SSE))
		{
			u8* P = surface_tbb + (y + R.a.y) * SurfaceGrid + R.a.x;	// destination scan-line
			u8* S = lm + y * s_x;

			u32 x = 0;
			// Проходим по 16 байт за итерацию
			for (x = 0; x < s_x - 16; x += 16, P += 16, S += 16)
			{
				auto mm_reg_s = _mm_loadu_si128((__m128i*) S);
				auto mm_reg_p = _mm_loadu_si128((__m128i*) P);
				auto mm_max = _mm_max_epu8(mm_reg_s, mm_alpha_ref);
				auto mm_cmp = _mm_cmpeq_epi8(mm_max, mm_alpha_ref);
				auto mm_andn = _mm_andnot_si128(mm_cmp, mm_reg_p);
				auto mm_sad = _mm_sad_epu8(mm_andn, mm_zero);

 				if (_mm_cvtsi128_si32(mm_sad))
					return false;
			}
 
			// Оставшееся 
			for (; x < s_x; x++, P++, S++)
			{
				if ((*P) && (*S >= alpha_ref))
					return false;
			}
		}
		else
		{
			BYTE* P = surface_tbb + (y + R.a.y) * SurfaceGrid + R.a.x;	// destination scan-line
			u8* S = lm + y * s_x;
			for (u32 x = 0; x < s_x; x++, P++, S++)
			{
				if ((*P) && (*S >= alpha_ref))
					return false;
			}
		}
	}
 
	// It's OK to place it
	return true;
}

bool SurfacePlacePerpixel::rect_place_full(L_rect& r, lm_layer* D)
{
	int SizeX = r.b.x;
	int SizeY = r.b.y;

	int x_max = SurfaceGrid - SizeX;
	int y_max = SurfaceGrid - SizeY;

	int y_max_line = SurfaceGrid * MAX_GRID_SPACE_WRITE;

	L_rect R;
	
	for (int _Y = StartYPos; _Y < y_max; _Y++)
	{
		if (occupied_y[_Y] > y_max_line)			// Нет Места под заливку
			continue;

		if (occupied_y[_Y] > SurfaceGrid - SizeX)	// Нет Места под заливку
			continue;

		if (SurfaceGrid - occupied_y[_Y] < SizeX)   // Не влезет тупо
			continue;
  
		BYTE* temp_surf = surface_tbb + _Y * SurfaceGrid;
 
		// remainder part
		for (int _X = 0; _X < x_max; _X++)
		{
			R.init(_X, _Y, _X + SizeX, _Y + SizeY);
			if (Place_Perpixel_tbb(R, D))
			{
				_rect_register_tbb(R, D);
 				r.set(R);
				return TRUE;
			}
		}
	}
	return FALSE;
}

SurfacePlacePerpixel placer_perpixel;
 