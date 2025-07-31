/*

#include "stdafx.h"
#include "build.h"
#include "xrPhase_MergeLM_Rect.h"
#include "../xrLC_Light/xrdeflector.h"

#include <intrin.h>
#include <mmintrin.h>
#include <emmintrin.h>
#include <atomic>
#include <shared_mutex>

extern int CurrentArea = 0;

std::vector<BYTE>	surface_static;
std::vector<bool>	occuped_Y;

std::atomic<BYTE*>	surface = surface_static.data();

std::mutex lock_mutex;


const	u32		alpha_ref = 254 - BORDER;

// Initialization
void _InitSurface()
{
	surface_static.reserve(getLMSIZE() * getLMSIZE());
	FillMemory(surface_static.data(), getLMSIZE() * getLMSIZE(), 0);
	surface.store(surface_static.data());

	occuped_Y = std::vector<bool>(getLMSIZE(), false);

	CurrentArea = 0;
}

// Rendering of rect
void _rect_register(L_rect& R, lm_layer* D, BOOL bRotate)
{
	//	std::unique_lock<std::shared_mutex> lock(surface_mutex);

	u8* lm = &*(D->marker.begin());
	u32		s_x = D->width + 2 * BORDER;
	u32		s_y = D->height + 2 * BORDER;

	BYTE* atomicBuffer(reinterpret_cast<BYTE*>(surface.load()));

	if (!bRotate)
	{
		// Normal (and fastest way)
		for (u32 y = 0; y < s_y; y++)
		{
			int INDEX = (y + R.a.y) * getLMSIZE() + R.a.x;
			BYTE* P = atomicBuffer + INDEX;	// destination scan-line
			// Считываем и записывем S и проверяем по 253 для альфы
			u8* S = lm + y * s_x;
			for (u32 x = 0; x < s_x; x++, P++, S++)
				if (*S >= alpha_ref)
				{
					*P = 255;
					CurrentArea++;
				}
		}
	}
	else
	{
		// Rotated :(
		for (u32 y = 0; y < s_x; y++)
		{
			int INDEX = (y + R.a.y) * getLMSIZE() + R.a.x;
			BYTE* P = atomicBuffer + INDEX;	// destination scan-line

			// Считываем и записывем S и проверяем по 253 для альфы
			for (u32 x = 0; x < s_y; x++, P++)
				if (lm[x * s_x + y] >= alpha_ref)
				{
					*P = 255;
					CurrentArea++;
				}
		}
	}

	surface.store(atomicBuffer);

	// CurrentArea += s_x * s_y;
}


bool PlacePixel(lm_layer* D, L_rect& rect)
{

	u8* lm = &*(D->marker.begin());
	int	s_x = D->width + 2 * BORDER;
	int	s_y = D->height + 2 * BORDER;

	int x_max = getLMSIZE() - rect.b.x;
	int y_max = getLMSIZE() - rect.b.y;


	for (int _Y = 0; _Y < y_max; _Y++)
	{
		BYTE* temp_surf = surface + _Y * getLMSIZE();
		// remainder part
		for (int _X = 0; _X < x_max; _X++)
		{
			if (temp_surf[_X])
				continue;

			L_rect R;
			R.init(_X, _Y, _X + rect.b.x, _Y + rect.b.y);

			// Normal (and fastest way)

			bool Placed = true;
			for (int y = 0; y < s_y; y++)
			{
				BYTE* P = surface + (y + R.a.y) * getLMSIZE() + R.a.x;	// destination scan-line
				u8* S = lm + y * s_x;

				// remainder part
				for (int _X = 0; _X < s_x; _X++, P++, S++)
					if ((*P) && (*S >= alpha_ref))
					{
						Placed = false;
						break;
					}

				if (!Placed)
					break;
			}

			if (Placed)
			{
				_rect_register(R, D, FALSE);
				rect.set(R);
				return TRUE;
			}
		}
	}

	return FALSE;
}


// Test of per-pixel intersection (surface test)
bool Place_Perpixel(L_rect& R, lm_layer* D, BOOL bRotate)
{
	//	std::shared_lock<std::shared_mutex> lock(surface_mutex);

	u8* lm = &*(D->marker.begin());
	int	s_x = D->width + 2 * BORDER;
	int	s_y = D->height + 2 * BORDER;

	//BYTE* SURFACE_LOCAL = surface.load();
	BYTE* atomicBuffer(reinterpret_cast<BYTE*>(surface.load()));

	// Ждем Пока зарегаем
	if (!bRotate)
	{
		// Normal (and fastest way)
		for (int y = 0; y < s_y; y++)
		{
			BYTE* P = atomicBuffer + (y + R.a.y) * getLMSIZE() + R.a.x;	// destination scan-line
			u8* S = lm + y * s_x;

			// remainder part
			for (int _X = 0; _X < s_x; _X++, P++, S++)
				if ((*P) && (*S >= alpha_ref))
				{
					return false;
				}
		}
	}
	else
	{
		// Rotated :(
		for (int y = 0; y < s_x; y++)
		{
			BYTE* P = atomicBuffer + (y + R.a.y) * getLMSIZE() + R.a.x;	// destination scan-line
			for (int _X = 0; _X < s_y; _X++, P++)
				if ((*P) && (lm[_X * s_x + y] >= alpha_ref))
				{
					return false;
				}
		}
	}

	// It's OK to place it
	return true;
}


// Check for intersection 
// Проверка для очень быстрой растоновки (Моего ворианта)
BOOL _rect_place_fast(L_rect& rect, lm_layer* D, int _X, int _Y)
{
	L_rect R;
	R.init(_X, _Y,
		_X + rect.b.x,
		_Y + rect.b.y);

	u32		s_x = D->width + 2 * BORDER;
	u32		s_y = D->height + 2 * BORDER;
	u32 INDEX = (s_y + R.a.y) * getLMSIZE() + R.a.x;
	if (INDEX > getLMSIZE() * getLMSIZE())
		return FALSE;
	if (rect.b.y >= getLMSIZE() - 16)
		return FALSE;

	if (Place_Perpixel(R, D, FALSE))
	{
		_rect_register(R, D, FALSE);
		rect.set(R);
		return TRUE;
	}


	return FALSE;
}

// Оригенал с доработкой
BOOL _rect_place(L_rect& r, lm_layer* D)
{
	BYTE* temp_surf;

	// Normal
	{
		int x_max = getLMSIZE() - r.b.x;
		int y_max = getLMSIZE() - r.b.y;

		int part_lmap = getLMSIZE() / 16;

		L_rect R;
		for (int _Y = 0; _Y < y_max; _Y++)
		{
			temp_surf = surface + _Y * getLMSIZE();

			if (occuped_Y[_Y])
			{
				_Y++;
				continue;
			}

			u32 occuped_parts = 0;

			// remainder part
			for (int _X = 0; _X < x_max;)
			{
				__m128i block = _mm_loadu_si128((__m128i*) & temp_surf[_X]);

				// Сравниваем каждый байт с нулем
				__m128i zeros = _mm_setzero_si128();
				__m128i cmp = _mm_cmpeq_epi8(block, zeros);

				// Получаем маску ненулевых байтов
				int mask = _mm_movemask_epi8(cmp);

				// Если есть хотя бы один ненулевой байт
				if (mask != 0xFFFF)
				{
					_X += 16;
					occuped_parts++;
					continue;
				}

				_X++;


				// Msg("Start Process From X: %d", _X);

				R.init(_X, _Y, _X + r.b.x, _Y + r.b.y);
				if (Place_Perpixel(R, D, FALSE))
				{
					_rect_register(R, D, FALSE);
					r.set(R);
					return TRUE;
				}
			}


			if (occuped_parts == part_lmap)	// Все занято заменяем
			{
				lock_mutex.lock();
				occuped_Y[_Y] = true;
				lock_mutex.unlock();
			}

		}


	}


	// Rotated
	{
		L_rect R;
		int x_max = getLMSIZE() - r.b.y;
		int y_max = getLMSIZE() - r.b.x;
		for (int _Y = 0; _Y < y_max; _Y++)
		{
			temp_surf = surface + _Y * getLMSIZE();

			if (occuped_Y[_Y])
			{
				_Y++;
				continue;
			}

			// remainder part
			for (int _X = 0; _X < x_max; )
			{
				__m128i block = _mm_loadu_si128((__m128i*) & temp_surf[_X]);

				// Сравниваем каждый байт с нулем
				__m128i zeros = _mm_setzero_si128();
				__m128i cmp = _mm_cmpeq_epi8(block, zeros);

				// Получаем маску ненулевых байтов
				int mask = _mm_movemask_epi8(cmp);

				// Если есть хотя бы один ненулевой байт
				if (mask != 0xFFFF)
				{
					_X += 16;
					continue;
				}

				_X++;


				R.init(_X, _Y, _X + r.b.y, _Y + r.b.x);
				if (Place_Perpixel(R, D, TRUE))
				{
					_rect_register(R, D, TRUE);
					r.set(R);
					return TRUE;
				}
			}
		}

	}
	return FALSE;
}
*/




