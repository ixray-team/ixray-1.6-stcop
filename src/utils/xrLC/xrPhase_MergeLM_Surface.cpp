#include "StdAfx.h"
#include "xrPhase_MergeLM_Surface.h"
#include "../xrForms/CompilersUI.h"
 
SurfacePlacePerpixel placer_perpixel;

float SurfacePlacePerpixel::GetMaxFilled()
{
    return gCompilerMode.LC_sizeFill;
}

float SurfacePlacePerpixel::GetCurrFilled()
{ 
    return float(FilledCount) / float(SurfaceGrid * SurfaceGrid);
};

void SurfacePlacePerpixel::UpdateFill()
{
    std::lock_guard lock(csLMMerge);

    int y_max_line = SurfaceGrid * GetMaxFilled();
    FullFilled = 0;
    for (auto _Y = 0; _Y < SurfaceGrid; _Y++)
    {
        if (occupied_y[_Y] > y_max_line)
            FullFilled++;
    }
}

// --------------------------------------------------------
void SurfacePlacePerpixel::_InitSurface()
{
    BORDER = gCompilerMode.LC_BORDER;;
    alpha_ref = 254 - BORDER;

    FilledCount = 0;
    RegisterSize = 0;

    SurfaceGrid = gCompilerMode.LC_sizeLmaps;

    surface_tbb.resize(SurfaceGrid * SurfaceGrid);
    FillMemory(surface_tbb.data(), SurfaceGrid * SurfaceGrid, 0);

    occupied_y.resize(SurfaceGrid);
    FillMemory(occupied_y.data(), SurfaceGrid * sizeof(u16), 0);
}

// --------------------------------------------------------
bool SurfacePlacePerpixel::Place_Perpixel(L_rect& R, lm_layer* D)
{
    u8* lm = &*(D->marker.begin());

    u32 s_x = D->width + 2 * BORDER;
    u32 s_y = D->height + 2 * BORDER;

    for (u32 y = 0; y < s_y; y++)
    {
        BYTE* P = surface_tbb.data() + (y + R.a.y) * SurfaceGrid + R.a.x;
        u8* S = lm + y * s_x;

        for (u32 x = 0; x < s_x; x++, P++, S++)
        {
            if (*P && (*S >= alpha_ref))
                return false;
        }
    }

    return true;
}

// --------------------------------------------------------
bool SurfacePlacePerpixel::_rect_register(L_rect& R, lm_layer* D)
{
    std::lock_guard lock(csLMMerge);

    bool isCanRegister = Place_Perpixel(R, D);
    if (isCanRegister)
    {
        u8* lm = &*(D->marker.begin());

        u32 s_x = D->width + 2 * BORDER;
        u32 s_y = D->height + 2 * BORDER;

        for (u32 y = 0; y < s_y; y++)
        {
            u32 _Y = y + R.a.y;

            BYTE* P = surface_tbb.data() + _Y * SurfaceGrid + R.a.x;
            u8* S = lm + y * s_x;

            for (u32 x = 0; x < s_x; x++, P++, S++)
            {
                if (*S >= alpha_ref)
                {
                    *P = 255;
                    occupied_y[_Y]++;
                    FilledCount++;
                }
            }
        }

        RegisterSize++;
    }

    return true;
}

// --------------------------------------------------------
bool SurfacePlacePerpixel::rect_place_full(L_rect& r, lm_layer* D)
{
    int SizeX = D->width + 2 * BORDER;
    int SizeY = D->height + 2 * BORDER;

    int x_max = SurfaceGrid - SizeX;
    int y_max = SurfaceGrid - SizeY;
    int y_max_line = SurfaceGrid * GetMaxFilled();

    L_rect R;
    for (int _Y = 0; _Y < y_max; _Y++)
    {
        if (occupied_y[_Y] > y_max_line)		  continue;
        if (occupied_y[_Y] > SurfaceGrid - SizeX) continue;

        for (int _X = 0; _X < x_max;)
        {
            R.init(_X, _Y, _X + SizeX, _Y + SizeY);
            if (Place_Perpixel(R, D) && _rect_register(R, D))
            {
                r.set(R);
                return true;
            }

            _X += std::max(1, SizeX);
        }
    }

    return false;
}