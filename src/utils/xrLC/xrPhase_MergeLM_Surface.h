#pragma once

#include <atomic>
#include "xrPhase_MergeLM_Rect.h"
#include "../xrLC_Light/xrDeflector.h"
#include "../../xrCore/xrCore.h"

class lm_layer;

class SurfacePlacePerpixel
{
public:
	u32 BORDER = 1; 
	// Original 
	std::mutex csLMMerge;
	
	u32	alpha_ref = 254 - BORDER;
	u32 SurfaceGrid = 4096;

	xr_vector<u8> surface_tbb;
	xr_vector<u16> occupied_y;

	u32 FilledCount = 0;
	u32 RegisterSize = 0;

	float GetMaxFilled();
	float GetCurrFilled();

	// 
 	void _InitSurface();
	bool _rect_register(L_rect& R, lm_layer* D);
	bool Place_Perpixel(L_rect& R, lm_layer* D);
	bool rect_place_full(L_rect& r, lm_layer* D);
};

extern SurfacePlacePerpixel placer_perpixel;
