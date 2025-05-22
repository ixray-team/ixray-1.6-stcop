#pragma once

#include <atomic>
#include "xrPhase_MergeLM_Rect.h"
#include "../xrLC_Light/xrdeflector.h" 
#include "../../XrCore/xrCore.h"

class lm_layer;

class SurfacePlacePerpixel
{
	u8* surface_tbb = nullptr;
	u16* occupied_y = nullptr;

	const	u32		alpha_ref = 254 - BORDER;
 
	// Initialization
	u32 SurfaceGrid = 0;
 	// Rendering of rect
public:
	u32 StartYPos  = 0;
	u32 FilledSize = 0;
	u32 FilledPercent = 0;

	void RecalcY();
	void _InitSurface_tbb();
	void _rect_register_tbb(L_rect& R, lm_layer* D);
 	bool Place_Perpixel_tbb(L_rect& R, lm_layer* D);
 	bool rect_place_full(L_rect& r, lm_layer* D);
};

extern SurfacePlacePerpixel placer_perpixel;