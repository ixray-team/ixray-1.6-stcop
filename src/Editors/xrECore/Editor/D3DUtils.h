//----------------------------------------------------
// file: D3DUtils.h
//----------------------------------------------------
#include "device.h"
#include "../../../Layers/xrRender/D3DUtils.h"

extern ECORE_API CDrawUtilities DU_impl;
ECORE_API void AddCross(const Fvector& p, float szx1, float szy1, float szz1, float szx2, float szy2, float szz2, u32 clr, bool bRot45 = false);
ECORE_API void FlushCrosses();