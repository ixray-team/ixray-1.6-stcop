#pragma once

#include "Fmesh.h"

using VertexDeclarator = svector<RHIInputElementDesc,65>;
struct ENGINE_API GeomData{
	xr_vector<VertexDeclarator> DCL;
	xr_vector<IRHIBuffer*> VB;
	xr_vector<IRHIBuffer*> IB;
	xr_vector<FSlideWindowItem> SWIs;
};