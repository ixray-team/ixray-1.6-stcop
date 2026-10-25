#ifndef	dx103DFluidVolume_included
#define	dx103DFluidVolume_included
#pragma once

#include "dx103DFluidData.h"
#include "../FBasicVisual.h"

class dx103DFluidVolume : public dxRender_Visual
{
public:
	dx103DFluidVolume();
	virtual ~dx103DFluidVolume() override;

	virtual void	Load( const char* N, IReader *data, u32 dwFlags ) override;
	virtual void	Render( float LOD ) override;		// LOD - Level Of Detail  [0.0f - min, 1.0f - max], Ignored ?
	virtual void	Copy( dxRender_Visual *pFrom ) override;
	virtual void	Release() override;

private:
	//	For debug purpose only
	ref_geom		m_Geom;

	dx103DFluidData	m_FluidData;
};

#endif	//	dx103DFluidVolume_included