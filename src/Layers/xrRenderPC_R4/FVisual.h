// FVisual.h: interface for the FVisual class.
//
//////////////////////////////////////////////////////////////////////
#ifndef FVisualH
#define FVisualH
#pragma once

#include "FBasicVisual.h"

class	Fvisual					: public		dxRender_Visual, public IRender_Mesh
{
public:
	IRender_Mesh*				m_fast			;	
	s8							m_fast_usable	= -1;
	bool						fast_usable		();
public:
	virtual void				Render			(float LOD		) override;		// LOD - Level Of Detail  [0.0f - min, 1.0f - max], Ignored ?
	virtual void				Load			(const char* N, IReader *data, u32 dwFlags) override;
	virtual void				Copy			(dxRender_Visual *pFrom	) override;
	virtual void				Release			() override;

	Fvisual();
	virtual ~Fvisual() override;
};

#endif 
