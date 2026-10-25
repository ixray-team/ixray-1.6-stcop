#ifndef dxThunderboltRender_included
#define dxThunderboltRender_included
#pragma once

#include "../../Include/xrRender/ThunderboltRender.h"

class dxThunderboltRender : public IThunderboltRender
{
public:
	dxThunderboltRender();
	virtual ~dxThunderboltRender() override;

	virtual void Copy(IThunderboltRender &_in) override;

	virtual void Render(CEffect_Thunderbolt &owner) override;
private:
	ref_geom			  		hGeom_model;
	ref_geom			  		hGeom_gradient;
};

#endif	//	ThunderboltRender_included