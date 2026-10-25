#ifndef dxLensFlareRender_included
#define dxLensFlareRender_included
#pragma once

#include "../../Include/xrRender/LensFlareRender.h"

class dxFlareRender : public IFlareRender
{
public:
	virtual void Copy(IFlareRender &_in) override;

	virtual void CreateShader(const char* sh_name, const char* tex_name) override;
	virtual void DestroyShader() override;

//private:
public:
	ref_shader		hShader;
};

class dxLensFlareRender : public ILensFlareRender
{
public:
	virtual void Copy(ILensFlareRender &_in) override;

	virtual void Render(CLensFlare &owner, bool bSun, bool bFlares, bool bGradient) override;

	virtual void OnDeviceCreate() override;
	virtual void OnDeviceDestroy() override;
private:
	ref_geom		hGeom;
};

#endif	//	LensFlareRender_included