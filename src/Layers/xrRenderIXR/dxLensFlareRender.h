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
	virtual ~dxLensFlareRender() override;

	// What a frame draws of the flares, taken from the game's with the frame's collect; Draw reads nothing else
	static void Take(CLensFlare& Owner);
	static void Draw(bool bSun, bool bFlares, bool bGradient);

private:
	struct FrameState
	{
		Fcolor LightColor;
		Fvector vecLight, vecX, vecY, vecDir, vecAxis, vecCenter;
		float fBlend, fGradientValue, m_StateBlend;
		class CLensFlareDescriptor* m_Current;
	};

	void DrawState(bool bSun, bool bFlares, bool bGradient);

	static dxLensFlareRender* Frame;
	FrameState State;
	ref_geom		hGeom;
};

#endif	//	LensFlareRender_included