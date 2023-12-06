#pragma once

#include "..\..\Include\xrRender\FontRender.h"

class dxFontRender : public IFontRender
{
public:
	dxFontRender();
	virtual ~dxFontRender();

	virtual void Initialize(const char* cShader, const char* cTexture);
	virtual void OnRender(CGameFont& owner);

	virtual void CreateFontAtlas(u32 width, u32 height, const char* name, void* bitmap) override;

private:
	ref_shader				pShader;
	ref_geom				pGeom;
	ref_texture				pTexture;
};