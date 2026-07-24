#pragma once

#include "TiramisuRenderTypes.h"

class TiramisuRenderTexture2D;

class CDS0_FontRender:
	public IFontRender
{
public:
	CDS0_FontRender();
	~CDS0_FontRender();
	virtual void Initialize(LPCSTR cShader, LPCSTR cTexture) ;
	virtual void OnRender(CGameFont &owner) ;
	void CreateFontAtlas(u32 width, u32 height, const char* name, void* bitmap) override;

private:
	TiramisuRenderTexture2D* Atlas;
};
