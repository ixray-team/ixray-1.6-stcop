#pragma once
#include "GameFont.h"

class CFontAtlas
{
	friend class CFontManager;
public:
	CFontAtlas();
	~CFontAtlas();

	CFontAtlas(const CFontAtlas& Other);
	CFontAtlas(CFontAtlas&& Other);
	CFontAtlas& operator=(CFontAtlas&& Other);
	CFontAtlas& operator=(const CFontAtlas& Other);

	u32 GetFreeHeight() const;
	u32 GetUsedHeight() const;

	IFontRender* AddFontUser(CGameFont * const User, shared_str ShaderName, void* GreyscaleBitmap, u32 HeightRequired);

protected:

	bool RemoveFontUser(CGameFont* User);
	bool HasUsers() const;

	static const u32 TextureDimension = 2048;
	static u32 AtlasTextureCounter;

	//IFontRender* FontRender = nullptr;
	xr_map<shared_str, IFontRender*> FontRendersPerShader;
	u32 UsedHeight = 0;
	xr_string Name;
	xr_list<CGameFont*> Users;
	bool bTextureCreated = false;
};
