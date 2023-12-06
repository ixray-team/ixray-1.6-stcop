#pragma once
#include "GameFont.h"
#include "FontAtlas.h"

class ENGINE_API CFontManager : public pureDeviceReset
{
public:
	// Hud font
	CGameFont* pFontDI;
	CGameFont* pFontMedium;
	CGameFont* pFontSystem;
	CGameFont* pFontSystem16;
	CGameFont* pFontStat;

	xr_map<shared_str, CGameFont*> Fonts;

	CFontManager();
	~CFontManager();

	void InitializeFonts();

	void Render();
	virtual void OnDeviceReset();

	CGameFont* GetFont(const shared_str& name, u32 flags = 0);
protected:

	shared_str FontConsoleName = "ui_font_console";
	shared_str FontMediumName = "hud_font_medium";
	shared_str FontSystem16Name = "ui_font_letterica16_russian";
	shared_str FontSystemName = "ui_font_letterica18_russian";
	shared_str FontStatName = "stat_font";
};

extern ENGINE_API CFontManager* g_FontManager;