#include "stdafx.h"
#include "FontManager.h"

#include <freetype/freetype.h>
#include <freetype/ftmodapi.h>

ENGINE_API CFontManager* g_FontManager = nullptr;

CFontManager::CFontManager()
{
	g_FontManager = this;
	Device.seqDeviceReset.Add(this, REG_PRIORITY_HIGH);
	pFontDI = nullptr;
	pFontMedium = nullptr;
}

CFontManager::~CFontManager()
{
	Device.seqDeviceReset.Remove(this);

	for (auto FontPair : Fonts)
		xr_delete(FontPair.second);
	Fonts.clear();
}

void CFontManager::InitializeFonts()
{
	pFontDI = GetFont(FontConsoleName); //hud_font_di
	pFontMedium = GetFont(FontMediumName);
	pFontSystem = GetFont(FontSystemName);
	pFontSystem16 = GetFont(FontSystem16Name);
	pFontStat = GetFont(FontStatName);
}

void CFontManager::Render()
{
	for (const auto& [FontName, Font] : Fonts)
		Font->OnRender();
}

CGameFont* CFontManager::GetFont(const shared_str& name, u32 flags /*= 0*/)
{
	auto FontIter = Fonts.find(name);
	if (FontIter == Fonts.end())
	{
		CGameFont* NewFont = new CGameFont(name.c_str(), flags);
		Fonts[name] = NewFont;
		return NewFont;
	}

	return FontIter->second;
}

void CFontManager::OnDeviceReset()
{
	// Removed old fonts
	for (auto& [Name, Font] : Fonts)
		Font->ReInit();

	InitializeFonts();
}