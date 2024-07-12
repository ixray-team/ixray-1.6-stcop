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
	pFontSystem = nullptr;
	pFontSystem16 = nullptr;
	pFontStat = nullptr;
}

CFontManager::~CFontManager()
{
	Device.seqDeviceReset.Remove(this);

	for (auto& fontPair : Fonts) {
		xr_delete(fontPair.second);
	}
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

CGameFont* CFontManager::CloneFont(const shared_str& name)
{
	auto it = Fonts.find(name);
	if (it != Fonts.end())
	{
		CGameFont* font = new CGameFont(name.c_str(), 0);
		shared_str newName;
		newName.printf("%s_cloned", name.c_str());
		Fonts[newName] = font;
		return font;
	}

	FATAL("Failed to clone font which was'nt initialized");
	return nullptr;
}

void CFontManager::OnDeviceReset()
{
	// Removed old fonts
	for (auto& [Name, Font] : Fonts)
		Font->ReInit();

	InitializeFonts();
}