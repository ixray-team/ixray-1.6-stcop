#include "StdAfx.h"
#include "WristwatchFont.h"

#include "../../xrCore/EngineExternal.h"
#include "../../xrEngine/FontManager.h"
#include "../../xrEngine/GameFont.h"
#include "../../xrEngine/WristwatchSettings.h"

namespace
{
CGameFont* g_font = nullptr;
float g_atlasInvW = 1.0f;
float g_atlasInvH = 1.0f;
bool g_atlasMetricsReady = false;

void RefreshAtlasMetrics(CGameFont& font)
{
	u32 atlasW = 0;
	u32 atlasH = 0;
	if (font.GetAtlasTexSize(atlasW, atlasH))
	{
		g_atlasInvW = atlasW > 0 ? 1.0f / static_cast<float>(atlasW) : 1.0f;
		g_atlasInvH = atlasH > 0 ? 1.0f / static_cast<float>(atlasH) : 1.0f;
		g_atlasMetricsReady = true;
		return;
	}

	if (g_atlasMetricsReady)
	{
		return;
	}

	atlasW = EngineExternal().GetFontAltasSize();
	atlasH = atlasW;

	u32 maxBottom = 16;
	for (int character = '0'; character <= '9'; ++character)
	{
		if (const CGameFont::Glyph* glyph = font.GetGlyphInfo(character))
		{
			maxBottom = std::max(maxBottom, static_cast<u32>(glyph->TextureCoord.bottom));
		}
	}

	if (const CGameFont::Glyph* colonGlyph = font.GetGlyphInfo(':'))
	{
		maxBottom = std::max(maxBottom, static_cast<u32>(colonGlyph->TextureCoord.bottom));
	}

	while (atlasH < maxBottom)
	{
		atlasH *= 2;
	}

	g_atlasInvW = atlasW > 0 ? 1.0f / static_cast<float>(atlasW) : 1.0f;
	g_atlasInvH = atlasH > 0 ? 1.0f / static_cast<float>(atlasH) : 1.0f;
	g_atlasMetricsReady = true;
}

void SetGlyphRect(const CGameFont::Glyph& glyph, Fvector4& outRect)
{
	const float left = static_cast<float>(glyph.TextureCoord.left) * g_atlasInvW;
	const float right = static_cast<float>(glyph.TextureCoord.right) * g_atlasInvW;
	const float top = static_cast<float>(glyph.TextureCoord.top) * g_atlasInvH;
	const float bottom = static_cast<float>(glyph.TextureCoord.bottom) * g_atlasInvH;

	outRect.set(left, top, right, bottom);
}

void SetDigitGlyph(CGameFont& font, u32 digit, Fvector4& outRect)
{
	digit = std::min(digit, 9u);
	if (const CGameFont::Glyph* glyph = font.GetGlyphInfo(static_cast<int>('0' + digit)))
	{
		SetGlyphRect(*glyph, outRect);
		return;
	}

	outRect.set(0.0f, 0.0f, 0.0f, 0.0f);
}
}

void WristwatchFont::Invalidate()
{
	g_font = nullptr;
	g_atlasMetricsReady = false;
}

void WristwatchFont::EnsureLoaded()
{
	if (g_font != nullptr)
	{
		return;
	}

	if (g_FontManager == nullptr)
	{
		return;
	}

	const shared_str fontSection = GetWristwatchRuntimeSettings().fontSection;
	if (!pSettings->section_exist(fontSection))
	{
		Msg("! [wristwatch] font section '%s' not found", fontSection.c_str());
		return;
	}

	g_font = g_FontManager->GetFont(fontSection);
	if (g_font == nullptr)
	{
		return;
	}

	RefreshAtlasMetrics(*g_font);
}

void WristwatchFont::UpdateGlyphs(SWristwatchHudData& hudData, u32 digit0, u32 digit1, u32 digit2, u32 digit3)
{
	EnsureLoaded();

	hudData.fontReady = g_font != nullptr;
	if (!hudData.fontReady)
	{
		hudData.fontGlyph0.set(0.0f, 0.0f, 0.0f, 0.0f);
		hudData.fontGlyph1.set(0.0f, 0.0f, 0.0f, 0.0f);
		hudData.fontGlyph2.set(0.0f, 0.0f, 0.0f, 0.0f);
		hudData.fontGlyph3.set(0.0f, 0.0f, 0.0f, 0.0f);
		hudData.fontGlyphColon.set(0.0f, 0.0f, 0.0f, 0.0f);
		hudData.fontGlyphEight.set(0.0f, 0.0f, 0.0f, 0.0f);
		return;
	}

	SetDigitGlyph(*g_font, digit0, hudData.fontGlyph0);
	SetDigitGlyph(*g_font, digit1, hudData.fontGlyph1);
	SetDigitGlyph(*g_font, digit2, hudData.fontGlyph2);
	SetDigitGlyph(*g_font, digit3, hudData.fontGlyph3);
	SetDigitGlyph(*g_font, 8u, hudData.fontGlyphEight);

	if (const CGameFont::Glyph* colonGlyph = g_font->GetGlyphInfo(':'))
	{
		SetGlyphRect(*colonGlyph, hudData.fontGlyphColon);
	}
	else
	{
		hudData.fontGlyphColon.set(0.0f, 0.0f, 0.0f, 0.0f);
	}
}
