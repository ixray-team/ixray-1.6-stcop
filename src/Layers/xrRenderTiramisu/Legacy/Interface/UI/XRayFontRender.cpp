#include <RedImage/RedImage.hpp>
#include "Resources/Textures/TiramisuRenderTexture2D.h"

ENGINE_API extern Fvector2 g_current_font_scale;

CDS0_FontRender::CDS0_FontRender()
	: Atlas(nullptr)
{
}

CDS0_FontRender::~CDS0_FontRender()
{
	xr_delete(Atlas);
}

void CDS0_FontRender::Initialize(LPCSTR, LPCSTR)
{
}

void CDS0_FontRender::OnRender(CGameFont& Owner)
{
	if (!Atlas)
	{
		return;
	}

	CDS0_UIShader FontShader;
	FontShader.Texture = Atlas;
	GUIRender.SetShader(FontShader);

	const float fWidth = float(std::max(Atlas->TextureDescription.width, (u16)4));
	const float fHeight = float(std::max(Atlas->TextureDescription.height, (u16)4));

	for (CGameFont::String& String : Owner.strings)
	{
		const int Length = xr_strlen(String.string);
		if (Length == 0)
		{
			continue;
		}

		GUIRender.StartPrimitive(Length * 4, IUIRender::ptTriList, IUIRender::pttTL);

		float X = float(iFloor(String.x));
		float Y = float(iFloor(String.y));
		float Y2 = Y + String.height;

		if (String.align)
		{
			float width = float(Owner.WidthOf(String.string));
			switch (String.align)
			{
				case CGameFont::alCenter:
					X -= iFloor(width * 0.5f);
					break;
				case CGameFont::alRight:
					X -= iFloor(width);
					break;
			}
		}

		u32 Clr = String.c;
		u32 Clr2 = Clr;
		if (String.gradient)
		{
			Clr2 = String.gradientColor;
		}

		X -= 0.5f;
		Y -= 0.5f;
		Y2 -= 0.5f;

		const xr_special_char* UniStr = nullptr;
		const bool IsUTF8Str = IsUTF8(String.string);
		if (IsUTF8Str)
		{
			UniStr = Platform::ANSI_TO_TCHAR(String.string);
		}
		else
		{
			UniStr = Platform::ANSI_TO_TCHAR(String.string_utf8.c_str());
		}

		for (int i = 0; i < Length; ++i)
		{
			CGameFont::Glyph* GlyphInfo = nullptr;
			if (!IsUTF8Str)
			{
				const unsigned char* s = reinterpret_cast<const unsigned char*>(&String.string[i]);
				if (s[0] == 0xEE && s[1] == 0x80 && (s[2] >= 0x80 && s[2] <= 0xBF))
				{
					const u32 cp = 0xE000 + (s[2] - 0x80);
					GlyphInfo = const_cast<CGameFont::Glyph*>(Owner.GetGlyphInfo(cp));
					i += 2;
				}
				else
				{
					GlyphInfo = const_cast<CGameFont::Glyph*>(Owner.GetGlyphInfo(static_cast<u8>(s[0])));
				}
			}

			if (GlyphInfo == nullptr)
			{
				GlyphInfo = const_cast<CGameFont::Glyph*>(Owner.GetGlyphInfo(UniStr[i]));
				if (GlyphInfo == nullptr)
				{
					continue;
				}
			}

			if (i != 0)
			{
				X += GlyphInfo->Abc.abcA;
			}

			const float GlyphY = Y + GlyphInfo->yOffset;
			const float GlyphY2 = Y2 + GlyphInfo->yOffset;
			const float X2 = X + GlyphInfo->Abc.abcB;

			const float u1 = float(GlyphInfo->TextureCoord.left) / fWidth;
			const float u2 = float(GlyphInfo->TextureCoord.right) / fWidth;
			const float v1 = float(GlyphInfo->TextureCoord.top) / fHeight;
			const float v2 = float(GlyphInfo->TextureCoord.bottom) / fHeight;

			u32 ColorTL = Clr;
			u32 ColorBL = Clr;
			u32 ColorTR = Clr2;
			u32 ColorBR = Clr2;

			switch (String.gradientMode)
			{
				case CGameFont::gm_horz:
					break;
				case CGameFont::gm_back:
					ColorTL = Clr2;
					ColorBL = Clr2;
					break;
				case CGameFont::gm_down:
					ColorBL = Clr2;
					ColorBR = Clr;
					break;
				default:
					ColorTL = Clr2;
					ColorBR = Clr;
					break;
			}

			GUIRender.PushPoint(X, GlyphY2, .0001f, ColorBL, u1, v2);
			GUIRender.PushPoint(X, GlyphY, .0001f, ColorTL, u1, v1);
			GUIRender.PushPoint(X2, GlyphY2, .0001f, ColorBR, u2, v2);

			GUIRender.PushPoint(X2, GlyphY2, .0001f, ColorBR, u2, v2);
			GUIRender.PushPoint(X, GlyphY, .0001f, ColorTL, u1, v1);
			GUIRender.PushPoint(X2, GlyphY, .0001f, ColorTR, u2, v1);

			X = X2 + GlyphInfo->Abc.abcC + Owner.GetLetterSpacing();
		}

		GUIRender.FlushPrimitive();
	}

	GUIRender.CurrentShader = nullptr;
}

void CDS0_FontRender::CreateFontAtlas(u32 Width, u32 Height, const char* Name, void* Bitmap)
{
	RedImageTool::RedImage Image(Width, Height, 1, 1);
	memcpy(*Image, Bitmap, Image.GetSizeInMemory());

	xr_delete(Atlas);
	Atlas = new TiramisuRenderTexture2D(Name);
	Atlas->LoadFromImage(Image);
}