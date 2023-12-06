﻿
#include "stdafx.h"

#include "GameFont.h"
#include "../Include/xrAPI/xrAPI.h"
#include "../Include/xrRender/RenderFactory.h"
#include "../Include/xrRender/FontRender.h"
#include <freetype/freetype.h>

FT_Library FreetypeLib = nullptr;

bool CGameFont::bFreetypeInitialized = false;

void CGameFont::InitializeFreetype()
{
	FT_Error Error = FT_Init_FreeType(&FreetypeLib);
	R_ASSERT2(Error == 0, "Freetype2 initialize failed");
}

#define DI2PX(x) float(iFloor((x + 1) * float(Device.TargetWidth) * 0.5f))
#define DI2PY(y) float(iFloor((y + 1) * float(Device.TargetHeight) * 0.5f))

ENGINE_API Fvector2	g_current_font_scale = { 1.0f, 1.0f };

CGameFont::CGameFont(const char* section, u32 flags) : Name(section)
{
#ifdef DEBUG
	Msg("* Init font %s", section);
#endif

	uFlags = flags;

	// Read font name
	if (pSettings->line_exist(section, "name"))
		Data.Name = xr_strdup(xr_strlwr((char*)pSettings->r_string(section, "name")));

	if (!Data.Name || !xr_strlen(Data.Name))
		Data.Name = xr_strdup(section);

	// Set font shader and style
	Data.Shader = xr_strdup(pSettings->r_string(section, "shader"));

	Data.Style = nullptr;
	if (pSettings->line_exist(section, "style"))
		Data.Style = xr_strdup(pSettings->r_string(section, "style"));

	// Read font size
	Data.Size = 14;
	if (pSettings->line_exist(section, "size"))
		Data.Size = (u16)pSettings->r_u32(section, "size");

	if (pSettings->line_exist(section, "letter_spacing"))
		LetterSpacing = pSettings->r_float(section, "letter_spacing");

	if (pSettings->line_exist(section, "line_spacing"))
		LineSpacing = pSettings->r_float(section, "line_spacing");

	// Init
	Prepare(Data.Name, Data.Shader, Data.Style, Data.Size);
}

CGameFont::~CGameFont()
{
	// Shading
	RenderFactory->DestroyFontRender(pFontRender);
	pFontRender = nullptr;
}

void CGameFont::ReInit()
{
	Prepare(Data.Name, Data.Shader, Data.Style, Data.Size);
}

void CGameFont::Prepare(const char* name, const char* shader, const char* style, u32 size)
{
	Initialize2(name, shader, style, size);
}

wchar_t TranslateSymbolUsingCP1251(char Symbol);

xr_vector<xr_string> split(const xr_string& s, char delim)
{
	xr_vector<xr_string> elems;
	std::stringstream ss(s);
	xr_string item;
	while (std::getline(ss, item, delim))
	{
		elems.push_back(item);
	}
	return std::move(elems);
}

void CGameFont::Initialize2(const char* name, const char* shader, const char* style, u32 size)
{
	if (!bFreetypeInitialized)
	{
		InitializeFreetype();
		bFreetypeInitialized = true;
	}

	ZeroMemory(&Style, sizeof(Style));
	Size = size;

	if (style != nullptr)
	{
		xr_string StyleDesc(style);
		xr_vector<xr_string> StyleTokens = split(StyleDesc, '|');
		for (const xr_string& token : StyleTokens)
		{
			if (token == "bold")
			{
				Style.bold = 1;
			}
			else if (token == "italic")
			{
				Style.italic = 1;
			}
			else if (token == "underline")
			{
				Style.underline = 1;
			}
			else if (token == "strike")
			{
				Style.strike = 1;
			}
		}
	}

	constexpr u32 TextureDimension = 2048; //#TODO calculate size based on font size
	u32* FontBitmap = (u32*)Memory.mem_alloc((TextureDimension * TextureDimension) * 4);

	// есть кучу способов высчитать размер шрифта для скейлинга
	// 1. основываясь на DPI(PPI), однако, как не вычисляй его он всегда считается исходя из разрешения моника(системы) и 23 дюймов(мб с дровами на моник - из реальных дюймов)
	// 2. основываясь на том, как ПЫС делают скейлинг из UI_BASE_HEIGHT/UI_BASE_WIDTH и тд...

	constexpr float DEFAULT_WND_Y = 800.0f;
	auto CalcConstant = []()
		{
			u32 Constant = 2;
			u32 scale = u32(Device.TargetHeight / 1000);

			if (scale >= 1)
				Constant *= 2 * scale;

			return Constant;
		};

	float fHeight = 0.0f;

	if (Device.TargetHeight > DEFAULT_WND_Y)
		fHeight = (Device.TargetHeight / DEFAULT_WND_Y) * CalcConstant() + size;
	else
		fHeight = (float)size - (2 / CalcConstant() + 0.5f);

	xr_string NameWithExt = name;
	NameWithExt += ".ttf";

	string_path FullPath;
	FS.update_path(FullPath, _game_fonts_, NameWithExt.c_str());

	IReader* FontFile = FS.r_open(FullPath);
	R_ASSERT3(FontFile != nullptr, "Can't open font file", name);

	FT_Face OurFont;
	FT_Error FTError = FT_New_Memory_Face(FreetypeLib, (FT_Byte*)FontFile->pointer(), FontFile->length(), 0, &OurFont);
	R_ASSERT3(FTError == 0, "FT_New_Memory_Face return error", FullPath);

	u32 TargetX = 0;
	u32 TargetY = 0;
	u32 TargetX2 = 0;

	//FT_Select_Charmap(OurFont, FT_ENCODING_UNICODE);

// 	FTError = FT_Set_Pixel_Sizes(OurFont, 0, fHeight);
// 	R_ASSERT3(FTError == 0, "FT_Set_Pixel_Sizes return error", FullPath);

	FT_Size_RequestRec req;
	req.type = FT_SIZE_REQUEST_TYPE_NOMINAL;
	req.width = 0;
	req.height = (uint32_t)fHeight * 64;
	req.horiResolution = 0;
	req.vertResolution = 0;
	FT_Request_Size(OurFont, &req);

#define FT_CEIL(X)  (((X + 63) & -64) / 64)

	float FontSizeInPixels = (float)FT_CEIL(OurFont->size->metrics.height);

	auto CopyGlyphImageToAtlas = [this, TextureDimension, &TargetX, &TargetX2, &TargetY, FontBitmap, FontSizeInPixels](FT_Bitmap& GlyphBitmap)
		{
			TargetX2 = TargetX + GlyphBitmap.width;
			if (TargetX2 >= TextureDimension)
			{
				TargetX = 0;
				TargetX2 = GlyphBitmap.width;
				TargetY += (float)FontSizeInPixels + 5;

				R_ASSERT2(TargetY <= TextureDimension, "Font too large, or dimension texture is too small");
			}

			u32 SourceX = 0;
			u32 SourceY = 0;

			u32 TargetY2 = TargetY + (u32)FontSizeInPixels;
			u32 TargetYSaved = TargetY;
			TargetY = TargetY + FontSizeInPixels - (float)GlyphBitmap.rows;

			switch (GlyphBitmap.pixel_mode)
			{
			case FT_PIXEL_MODE_GRAY:
			{
				u8* SourceImage = GlyphBitmap.buffer;
				for (u32 y = TargetY; y < TargetY2; y++, SourceY++)
				{
					for (u32 x = TargetX; x < TargetX2; x++, SourceX++)
					{
						u8 SourcePixel = SourceImage[(SourceY * GlyphBitmap.pitch) + SourceX];

						u32 FinalPixel = SourcePixel;
						FinalPixel |= (SourcePixel << 8);
						FinalPixel |= (SourcePixel << 16);
						FinalPixel |= (SourcePixel << 24);

						FontBitmap[(y * TextureDimension) + x] = FinalPixel;
					}
					SourceX = 0;
				}
			}
			break;
			case FT_PIXEL_MODE_BGRA:
			{
				u32* SourceImage = (u32*)GlyphBitmap.buffer;
				for (u32 y = TargetY; y < TargetY2; y++, SourceY++)
				{
					for (u32 x = TargetX; x < TargetX2; x++, SourceX++)
					{
						u32 SourcePixel = SourceImage[(SourceY * GlyphBitmap.pitch) + SourceX];

						u8 Alpha = (SourcePixel & 0x000000FF);
						u8 Red = (SourcePixel & 0x0000FF00) >> 8;
						u8 Green = (SourcePixel & 0x00FF0000) >> 16;
						u8 Blue = (SourcePixel & 0xFF000000) >> 24;

						u32 FinalPixel = Alpha;
						FinalPixel |= (u32(Blue) << 8);
						FinalPixel |= (u32(Green) << 16);
						FinalPixel |= (u32(Red) << 24);

						FontBitmap[(y * TextureDimension) + x] = FinalPixel;
					}
					SourceX = 0;
				}
			}
			break;
			default:
				break;
			}

			TargetY = TargetYSaved;
		};

	for (u32 glyphID = FirstChar; glyphID <= LastChar; glyphID++)
	{
		//u32 TrueGlyph = TranslateSymbolUsingCP1251((char)glyphID);

		FT_UInt FreetypeCharacter = FT_Get_Char_Index(OurFont, glyphID);

		FTError = FT_Load_Glyph(OurFont, FreetypeCharacter, FT_LOAD_RENDER);
		R_ASSERT3(FTError == 0, "FT_Load_Glyph return error", FullPath);
		FT_GlyphSlot Glyph = OurFont->glyph;
		FT_Glyph_Metrics& GlyphMetrics = Glyph->metrics;

		CopyGlyphImageToAtlas(Glyph->bitmap);

		RECT region;
		region.left = TargetX;
		region.right = TargetX + Glyph->bitmap.width;
		region.top = TargetY;
		region.bottom = TargetY + FontSizeInPixels;

		ABC widths;
		widths.abcA = FT_CEIL(GlyphMetrics.horiBearingX);
		widths.abcB = Glyph->bitmap.width;
		widths.abcC = FT_CEIL(GlyphMetrics.horiAdvance) - widths.abcB - widths.abcA;

		int GlyphTopScanlineOffset = FontSizeInPixels - Glyph->bitmap.rows;
		int yOffset = -Glyph->bitmap_top - GlyphTopScanlineOffset; //#GIPERION: Magic thing, that align text by baseline.
		yOffset += FontSizeInPixels; // Return back to the center pos
		yOffset -= (FontSizeInPixels / 4);

		GlyphData[(char)glyphID] = { region, widths, yOffset };

		TargetX = TargetX2;
		TargetX += 4;
	}

	FT_Done_Face(OurFont);
	fCurrentHeight = FontSizeInPixels;

	string128 textureName;
	xr_sprintf(textureName, "$user$%s", Name); //#TODO optimize

	pFontRender = RenderFactory->CreateFontRender();
	pFontRender->CreateFontAtlas(TextureDimension, TextureDimension, textureName, FontBitmap);
	Memory.mem_free(FontBitmap);

	pFontRender->Initialize(shader, textureName);
}

void CGameFont::OutSet(float x, float y)
{
	fCurrentX = x;
	fCurrentY = y;
}

void CGameFont::OutSetI(float x, float y)
{
	OutSet(DI2PX(x), DI2PY(y));
}

void CGameFont::OnRender()
{
	pFontRender->OnRender(*this);
	if (!strings.empty())
		strings.clear();
}

u16 CGameFont::GetCutLengthPos(float fTargetWidth, const char* pszText)
{

	return 0;
}

u16 CGameFont::SplitByWidth(u16* puBuffer, u16 uBufferSize, float fTargetWidth, const char* pszText)
{

	return 0;
}

void CGameFont::MasterOut(
	BOOL bCheckDevice, BOOL bUseCoords, BOOL bScaleCoords, BOOL bUseSkip,
	float _x, float _y, float _skip, const char* fmt, va_list p)
{
	if (bCheckDevice && (!Device.b_is_Active))
		return;

	String rs;

	rs.x = (bUseCoords ? (bScaleCoords ? (DI2PX(_x)) : _x) : fCurrentX);
	rs.y = (bUseCoords ? (bScaleCoords ? (DI2PY(_y)) : _y) : fCurrentY);
	rs.c = dwCurrentColor;
	rs.height = fCurrentHeight;
	rs.align = eCurrentAlignment;
#ifndef	_EDITOR
	int vs_sz = vsprintf_s(rs.string, fmt, p);
#else
	int vs_sz = vsprintf(rs.string, fmt, p);
#endif
	//VERIFY( ( vs_sz != -1 ) && ( rs.string[ vs_sz ] == '\0' ) );

	rs.string[sizeof(rs.string) - 1] = 0;
	if (vs_sz == -1)
	{
		return;
	}

	if (vs_sz)
		strings.push_back(rs);

	if (bUseSkip)
		OutSkip(_skip);
}

#define MASTER_OUT(CHECK_DEVICE,USE_COORDS,SCALE_COORDS,USE_SKIP,X,Y,SKIP,FMT) \
	{ va_list p; va_start ( p , fmt ); \
	  MasterOut( CHECK_DEVICE , USE_COORDS , SCALE_COORDS , USE_SKIP , X , Y , SKIP , FMT, p ); \
	  va_end( p ); }

void CGameFont::OutI(float _x, float _y, const char* fmt, ...)
{
	MASTER_OUT(FALSE, TRUE, TRUE, FALSE, _x, _y, 0.0f, fmt);
};

void CGameFont::Out(float _x, float _y, const char* fmt, ...)
{
	MASTER_OUT(TRUE, TRUE, FALSE, FALSE, _x, _y, 0.0f, fmt);
};

void CGameFont::OutNext(const char* fmt, ...)
{
	MASTER_OUT(TRUE, FALSE, FALSE, TRUE, 0.0f, 0.0f, 1.0f, fmt);
};

void CGameFont::OutSkip(float val)
{
	fCurrentY += val * CurrentHeight_();
}

float CGameFont::SizeOf_(const char cChar)
{
	//return (GetCharTC((u16)(u8)(((IsMultibyte() && cChar == ' ')) ? 0 : cChar)).z * vInterval.x);
	return static_cast<float>(WidthOf(cChar));
}

float CGameFont::SizeOf_(const char* s)
{
	//if (!(s && s[0]))
	//	return 0;

	//if (IsMultibyte())
	//{
	//	wide_char wsStr[MAX_MB_CHARS];
	//	mbhMulti2Wide(wsStr, NULL, MAX_MB_CHARS, s);
	//	return SizeOf_(wsStr);
	//}

	//int	len = xr_strlen(s);
	//float X = 0;
	//if (len)
	//	for (int j = 0; j < len; j++)
	//		X += GetCharTC((u16)(u8)s[j]).z;

	//return(X * vInterval.x);
	return static_cast<float> (WidthOf(s));
}

float CGameFont::SizeOf_(const wide_char* wsStr)
{
	//if (!(wsStr && wsStr[0]))
	//	return 0;

	//unsigned int len = wsStr[0];
	//float X = 0.0f, fDelta = 0.0f;

	//if (len)
	//	for (unsigned int j = 1; j <= len; j++) {
	//		fDelta = GetCharTC(wsStr[j]).z - 2;
	//		if (IsNeedSpaceCharacter(wsStr[j]))
	//			fDelta += fXStep;
	//		X += fDelta;
	//	}

	//return (X * vInterval.x);
	return 0;
}

float CGameFont::CurrentHeight_()
{
	return GetHeight();//fCurrentHeight * vInterval.y;
}

void CGameFont::SetHeight(float S)
{
	if (uFlags & fsDeviceIndependent)
	{
		fCurrentHeight = S;
	}
}

//void CGameFont::SetHeightI(float S)
//{
//	if (uFlags & fsDeviceIndependent)
//	{
//		fCurrentHeight = S * RDevice.CurrentViewport->Height;
//	}
//}

const CGameFont::Glyph* CGameFont::GetGlyphInfo(char ch)
{
	auto symbolInfoIterator = GlyphData.find(ch);
	if (symbolInfoIterator == GlyphData.end())
	{
		return nullptr;
	}

	return &symbolInfoIterator->second;
}

int CGameFont::WidthOf(const char ch)
{
	const Glyph* glyphInfo = GetGlyphInfo(ch);
	return glyphInfo ? (glyphInfo->Abc.abcA + glyphInfo->Abc.abcB + glyphInfo->Abc.abcC) : 0;
}

int CGameFont::WidthOf(const char* str)
{
	if (!str || !str[0])
	{
		return 0;
	}

	int length = xr_strlen(str);
	int size = 0;

	for (int i = 0; i < length; i++)
	{
		size += WidthOf(str[i]);
	}

	return size;
}

wchar_t CP1251ConvertationTable[] =
{
	0x0402, // Ђ
	0x0403, // Ѓ
	0x201A, // ‚
	0x0453, // ѓ
	0x201E, // „
	0x2026, // …
	0x2020, // †
	0x2021, // ‡
	0x20AC, // €
	0x2030, // ‰
	0x0409, // Љ
	0x2039, // ‹
	0x040A, // Њ
	0x040C, // Ќ
	0x040B, // Ћ
	0x040F, // Џ

	0x0452, // ђ
	0x2018, // ‘
	0x2019, // ’
	0x201C, // “
	0x201D, // ”
	0x2022, // •
	0x2013, // –
	0x2014, // —
	0x0,    // empty (0x98)
	0x2122, // ™
	0x0459, // љ
	0x203A, // ›
	0x045A, // њ
	0x045C, // ќ
	0x045B, // ћ
	0x045F, // џ

	0x00A0, //  
	0x040E, // Ў
	0x045E, // ў
	0x0408, // Ј
	0x00A4, // ¤
	0x0490, // Ґ
	0x00A6, // ¦
	0x00A7, // §
	0x0401, // Ё
	0x00A9, // ©
	0x0404, // Є
	0x00AB, // «
	0x00AC, // ¬
	0x00AD, // 
	0x00AE, // ®
	0x0407, // Ї

	0x00B0, // °
	0x00B1, // ±
	0x0406, // І
	0x0456, // і
	0x0491, // ґ
	0x00B5, // µ
	0x00B6, // ¶
	0x00B7, // ·
	0x0451, // ё
	0x2116, // №
	0x0454, // є
	0x00BB, // »
	0x0458, // ј
	0x0405, // Ѕ
	0x0455, // ѕ
	0x0457, // ї
};

wchar_t TranslateSymbolUsingCP1251(char Symbol)
{
	unsigned char RawSymbol = *(unsigned char*)&Symbol;

	if (RawSymbol < 0x80)
	{
		return wchar_t(RawSymbol);
	}

	if (RawSymbol < 0xc0)
	{
		return CP1251ConvertationTable[RawSymbol - 0x80];
	}

	return wchar_t(RawSymbol - 0xc0) + 0x410;
}