
#include "stdafx.h"
#include "GameFont.h"
#include "string_table.h"
#include "IGame_Persistent.h"
#include "../Include/xrRender/RenderFactory.h"
#include "../Include/xrRender/FontRender.h"
#include <freetype/freetype.h>
#include <freetype/ftfntfmt.h>

namespace
{
    class FontGlyphRaster
    {
    public:
        FT_UInt id = 0;
        FT_UInt index = 0;
        FT_Glyph_Metrics metrics{};
        FT_Bitmap bitmap{};
        int bitmapTop = 0;
        xr_vector<u8> pixels;
        bool isReady = false;
    };

    class FontGlyphRasterizer
    {
    public:
        FontGlyphRasterizer(const FT_Byte* bytes, FT_Long length, FT_Size_RequestRec request)
        {
            if (FT_Init_FreeType(&_library) != 0)
                return;
            if (FT_New_Memory_Face(_library, bytes, length, 0, &_face) != 0)
                return;
            _isReady = FT_Request_Size(_face, &request) == 0;
        }

        ~FontGlyphRasterizer()
        {
            if (_face)
                FT_Done_Face(_face);
            if (_library)
                FT_Done_FreeType(_library);
        }

        FontGlyphRasterizer(const FontGlyphRasterizer&) = delete;
        FontGlyphRasterizer& operator=(const FontGlyphRasterizer&) = delete;

        void Rasterize(FontGlyphRaster& result)
        {
            if (!_isReady || (result.index == 0 && result.id != 0))
                return;
            if (FT_Load_Glyph(_face, result.index, FT_LOAD_RENDER | FT_LOAD_TARGET_NORMAL) != 0)
                return;

            const FT_GlyphSlot glyph = _face->glyph;
            if (glyph->bitmap.pixel_mode != FT_PIXEL_MODE_GRAY || glyph->bitmap.pitch < 0)
                return;

            result.metrics = glyph->metrics;
            result.bitmap = glyph->bitmap;
            result.bitmapTop = glyph->bitmap_top;
            result.pixels.resize(size_t(glyph->bitmap.rows) * glyph->bitmap.pitch);
            if (!result.pixels.empty())
                CopyMemory(result.pixels.data(), glyph->bitmap.buffer, result.pixels.size());
            result.bitmap.buffer = result.pixels.data();
            result.isReady = true;
        }

    private:
        FT_Library _library = nullptr;
        FT_Face _face = nullptr;
        bool _isReady = false;
    };
}

extern u32 TextureDimension;
thread_local xr_vector<u32> FontBitmap;

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

CGameFont::CGameFont(const char* section, u32 flags) : Name(section), LineSpacing(1.f), LetterSpacing(0.f)
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
	
	if (pSettings->line_exist(section, "opentype"))
		Data.OpenType = pSettings->r_bool(section, "opentype");

	if (pSettings->line_exist(section, "letter_spacing"))
		SetLetterSpacing(pSettings->r_float(section, "letter_spacing"));

	if (pSettings->line_exist(section, "line_spacing"))
		SetLineSpacing(pSettings->r_float(section, "line_spacing"));

	// Init
	pFontRender = RenderFactory->CreateFontRender();
	Prepare(Data.Name, Data.Shader, Data.Style, Data.Size);
}

CGameFont::~CGameFont()
{
	// Shading
	FT_Done_Face(OurFont);

	RenderFactory->DestroyFontRender(pFontRender);
	pFontRender = nullptr;

	xr_free(Data.Shader);
	xr_free(Data.Style);
	xr_free(Data.Name);
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
	return elems;
}

bool GetDisplayMetricsSDL3(float& width_mm, float& height_mm, float& width_px, float& height_px) 
{
	SDL_DisplayID* displays = SDL_GetDisplays(nullptr);
	if (!displays)
	{
		return false;
	}

	SDL_DisplayID display_id = displays[0];
	SDL_free(displays);

	float scale = SDL_GetDisplayContentScale(display_id);
	float dpi = scale * 96.0f;  // scale в DPI

	float dpi_horizontal = scale * 96.0f;
	float dpi_vertical = scale * 96.0f;

	const SDL_DisplayMode* mode = SDL_GetCurrentDisplayMode(display_id);
	if (mode)
	{
		width_px = static_cast<float>(mode->w);
		height_px = static_cast<float>(mode->h);
	}
	else
	{
		width_px = 1920.0f;
		height_px = 1080.0f;
	}

	width_mm = (width_px / dpi_horizontal) * 25.4f;
	height_mm = (height_px / dpi_vertical) * 25.4f;

	return true;
}

void CGameFont::Initialize2(const char* name, const char* shader, const char* style, u32 size)
{
    FontBitmap.resize(size_t(TextureDimension) * 16);
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
	ZeroMemory(FontBitmap.data(), FontBitmap.size() * sizeof(u32));

	// есть кучу способов высчитать размер шрифта для скейлинга
	// 1. основываясь на DPI(PPI), однако, как не вычисляй его он всегда считается исходя из разрешения моника(системы) и 23 дюймов(мб с дровами на моник - из реальных дюймов)
	// 2. основываясь на том, как ПЫС делают скейлинг из UI_BASE_HEIGHT/UI_BASE_WIDTH и тд...
#ifdef IXR_WINDOWS
	SetProcessDPIAware();
	HDC hDCScreen = GetDC(NULL);

	auto Hmm = (float)GetDeviceCaps(hDCScreen, VERTSIZE);
	auto Wmm = (float)GetDeviceCaps(hDCScreen, HORZSIZE);
	auto Hpx = (float)GetDeviceCaps(hDCScreen, VERTRES);
	auto Wpx = (float)GetDeviceCaps(hDCScreen, HORZRES);

	ReleaseDC(NULL, hDCScreen);
#else
	float Hmm = 0.f;
	float Wmm = 0.f;
	float Hpx = 0.f;
	float Wpx = 0.f;

	GetDisplayMetricsSDL3(Wmm, Hmm, Wpx, Hpx);
#endif

	auto is_res_depend = !!pSettings->read_if_exists<bool>(Name, "res_depend", true);
	auto is_dpi_depend = !!pSettings->read_if_exists<bool>(Name, "dpi_depend", !is_res_depend);

	auto ppi = int(25.4f * sqrt(Hpx * Hpx + Wpx * Wpx) / sqrt(Hmm * Hmm + Wmm * Wmm));

	auto res_scale = is_res_depend ? float(Device.TargetHeight) / 900.0f : 1.0f;
	auto ppi_scale = is_dpi_depend ? float(ppi) / 92.0f : 1.0f;

	auto fHeight = float(size * res_scale * ppi_scale);

	CStringTable::LangName();
	xr_string NameWithExt = CStringTable::LangName() + "\\" + name + ".ttf";

	string_path FullPath;
	FS.update_path(FullPath, _game_fonts_, NameWithExt.c_str());

	IReader* FontFile = FS.r_open(FullPath);
	if (FontFile == nullptr)
	{
		Msg("! Can't open font file %s", name);

		string_path DefPath;
		NameWithExt = CStringTable::LangName() + "\\default.ttf";
		FS.update_path(DefPath, _game_fonts_, NameWithExt.c_str());
		FontFile = FS.r_open(DefPath);

		R_ASSERT3(FontFile != nullptr, "Can't find default font: %s", DefPath);
	}

	FT_Error FTError = FT_New_Memory_Face(FreetypeLib, (FT_Byte*)FontFile->pointer(), FontFile->length(), 0, &OurFont);
	R_ASSERT3(FTError == 0, "FT_New_Memory_Face return error", FullPath);

	u32 TargetX = 0;
	u32 TargetY = 0;
	u32 TargetX2 = 0;
	u32 TargetY2 = 0;

	FT_Size_RequestRec req;
	req.type = FT_SIZE_REQUEST_TYPE_CELL;
	req.width = 0;
	req.height = (uint32_t)fHeight * 64;
	req.horiResolution = 0;
	req.vertResolution = 0;
	FT_Request_Size(OurFont, &req);
#define FT_CEIL(X)  (((X + 63) & -64) / 64)

	float FontSizeInPixels = (float)(OurFont->size->metrics.ascender - OurFont->size->metrics.descender) / 64.0f;

	auto CopyGlyphImageToAtlas = [&TargetX, &TargetX2, &TargetY, &TargetY2, FontSizeInPixels](FT_Bitmap& GlyphBitmap)
		{
			TargetX2 = TargetX + GlyphBitmap.width;
			if (TargetX2 >= TextureDimension)
			{
				TargetX = 0;
				TargetX2 = GlyphBitmap.width;
				TargetY += (u32)FontSizeInPixels + 5;

				R_ASSERT2(TargetY <= TextureDimension, "Font too large, or dimension texture is too small");
			}

			u32 SourceX = 0;
			u32 SourceY = 0;

			TargetY2 = TargetY + (u32)FontSizeInPixels;
            R_ASSERT2(TargetY2 <= TextureDimension, "Font too large, or dimension texture is too small");
            const size_t RequiredPixels = size_t(TextureDimension) * std::max(16u, std::bit_ceil(TargetY2));
            if (FontBitmap.size() < RequiredPixels)
            {
                FontBitmap.resize(RequiredPixels);
            }
			u32 TargetYSaved = TargetY;
			TargetY = TargetY + u32(FontSizeInPixels - (float)GlyphBitmap.rows);

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

	//const char* Format = FT_Get_Font_Format(OurFont);
	u32 index = 0;
	auto LoadGlyph = [&](FT_UInt glyphID, FontGlyphRaster* prepared = nullptr)
	{
		u32 TrueGlyph = glyphID;
		if (Data.OpenType)
		{
			// Если символ входит в CP1251 (0-255) — конвертируем
			if (glyphID <= 0xFF)
			{
				TrueGlyph = TranslateSymbolUsingCP1251((char)glyphID);
			}
		}

		FT_UInt FreetypeCharacter = prepared ? prepared->index : FT_Get_Char_Index(OurFont, TrueGlyph);
		if (FreetypeCharacter == 0 && glyphID != 0)
		{
			Msg("! Glyph not found: %d, TrueGlyph: %d", glyphID, TrueGlyph);
			return;
		}

        const bool isPrepared = prepared && prepared->isReady;
        if (!isPrepared)
        {
		FTError = FT_Load_Glyph(OurFont, FreetypeCharacter, FT_LOAD_RENDER | FT_LOAD_TARGET_NORMAL);
		R_ASSERT3(FTError == 0, "FT_Load_Glyph return error", FullPath);
        }
		FT_GlyphSlot Glyph = OurFont->glyph;
        FT_Glyph_Metrics& GlyphMetrics = isPrepared ? prepared->metrics : Glyph->metrics;
        FT_Bitmap& GlyphBitmap = isPrepared ? prepared->bitmap : Glyph->bitmap;
        const int bitmapTop = isPrepared ? prepared->bitmapTop : Glyph->bitmap_top;

        CopyGlyphImageToAtlas(GlyphBitmap);

		RECT region;
		region.left = TargetX;
		region.right = TargetX + GlyphBitmap.width;
		region.top = TargetY;
		region.bottom = long(TargetY + FontSizeInPixels);

		ABC widths;
		widths.abcA = FT_CEIL(GlyphMetrics.horiBearingX);
		widths.abcB = GlyphBitmap.width;
		widths.abcC = FT_CEIL(GlyphMetrics.horiAdvance) - widths.abcB - widths.abcA;

		int GlyphTopScanlineOffset = int(FontSizeInPixels - GlyphBitmap.rows);
		int yOffset = -bitmapTop - GlyphTopScanlineOffset;
		yOffset += (int)FontSizeInPixels; // Return back to the center pos
		yOffset -= (int)(FontSizeInPixels / 4);

		GlyphData[glyphID] = { region, widths, yOffset };

		TargetX = TargetX2;
		TargetX += 4;
	};

    const size_t desiredWorkers = std::min(size_t(4), size_t(OurFont->num_glyphs) / 1024);
    const size_t workerCount = desiredWorkers > 1
        ? std::min(desiredWorkers, xr_max_concurrency()) : 1;
    if (workerCount > 1)
    {
        xr_vector<FontGlyphRaster> glyphs;
        glyphs.reserve(size_t(OurFont->num_glyphs));
        auto glyphID = FT_Get_First_Char(OurFont, &index);
        size_t lastValid = 0;
        while (index != 0)
        {
            const u32 trueGlyph = Data.OpenType && glyphID <= 0xff
                ? TranslateSymbolUsingCP1251((char)glyphID) : u32(glyphID);
            glyphs.emplace_back();
            auto& glyph = glyphs.back();
            glyph.id = FT_UInt(glyphID);
            glyph.index = FT_Get_Char_Index(OurFont, trueGlyph);
            if (glyph.index != 0 || glyph.id == 0)
                lastValid = glyphs.size() - 1;
            glyphID = FT_Get_Next_Char(OurFont, glyphID, &index);
        }

        const auto* fontBytes = (const FT_Byte*)FontFile->pointer();
        const FT_Long fontLength = FontFile->length();
        xr_parallel_for(size_t(0), workerCount, [&](size_t worker)
        {
            FontGlyphRasterizer rasterizer(fontBytes, fontLength, req);
            const size_t begin = glyphs.size() * worker / workerCount;
            const size_t end = glyphs.size() * (worker + 1) / workerCount;
            for (size_t i = begin; i < end; ++i)
            {
                if (i != lastValid)
                    rasterizer.Rasterize(glyphs[i]);
            }
        });

        for (auto& glyph : glyphs)
            LoadGlyph(glyph.id, &glyph);
    }
    else
    {
        auto glyphID = FT_Get_First_Char(OurFont, &index);
        while (index != 0)
        {
            LoadGlyph(glyphID);
            glyphID = FT_Get_Next_Char(OurFont, glyphID, &index);
        }
    }


	fCurrentHeight = FontSizeInPixels;

	string128 textureName;
	xr_sprintf(textureName, "$user$%s", Name); //#TODO optimize

	auto TargetDemensionY = 16u;

	while (TargetDemensionY < TargetY2) {
		TargetDemensionY *= 2u;
	}
#ifdef DEBUG
	Msg("* Font %s Y size [%d - %d]", Name, TargetDemensionY, TargetY2);
#endif
	R_ASSERT2(TargetDemensionY <= TextureDimension, "Font too large, or dimension texture is too small");

	pFontRender->CreateFontAtlas(TextureDimension, TargetDemensionY, textureName, FontBitmap.data());


	FS.r_close(FontFile);
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
	PROF_EVENT("Render Font");
	xrCriticalSectionGuard g(&s_cs);
	if (!strings.empty())
	{
		pFontRender->OnRender(*this);
		strings.clear();
	}
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
	bool bCheckDevice, bool bUseCoords, bool bScaleCoords, bool bUseSkip,
	float _x, float _y, float _skip, const char* fmt, va_list p)
{
	if (bCheckDevice && (!Device.b_is_Active))
		return;
	
	String rs;

	rs.x = (bUseCoords ? (bScaleCoords ? (DI2PX(_x)) : _x) : fCurrentX);
	rs.y = (bUseCoords ? (bScaleCoords ? (DI2PY(_y)) : _y) : fCurrentY);
	rs.c = dwCurrentColor;
	rs.gradientColor = dwGradientColor;
	rs.height = fCurrentHeight;
	rs.align = eCurrentAlignment;
	rs.gradient = fGradientEnabled;
	rs.gradientMode = fGradientMode;
	int vs_sz = vsprintf(rs.string, fmt, p);

	if (!IsUTF8(rs.string))
	{
		rs.string_utf8 = Platform::ANSI_TO_UTF8(rs.string);
	}
	
	rs.string[sizeof(rs.string) - 1] = 0;
	if (vs_sz == -1)
	{
		return;
	}

	if (vs_sz)
	{
		xrCriticalSectionGuard g(&s_cs);

		if (fOutlineEnabled && fOutlineOffset > 0.f)
		{
			static constexpr float outline_offsets[8][2] =
			{
				{ -1.f, 0.f }, { 1.f, 0.f }, { 0.f, -1.f }, { 0.f, 1.f },
				{ -1.f, -1.f }, { -1.f, 1.f }, { 1.f, -1.f }, { 1.f, 1.f }
			};

			String outline = rs;
			outline.c = dwOutlineColor;
			outline.gradient = false;

			for (const auto& [dx, dy] : outline_offsets)
			{
				outline.x = rs.x + dx * fOutlineOffset;
				outline.y = rs.y + dy * fOutlineOffset;
				strings.push_back(outline);
			}
		}

		strings.push_back(rs);
	}

	if (bUseSkip)
		OutSkip(_skip);
}

#define MASTER_OUT(CHECK_DEVICE,USE_COORDS,SCALE_COORDS,USE_SKIP,X,Y,SKIP,FMT) \
	{ va_list p; va_start ( p , fmt ); \
	  MasterOut( CHECK_DEVICE , USE_COORDS , SCALE_COORDS , USE_SKIP , X , Y , SKIP , FMT, p ); \
	  va_end( p ); }

void CGameFont::OutI(float _x, float _y, const char* fmt, ...)
{
	MASTER_OUT(false, true, true, false, _x, _y, 0.0f, fmt);
}

void CGameFont::Out(float _x, float _y, const char* fmt, ...)
{
	MASTER_OUT(true, true, false, false, _x, _y, 0.0f, fmt);
}

void CGameFont::OutNext(const char* fmt, ...)
{
	MASTER_OUT(true, false, false, true, 0.0f, 0.0f, LineSpacing, fmt);
}

void CGameFont::OutLeft(float x)
{
	fCurrentX = 0 + x;
}

void CGameFont::OutRight(float x)
{
	float max_str_size_in_px = 0.f;
	
	for (const String& str : strings)
		max_str_size_in_px = std::max(max_str_size_in_px, SizeOf_(str.string));
	
	fCurrentX = static_cast<float>(Device.TargetWidth) - max_str_size_in_px - x;
}

void CGameFont::OutTop(float y)
{
	fCurrentY = 0 + y;
}

void CGameFont::OutBottom(float y)
{
	fCurrentY = static_cast<float>(Device.TargetHeight) - GetHeight() * static_cast<float>(strings.size()) - y;
}

void CGameFont::OutSkip(float val)
{
	fCurrentY += val * CurrentHeight_();
}

float CGameFont::SizeOf_(int cChar)
{
	if (cChar < 0)
	{
		cChar = u8(cChar);
	}

	return WidthOf(cChar);
}

float CGameFont::SizeOf_(const char* s)
{
	return WidthOf(s);
}

float CGameFont::CurrentHeight_()
{
	return GetHeight();
}

void CGameFont::SetHeight(float S)
{
	if (uFlags & fsDeviceIndependent)
	{
		fCurrentHeight = S;
	}
}

const CGameFont::Glyph* CGameFont::GetGlyphInfo(int ch)
{
	auto symbolInfoIterator = GlyphData.find(ch);
	if (symbolInfoIterator == GlyphData.end())
	{
		return nullptr;
	}

	return &symbolInfoIterator->second;
}

bool CGameFont::GetAtlasTexSize(u32& outW, u32& outH) const
{
	if (pFontRender == nullptr)
	{
		return false;
	}

	return pFontRender->GetAtlasTexSize(outW, outH);
}

float CGameFont::WidthOf(int ch)
{
	if (ch == '\t' || ch == '\n')
		return 0.f;

	const float xs = g_current_font_scale.x;
	if (const Glyph* glyphInfo = GetGlyphInfo(ch))
	{
		return float(glyphInfo->Abc.abcA + (glyphInfo->Abc.abcB * GetWidthCoef()) + glyphInfo->Abc.abcC) * xs;
	}

	if (OurFont && OurFont->glyph)
		return float(OurFont->glyph->metrics.width) / 64.f * xs;

	return 0.f;
}

template <typename T>
size_t DynamicFontLen(T String)
{
	if constexpr (std::is_same_v<T, const char*>)
	{
		return std::strlen(String);
	}
	else
	{
		return std::wcslen(String);
	}
}

float CGameFont::WidthOf(const char* str)
{
	if (!str || !str[0]) return 0;

	if (!IsUTF8(str))
	{
		const xr_string utf8 = Platform::ANSI_TO_UTF8(str);
		return WidthOf(utf8.c_str());
	}

	float size = 0;
	const float spacing = GetLetterSpacing() * g_current_font_scale.x;

#ifdef IXR_APPLE_SERIES
	// HACK: TCHAR is char here; step codepoints, not UTF-8 bytes (see Platform::DecodeUtf8CodePoints).
	xr_vector<int> codePoints;
	Platform::DecodeUtf8CodePoints(str, codePoints);
	const int* wideStr = codePoints.data();
	const int length = (int)codePoints.size();
#else
	auto wideStr = Platform::ANSI_TO_TCHAR(str);
	const int length = DynamicFontLen(wideStr);
#endif
	for (int i = 0; i < length; i++)
	{
		if (g_FontManager->GamepadButtonMappings[wideStr[i]] &&
			wideStr[i] >= GAMEPAD_GLYPH_START && wideStr[i] <= GAMEPAD_GLYPH_END)
		{
			Frect rect;
			shared_str fn;
			g_pGamePersistent->GetTextureParams(g_FontManager->GamepadButtonMappings[wideStr[i]], rect, fn);
			size += sizeOfImage(rect).x;
		}
		else
		{
			size += WidthOf(wideStr[i]);
		}
		size += spacing;
	}

	size -= spacing;
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
