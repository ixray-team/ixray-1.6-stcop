#include "stdafx.h"
#include "dxFontRender.h"
#include "dxRenderDeviceRender.h"

#include "../../xrEngine/GameFont.h"

extern ENGINE_API xr_atomic_bool g_bRendering;

dxFontRender::dxFontRender() {}

dxFontRender::~dxFontRender() {
	pShader.destroy();
	pGeom.destroy();
	pTexture.destroy();
}

void dxFontRender::Initialize(const char* cShader, const char* cTexture) {
	if(pTexture._get() == nullptr) {
		pTexture.create(cTexture);
	}

	pShader.create(cShader, cTexture);
	pGeom.create(FVF::F_TL, RCache.Vertex.Buffer(), RCache.QuadIB);
}

void dxFontRender::OnRender(CGameFont& owner)
{
	VERIFY(g_bRendering);

	if(pShader != nullptr) {
		RCache.set_Shader(pShader);
	}

	auto fWidth = (float)std::max(pTexture->get_Width(), 4u);
	auto fHeight = (float)std::max(pTexture->get_Height(), 4u);

	//#TODO mb need use optimization for minimize vertexes allocations?
	for(CGameFont::String& str : owner.strings)
	{
		int length = xr_strlen(str.string);
		if(length)
		{
			// lock AGP memory
			u32	vOffset;
			FVF::TL* vertexes = (FVF::TL*)RCache.Vertex.Lock(length * 4, pGeom.stride(), vOffset);
			FVF::TL* start = vertexes;

			float X = float(iFloor(str.x));
			float Y = float(iFloor(str.y));
			float Y2 = Y + str.height;

			if(str.align)
			{
				float width = (float)owner.WidthOf(str.string);

				switch(str.align) {
					case CGameFont::alCenter:
					X -= iFloor(width * 0.5f);
					break;
					case CGameFont::alRight:
					X -= iFloor(width);
					break;
				}
			}

			u32	clr, clr2;
			clr2 = clr = str.c;
			if(str.gradient) {;
				clr2 = str.gradientColor;
			}

			X -= 0.5f;
			Y -= 0.5f;
			Y2 -= 0.5f;

			xr_special_char* UniStr = nullptr; 
			bool IsUTF8Str = IsUTF8(str.string);
			if (IsUTF8Str)
			{
				UniStr = Platform::ANSI_TO_TCHAR(str.string);
			}
			else
			{
				UniStr = Platform::ANSI_TO_TCHAR(str.string_utf8.c_str());
			}

			for(int i = 0; i < length; i++) 
			{
				CGameFont::Glyph* glyphInfo = nullptr;

				if (!IsUTF8Str)
				{
					glyphInfo = const_cast<CGameFont::Glyph*>(owner.GetGlyphInfo((u8)str.string[i]));
				}

				if (glyphInfo == nullptr) 
				{
					glyphInfo = const_cast<CGameFont::Glyph*>(owner.GetGlyphInfo(UniStr[i]));
					if (glyphInfo == nullptr)
					{
						continue;
					}
				}

				if(i != 0) {
					X += glyphInfo->Abc.abcA;
				}

				float GlyphY = Y + glyphInfo->yOffset;
				float GlyphY2 = Y2 + glyphInfo->yOffset;

				float X2 = X + glyphInfo->Abc.abcB;

				float u1 = float(glyphInfo->TextureCoord.left) / fWidth;
				float u2 = float(glyphInfo->TextureCoord.right) / fWidth;

				float v1 = float(glyphInfo->TextureCoord.top) / fHeight;
				float v2 = float(glyphInfo->TextureCoord.bottom) / fHeight;

				if (str.gradientMode == CGameFont::gm_horz)
				{ 
					vertexes->set(X, GlyphY2, clr, u1, v2);
					++vertexes;
					vertexes->set(X, GlyphY, clr, u1, v1);
					++vertexes;
					vertexes->set(X2, GlyphY2, clr2, u2, v2);
					++vertexes;
					vertexes->set(X2, GlyphY, clr2, u2, v1);
					++vertexes;
				}
				else if (str.gradientMode == CGameFont::gm_back)
				{
					vertexes->set(X, GlyphY2, clr2, u1, v2);
					++vertexes;
					vertexes->set(X, GlyphY, clr2, u1, v1);
					++vertexes;
					vertexes->set(X2, GlyphY2, clr, u2, v2);
					++vertexes;
					vertexes->set(X2, GlyphY, clr, u2, v1);
					++vertexes;
				}
				else if (str.gradientMode == CGameFont::gm_down)
				{
					vertexes->set(X, GlyphY2, clr, u1, v2);
					++vertexes;
					vertexes->set(X, GlyphY, clr2, u1, v1);
					++vertexes;
					vertexes->set(X2, GlyphY2, clr, u2, v2);
					++vertexes;
					vertexes->set(X2, GlyphY, clr2, u2, v1);
					++vertexes;
				}
				else
				{
					vertexes->set(X, GlyphY2, clr2, u1, v2);
					++vertexes;
					vertexes->set(X, GlyphY, clr, u1, v1);
					++vertexes;
					vertexes->set(X2, GlyphY2, clr2, u2, v2);
					++vertexes;
					vertexes->set(X2, GlyphY, clr, u2, v1);
					++vertexes;
				}
				X = X2 + glyphInfo->Abc.abcC + owner.GetLetterSpacing();
			}

			// Unlock and draw
			u32 vertexesCount = (u32)(vertexes - start);
			RCache.Vertex.Unlock(vertexesCount, pGeom.stride());

			if(vertexesCount > 0) {
				RCache.set_Geometry(pGeom);
				RCache.Render(D3DPT_TRIANGLELIST, vOffset, 0, vertexesCount, 0, vertexesCount / 2);
			}
		}
	}
}

void dxFontRender::CreateFontAtlas(u32 width, u32 height, const char* name, void* bitmap)
{
	PROF_EVENT("dxFontRender::CreateFontAtlas");

	// Заполняем описание текстуры
	RHITextureDesc rhiDesc = {};
	rhiDesc.Width = width;
	rhiDesc.Height = height;
	rhiDesc.Depth = 1;
	rhiDesc.MipLevels = 1;
	rhiDesc.Format = ERHI_FORMAT::B8G8R8A8_UNORM;
	rhiDesc.Usage = ERHI_USAGE::USAGE_DEFAULT;
	rhiDesc.BindFlags = ERHI_BIND_FLAG::SHADER_RESOURCE;
	rhiDesc.CPUAccessFlags = 0;
	rhiDesc.MiscFlags = 0;

	RHISubResource FontData;
	FontData.Data = bitmap;
	FontData.DataSize = width * 4;

	IRHISurface* rhiSurface = GRHI->CreateTexture2D(rhiDesc, FontData);
	if (!rhiSurface)
	{
		Msg("! Failed to create font atlas texture");
		return;
	}

	pTexture.create(name);
	pTexture->surface_set(rhiSurface);
}
