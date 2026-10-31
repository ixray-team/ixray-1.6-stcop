#include "stdafx.h"
#include "dxFontRender.h"
#include "dxRenderDeviceRender.h"
#include "../../xrEngine/IGame_Persistent.h"
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

	GfxCmdList& Cmd = RImplementation.Imm();
	pShader.create(cShader, cTexture);
	pGeom.create(FVF::F_TL, Cmd.Vertex.Buffer(), Cmd.QuadIB);
}

void dxFontRender::OnRender(CGameFont& owner)
{
	VERIFY(g_bRendering);
	GfxCmdList& Cmd = RImplementation.Imm();

	RenderBase(Cmd, owner);

	for (GamepadIcon s : IconsToRender)
	{
		RenderIcon(Cmd, s);
	}
	IconsToRender.clear();
}

bool dxFontRender::GetAtlasTexSize(u32& outW, u32& outH) const
{
	if (!pTexture)
	{
		return false;
	}

	outW = pTexture->get_Width();
	outH = pTexture->get_Height();
	return outW > 0 && outH > 0;
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

void dxFontRender::RenderBase(GfxCmdList& Cmd, CGameFont& owner)
{
	if(pShader != nullptr) {
		Cmd.SetPass(pShader);
	}

	auto fWidth = (float)std::max(pTexture->get_Width(), 4u);
	auto fHeight = (float)std::max(pTexture->get_Height(), 4u);

	struct TLF
	{
		struct
		{
			Fvector4 p; u32 color; Fvector2 uv;
		} buff[4];
	};
	const u32 maxGlyphs = std::min(Cmd.Vertex.GetSize() / pGeom.stride() / 4, 16384u);
	auto stringIt = owner.strings.begin();
	while (stringIt != owner.strings.end())
	{
		auto batchEnd = stringIt;
		u32 glyphCount = 0;
		for (; batchEnd != owner.strings.end(); ++batchEnd)
		{
			const u32 length = xr_strlen(batchEnd->string);
			R_ASSERT(length <= maxGlyphs);
			if (length > maxGlyphs - glyphCount)
			{
				break;
			}
			glyphCount += length;
		}
		if (!glyphCount)
		{
			stringIt = batchEnd;
			continue;
		}
		u32 vOffset;
		TLF* vertexes = (TLF*)Cmd.Vertex.Lock(glyphCount * 4, pGeom.stride(), vOffset);
		TLF* start = vertexes;

		for (; stringIt != batchEnd; ++stringIt)
		{
			CGameFont::String& str = *stringIt;
			int length = xr_strlen(str.string);
			if (!length)
			{
				continue;
			}

			float X = float(iFloor(str.x));
			float Y = float(iFloor(str.y));
			float Y2 = Y + str.height;
			const float xs = g_current_font_scale.x;

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

			const xr_special_char* UniStr = nullptr; 
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
					const unsigned char* s = (const unsigned char*)&str.string[i];

					// проверка на UTF-8 PUA (EE 80 80–FF)
					if (s[0] == 0xEE && (s[1] >= 0x80 && s[1] <= 0x82) &&
						(s[2] >= 0x80 && s[2] <= 0xFF))
					{
						u32 cp = 0xE000 + (s[1] - 0x80 == 0 ? 0 : s[1] - 0x41) + (s[2] - 0x80);

						glyphInfo = const_cast<CGameFont::Glyph*>(owner.GetGlyphInfo(cp));

						i += 2;
						if (shared_str texName = g_FontManager->GamepadButtonMappings[cp])
						{
							Frect rect;
							shared_str fileName;
							g_pGamePersistent->GetTextureParams(texName, rect, fileName);
							GamepadIcon icon;
							icon.texture_name = texName;
							icon.file_name = fileName;
							icon.rect = rect;
							icon.pos = {X, Y};
							icon.sz = owner.sizeOfImage(rect);
							icon.alpha = color_get_A(owner.dwCurrentColor);

							IconsToRender.push_back(icon);

							X += icon.sz.x + owner.GetLetterSpacing() * xs;
							continue;
						}
					}
					else
					{
						glyphInfo = const_cast<CGameFont::Glyph*>(owner.GetGlyphInfo((u8)s[0]));
					}
				}

				if (glyphInfo == nullptr) 
				{
					glyphInfo = const_cast<CGameFont::Glyph*>(owner.GetGlyphInfo(UniStr[i]));
					if (UniStr[i] >= GAMEPAD_GLYPH_START && UniStr[i] <= GAMEPAD_GLYPH_END)
					{
						if (shared_str texName = g_FontManager->GamepadButtonMappings[UniStr[i]])
						{
							Frect rect;
							shared_str fileName;
							g_pGamePersistent->GetTextureParams(texName, rect, fileName);
							GamepadIcon icon;
							icon.texture_name = texName;
							icon.file_name = fileName;
							icon.rect = rect;
							icon.pos = {X, Y};
							icon.sz = owner.sizeOfImage(rect);
							icon.alpha = color_get_A(owner.dwCurrentColor);

							IconsToRender.push_back(icon);

							X += icon.sz.x + owner.GetLetterSpacing() * xs;
							continue;
						}
					}
					if (glyphInfo == nullptr)
					{
						continue;
					}
				}

				if(i != 0) {
					X += glyphInfo->Abc.abcA * xs;
				}

				float GlyphY = Y + glyphInfo->yOffset;
				float GlyphY2 = Y2 + glyphInfo->yOffset;

				float X2 = X + glyphInfo->Abc.abcB * owner.GetWidthCoef() * xs;

				float u1 = float(glyphInfo->TextureCoord.left) / fWidth;
				float u2 = float(glyphInfo->TextureCoord.right) / fWidth;

				float v1 = float(glyphInfo->TextureCoord.top) / fHeight;
				float v2 = float(glyphInfo->TextureCoord.bottom) / fHeight;

				if (str.gradientMode == CGameFont::gm_horz)
				{ 
					*vertexes =
					{ 
						Fvector4{X, GlyphY2,.0001f,.9999f}, clr, Fvector2{u1, v2},
						Fvector4{X, GlyphY,.0001f,.9999f}, clr, Fvector2{u1, v1},
						Fvector4{X2, GlyphY2,.0001f,.9999f}, clr2, Fvector2{u2, v2},
						Fvector4{X2, GlyphY,.0001f,.9999f}, clr2, Fvector2{u2, v1},
					};
				}
				else if (str.gradientMode == CGameFont::gm_back)
				{
					*vertexes =
					{
						Fvector4{X, GlyphY2,.0001f,.9999f}, clr2, Fvector2{u1, v2},
						Fvector4{X, GlyphY,.0001f,.9999f}, clr2, Fvector2{u1, v1},
						Fvector4{X2, GlyphY2,.0001f,.9999f}, clr, Fvector2{u2, v2},
						Fvector4{X2, GlyphY,.0001f,.9999f}, clr, Fvector2{u2, v1}
					};
				}
				else if (str.gradientMode == CGameFont::gm_down)
				{
					*vertexes =
					{
						Fvector4{X, GlyphY2,.0001f,.9999f}, clr, Fvector2{u1, v2},
						Fvector4{X, GlyphY,.0001f,.9999f}, clr2, Fvector2{u1, v1},
						Fvector4{X2, GlyphY2,.0001f,.9999f}, clr, Fvector2{u2, v2},
						Fvector4{X2, GlyphY,.0001f,.9999f}, clr2, Fvector2{u2, v1}
					};
				}
				else
				{
					*vertexes =
					{
						Fvector4{X, GlyphY2,.0001f,.9999f}, clr2, Fvector2{u1, v2},
						Fvector4{X, GlyphY,.0001f,.9999f}, clr, Fvector2{u1, v1},
						Fvector4{X2, GlyphY2,.0001f,.9999f}, clr2, Fvector2{u2, v2},
						Fvector4{X2, GlyphY,.0001f,.9999f}, clr, Fvector2{u2, v1}
					};
				}
				++vertexes;
				X = X2 + glyphInfo->Abc.abcC * xs + owner.GetLetterSpacing() * xs;
			}

		}

		// Unlock and draw
		u32 vertexesCount = (u32)(vertexes - start) * 4;
		Cmd.Vertex.Unlock(vertexesCount, pGeom.stride());
		if (vertexesCount)
		{
			Cmd.SetGeometry(pGeom);
			Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, (vertexesCount / 2) * 3, 0, vOffset);
		}
	}
}

void dxFontRender::RenderIcon(GfxCmdList& Cmd, GamepadIcon icon)
{
	if (!icon.shader)
	{
		icon.shader.create("hud\\default", icon.file_name.c_str());
	}
	Cmd.SetPass(icon.shader);

	float fWidth = Cmd.GetTexture(0)->get_Width();
	float fHeight = Cmd.GetTexture(0)->get_Height();

	// lock AGP memory
	u32	vOffset;
	struct TLF
	{
		struct
		{
			Fvector4 p; u32 color; Fvector2	uv;
		} buff[4];
	};
	TLF* vertexes = (TLF*)Cmd.Vertex.Lock(4, pGeom.stride(), vOffset);
	TLF* start = vertexes;

	float Y2 = icon.pos.y + icon.sz.y;

	u32	clr = color_argb(icon.alpha, 255, 255, 255);

	float X2 = icon.pos.x + icon.sz.x;

	float u1 = float(icon.rect.left) / fWidth;
	float u2 = float(icon.rect.right) / fWidth;

	float v1 = float(icon.rect.top) / fHeight;
	float v2 = float(icon.rect.bottom) / fHeight;

	*vertexes =
	{
		Fvector4{icon.pos.x, Y2,.0001f,.9999f}, clr, Fvector2{u1, v2},
		Fvector4{icon.pos.x, icon.pos.y,.0001f,.9999f}, clr, Fvector2{u1, v1},
		Fvector4{X2, Y2,.0001f,.9999f}, clr, Fvector2{u2, v2},
		Fvector4{X2, icon.pos.y,.0001f,.9999f}, clr, Fvector2{u2, v1}
	};
	++vertexes;

	// Unlock and draw
	u32 vertexesCount = (u32)(vertexes - start) * 4;
	Cmd.Vertex.Unlock(vertexesCount, pGeom.stride());

	Cmd.SetGeometry(pGeom);
	Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, (vertexesCount / 2) * 3, 0, vOffset);
}
