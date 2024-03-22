#include "stdafx.h"
#include "dxFontRender.h"
#include "dxRenderDeviceRender.h"

#include "../../xrEngine/GameFont.h"

extern ENGINE_API BOOL g_bRendering;

dxFontRender::dxFontRender()
{
}

dxFontRender::~dxFontRender()
{
	pShader.destroy();
	pGeom.destroy();
	pTexture.destroy();
}

void dxFontRender::Initialize(const char* cShader, const char* cTexture)
{
	if (pTexture._get() == nullptr)
	{
		pTexture.create(cTexture);
	}

	pShader.create(cShader, cTexture);
	pGeom.create(FVF::F_TL, RCache.Vertex.Buffer(), RCache.QuadIB);
}

void dxFontRender::OnRender(CGameFont& owner)
{
	VERIFY(g_bRendering);

	if (pShader != nullptr)
	{
		RCache.set_Shader(pShader);
	}

	for (CGameFont::String& str : owner.strings) //#TODO mb need use optimization for minimize vertexes allocations?
	{
		int length = xr_strlen(str.string);
		if (length)
		{
			// lock AGP memory
			u32	vOffset;
			FVF::TL* vertexes = (FVF::TL*)RCache.Vertex.Lock(length * 4, pGeom.stride(), vOffset);
			FVF::TL* start = vertexes;

			float X = float(iFloor(str.x));
			float Y = float(iFloor(str.y));
			float Y2 = Y + str.height;

			if (str.align)
			{
				float width = owner.WidthOf(str.string);

				switch (str.align)
				{
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
			if (owner.uFlags & CGameFont::fsGradient)
			{
				u32	_R = color_get_R(clr) / 2;
				u32	_G = color_get_G(clr) / 2;
				u32	_B = color_get_B(clr) / 2;
				u32	_A = color_get_A(clr);
				clr2 = color_rgba(_R, _G, _B, _A);
			}

			for (int i = 0; i < length; i++)
			{
				const CGameFont::Glyph* glyphInfo = owner.GetGlyphInfo(str.string[i]);
				R_ASSERT(glyphInfo != nullptr);
				if (i != 0)
				{
					X += glyphInfo->Abc.abcA;
				}

				float GlyphY = Y + glyphInfo->yOffset;
				float GlyphY2 = Y2 + glyphInfo->yOffset;

				float X2 = X + glyphInfo->Abc.abcB;

				float u1 = float(glyphInfo->TextureCoord.left) / 2048.0f;
				float u2 = float(glyphInfo->TextureCoord.right) / 2048.0f;

				float v1 = float(glyphInfo->TextureCoord.top) / 2048.0f;
				float v2 = float(glyphInfo->TextureCoord.bottom) / 2048.0f;

				vertexes->set(X, GlyphY2, clr2, u1, v2);
				++vertexes;
				vertexes->set(X, GlyphY, clr, u1, v1);
				++vertexes;
				vertexes->set(X2, GlyphY2, clr2, u2, v2);
				++vertexes;
				vertexes->set(X2, GlyphY, clr, u2, v1);
				++vertexes;

				X = X2 + glyphInfo->Abc.abcC;
			}

			// Unlock and draw
			u32 vertexesCount = (u32)(vertexes - start);
			RCache.Vertex.Unlock(vertexesCount, pGeom.stride());
			if (vertexesCount)
			{
				RCache.set_Geometry(pGeom);
				RCache.Render(D3DPT_TRIANGLELIST, vOffset, 0, vertexesCount, 0, vertexesCount / 2);
			}
		}
	}
}

void dxFontRender::CreateFontAtlas(u32 width, u32 height, const char* name, void* bitmap)
{
#ifdef USE_DX11
	D3D_TEXTURE2D_DESC descFontAtlas;
	ZeroMemory(&descFontAtlas, sizeof(D3D_TEXTURE2D_DESC));
	descFontAtlas.Width = width;
	descFontAtlas.Height = height;
	descFontAtlas.MipLevels = 1;
	descFontAtlas.ArraySize = 1;
	descFontAtlas.SampleDesc.Count = 1;
	descFontAtlas.SampleDesc.Quality = 0;
	descFontAtlas.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
	descFontAtlas.Usage = D3D_USAGE_DEFAULT;
	descFontAtlas.BindFlags = D3D11_BIND_SHADER_RESOURCE;
	descFontAtlas.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;
	descFontAtlas.MiscFlags = 0;

	D3D_SUBRESOURCE_DATA FontData;
	FontData.pSysMem = bitmap;
	FontData.SysMemSlicePitch = 0;
	FontData.SysMemPitch = width * 4;

	ID3DTexture2D* Texture = nullptr;
	R_CHK(HW.pDevice->CreateTexture2D(&descFontAtlas, &FontData, &Texture));
#else
	D3DLOCKED_RECT LockedRect = {};
	ID3DTexture2D* Texture = nullptr;
	R_CHK(D3DXCreateTexture(HW.pDevice, width, height, 1, 0, D3DFMT_A8B8G8R8, D3DPOOL_MANAGED, &Texture));
	R_CHK(Texture->LockRect(0, &LockedRect, nullptr, 0));

	for (int y = 0; y < height; y++) {
		memcpy((unsigned char*)LockedRect.pBits + (size_t)LockedRect.Pitch * y, (u8*)bitmap + (size_t)width * 4 * y, (size_t)width * 4);
	}

	R_CHK(Texture->UnlockRect(0));
#endif

	pTexture.create(name);
	pTexture->surface_set(Texture);
}