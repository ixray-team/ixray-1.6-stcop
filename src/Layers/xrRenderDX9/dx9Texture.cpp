// Texture.cpp: implementation of the CTexture class.
//
//////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include <magic_enum/magic_enum.hpp>

#include "DDSTextureLoader9.h"
using namespace DirectX;

#ifndef _EDITOR
#include "../xrRender/dxRenderDeviceRender.h"
#else
#include <RedImage/RedImage.hpp>
#endif

void fix_texture_name(LPSTR fn) {
	LPSTR _ext = strext(fn);
	if (_ext &&
		(0 == _stricmp(_ext, ".tga") ||
			0 == _stricmp(_ext, ".dds") ||
			0 == _stricmp(_ext, ".bmp") ||
			0 == _stricmp(_ext, ".ogm")))
		*_ext = 0;
}

int get_texture_load_lod(LPCSTR fn) {
	auto& sect = pSettings->r_section("reduce_lod_texture_list");

	for (const auto& data : sect.Data) {
		if (strstr(fn, data.first.c_str())) {
			if (psTextureLOD < 1) {
				return 0;
			}
			else {
				if (psTextureLOD < 3) {
					return 1;
				}
				else {
					return 2;
				}
			}
		}
	}

	if (psTextureLOD < 2) {
		return 0;
	}
	else {
		if (psTextureLOD < 4) {
			return 1;
		}
		else {
			return 2;
		}
	}
}

u32 calc_texture_size(int lod, u32 mip_cnt, u32 orig_size) {
	if (1 == mip_cnt)
		return orig_size;

	int _lod = lod;
	float res = float(orig_size);

	while (_lod > 0) {
		--_lod;
		res -= res / 1.333f;
	}
	return iFloor(res);
}

//////////////////////////////////////////////////////////////////////
// Utility pack
//////////////////////////////////////////////////////////////////////

IC void	Reduce(int& w, int& h, int& l, int& skip) {
	while ((l > 1) && (w > 4) && (h > 4) && skip)
	{
		w /= 2;
		h /= 2;
		l -= 1;

		skip--;
	}
	if (w < 4) w = 4;
	if (h < 4) h = 4;
}

ID3DTexture2D* TW_LoadTextureFromTexture(ID3DTexture2D* t_from, int levels_2_skip)
{
	D3DSURFACE_DESC t_from_desc = { };
	t_from->GetLevelDesc(0, &t_from_desc);

	ID3DTexture2D* t_dest = nullptr;

	// Calculate levels & dimensions
	int top_width = t_from_desc.Width;
	int top_height = t_from_desc.Height;
	int levels_exist = t_from->GetLevelCount();

	Reduce(top_width, top_height, levels_exist, levels_2_skip);

	R_CHK(RDevice->CreateTexture(
		top_width, top_height,
		levels_exist, 0, t_from_desc.Format,
		D3DPOOL_DEFAULT, &t_dest, 0
	));

	// Copy surfaces & destroy temporary
	ID3DTexture2D* T_src = t_from;
	ID3DTexture2D* T_dst = t_dest;

	int L_src = T_src->GetLevelCount() - 1;
	int L_dst = T_dst->GetLevelCount() - 1;

	for (; L_dst >= 0; L_src--, L_dst--) {
		IDirect3DSurface9* S_src, * S_dst;
		R_CHK(T_src->GetSurfaceLevel(L_src, &S_src));
		R_CHK(T_dst->GetSurfaceLevel(L_dst, &S_dst));

		// Copy
		R_CHK(RDevice->UpdateSurface(S_src, nullptr, S_dst, nullptr));

		// Release surfaces
		_RELEASE(S_src);
		_RELEASE(S_dst);
	}

	return t_dest;
}


#include <string>
#include <unordered_map>
#include <ddraw.h>

shared_str D3DFormatToString(D3DFORMAT format)
{
	if (auto name = magic_enum::enum_name(static_cast<D3DFORMAT>(format));
		!name.empty())
	{
		return name.data();
	}

	return "D3DFMT_UNKNOWN";
}

void PrintTextureError(HRESULT hr, const char* fname, const void* ddsData, size_t ddsSize, IDirect3DBaseTexture9* pTexture = nullptr, bool PrintMem = true)
{
	// Получаем возможности устройства
	D3DCAPS9 caps;
	RDevice->GetDeviceCaps(&caps);

	string1024 msg;

	xr_sprintf(msg, "=== D3D TEXTURE LOAD ERROR ===");
	Msg(msg);

	xr_sprintf(msg, "File: %s", fname);
	Msg(msg);

	xr_sprintf(msg, "Error: 0x%08X (%s)", hr, Debug.dxerror2string(hr));
	Msg(msg);

	// Анализируем DDS заголовок
	if (PrintMem && ddsData && ddsSize >= sizeof(DWORD) + sizeof(DDS_HEADER))
	{
		const DWORD* magic = reinterpret_cast<const DWORD*>(ddsData);
		const DDS_HEADER* header = reinterpret_cast<const DDS_HEADER*>(magic + 1);

		// Дамп заголовка
		Msg("--- DDS Header Info ---");
		Msg("  Magic: 0x%08X ('%c%c%c%c')",
			*magic,
			((const char*)magic)[0],
			((const char*)magic)[1],
			((const char*)magic)[2],
			((const char*)magic)[3]);

		Msg("  Width: %u", header->width);
		Msg("  Height: %u", header->height);
		Msg("  Mips: %u", header->mipMapCount);
		Msg("  FourCC: 0x%08X ('%c%c%c%c')",
			header->ddspf.fourCC,
			((const char*)&header->ddspf.fourCC)[0],
			((const char*)&header->ddspf.fourCC)[1],
			((const char*)&header->ddspf.fourCC)[2],
			((const char*)&header->ddspf.fourCC)[3]);
		Msg("  Flags: 0x%08X", header->flags);
		Msg("  Caps: 0x%08X", header->caps);

		// Определяем формат
		D3DFORMAT d3dFormat = D3DFMT_UNKNOWN;
		if (header->ddspf.flags & DDPF_FOURCC) {
			switch (header->ddspf.fourCC) {
			case MAKEFOURCC('D', 'X', 'T', '1'): d3dFormat = D3DFMT_DXT1; break;
			case MAKEFOURCC('D', 'X', 'T', '3'): d3dFormat = D3DFMT_DXT3; break;
			case MAKEFOURCC('D', 'X', 'T', '5'): d3dFormat = D3DFMT_DXT5; break;
			default: break;
			}
		}

		bool sizeValid = (header->width <= caps.MaxTextureWidth) &&
			(header->height <= caps.MaxTextureHeight);

		auto IsPOT = [](UINT x) { return x && !(x & (x - 1)); };
		bool isPOT = IsPOT(header->width) && IsPOT(header->height);

		Msg("--- Validation ---");
		Msg("  Format supported: %s", (d3dFormat != D3DFMT_UNKNOWN) ? "YES" : "NO");
		Msg("  Size valid: %s", sizeValid ? "YES" : "NO");
		Msg("  Power-of-two: %s", isPOT ? "YES" : "NO");
	}
	else
	{
		Msg("! Invalid DDS data (size: %zu)", ddsSize);
	}

	// Информация о созданной текстуре
	if (pTexture)
	{
		D3DRESOURCETYPE resType = pTexture->GetType();
		Msg("--- Created Texture Info ---");
		Msg("  Type: %s",
			resType == D3DRTYPE_TEXTURE ? "2D" :
			resType == D3DRTYPE_CUBETEXTURE ? "CUBE" :
			resType == D3DRTYPE_VOLUMETEXTURE ? "VOLUME" : "UNKNOWN");

		D3DSURFACE_DESC desc;
		if (resType == D3DRTYPE_TEXTURE && SUCCEEDED(((IDirect3DTexture9*)pTexture)->GetLevelDesc(0, &desc)))
		{
			Msg("  Actual Format: %s", *D3DFormatToString(desc.Format));
			Msg("  Actual Size: %dx%d", desc.Width, desc.Height);
		}
	}

	Msg("--- Possible Solutions ---");
	Msg("1. Verify texture format is supported by GPU");
	Msg("2. Check texture dimensions (max %dx%d)", caps.MaxTextureWidth, caps.MaxTextureHeight);
	Msg("3. Try converting to DXT1/DXT5 format");
	Msg("4. Check mipmap chain consistency");
}

IRHISurface* CRender::load_texture(LPCSTR fname, u32& msize, bool bStaging /*= false*/)
{
	ID3DBaseTexture* pTexture = this->texture_load(fname, msize);
	IRHISurface* pResult = nullptr;
	if (pTexture) {
		// Create RHITextureDesc for the loaded texture
		RHITextureDesc rhiDesc;
		rhiDesc.Width = 1;  // Will be set properly by the texture
		rhiDesc.Height = 1;
		rhiDesc.Depth = 1;
		rhiDesc.MipLevels = 1;
		rhiDesc.Format = ERHI_FORMAT::B8G8R8A8_UNORM;
		rhiDesc.CPUAccessFlags = 0;
		rhiDesc.MiscFlags = 0;

		// Use GRHI to create the surface from loaded texture
		pResult = GRHI->CreateTextureFromMemory(pTexture, 0, rhiDesc);
	}

	return pResult;
}

ID3DBaseTexture* CRender::texture_load(LPCSTR fRName, u32& ret_msize)
{
	ID3DBaseTexture* pTexture3D = nullptr;
	ID3DTexture2D* pTexture2D = nullptr;

	string_path fn = "";

	u32 img_size = 0;
	int img_loaded_lod = 0;
	u32 mip_cnt = u32(-1);

	// validation
	R_ASSERT(fRName && fRName[0]);

	// make file name
	string_path fname = "";
	xr_strcpy(fname, fRName); //. andy if (strext(fname)) *strext(fname)=0;
	fix_texture_name(fname);
	IReader* S = nullptr;

	if (FS.exist(fn, "$level$", fname, ".dds"))
		goto _DDS;
	if (FS.exist(fn, "$game_saves$", fname, ".dds"))
		goto _DDS;
	if (FS.exist(fn, _game_textures_, fname, ".dds"))
		goto _DDS;
	if (!FS.exist(fn, _game_textures_, fname, ".dds") && strstr(fname, "_bump"))
		goto _BUMP_from_base;
	if (FS.TryLoad(xr_string(fname) + ".dds"))
	{
		xr_string editor_name = xr_string(fname) + ".dds";
		xr_strcpy(fn, editor_name.c_str());
		goto _DDS;
	}
	Msg("! Can't find texture '%s'", fname);

	R_ASSERT(FS.exist(fn, _game_textures_, "ed\\ed_not_existing_texture", ".dds"));
	goto _DDS;

_DDS:
	{
		S = FS.r_open(fn);
		img_size = S->length();
		R_ASSERT(S);

		// Validate DDS file in memory
		const uint8_t* bitData = nullptr;
		size_t bitSize = 0;

		const DDS_HEADER* header = nullptr;
		HRESULT const result = LoadTextureDataFromMemory((uint8_t*)S->pointer(), S->length(), &header, &bitData, &bitSize);
		bool UseRedImage = false;

#ifdef _EDITOR
		RedImageTool::RedImage Image;
		if (result == 0x80070032)
		{
			UseRedImage = true;
			if (!Image.LoadFromMemory(S->pointer(), S->length()))
			{
				UseRedImage = false;
			}

			const size_t ImageBufferSize = Image.GetWidth() * 4 * Image.GetHeight();
			bitData = (uint8_t*)*Image;
		}
#endif
		if (!UseRedImage)
		{
			D3DCAPS9 d3dCaps;
			if (FAILED(RDevice->GetDeviceCaps(&d3dCaps)))
			{
				string512 errMsg;
				xr_sprintf(errMsg, "Failed to get device capabilities for texture size check.");
				R_ASSERT3(false, errMsg, fname);
			}

			const u32 maxTextureDimension = _max(d3dCaps.MaxTextureWidth, d3dCaps.MaxTextureHeight);

			if (header && (header->width > maxTextureDimension || header->height > maxTextureDimension))
			{
				string512 errMsg;
				xr_sprintf(errMsg, "Texture dimensions exceed hardware limits: %dx%d (Max: %d)",
					header->width, header->height,
					maxTextureDimension);
				R_ASSERT3(false, errMsg, fname);
			}
		}

		if (FAILED(result) && !UseRedImage)
		{
			Msg("! Unsupported texture [%s]", fn);
			string1024 errorMsg;
			xr_sprintf
			(
				errorMsg,
				"Failed to get DDS metadata for '%s'\n"
				"File size: %u bytes\n"
				"Error: %s (0x%08X)\n"
				"Possible causes:\n"
				"- Corrupted DDS header\n"
				"- Unsupported DDS variant",
				fname, S->length(),
				Debug.dxerror2string(result), result
			);

			VERIFY2(Device.IsEditorMode(), errorMsg);

			Msg("! DDS METADATA ERROR: %s", errorMsg);
			FS.r_close(S);

			string_path temp;
			R_ASSERT(FS.exist(temp, _game_textures_, "ed\\ed_not_existing_texture", ".dds"));
			R_ASSERT(xr_strcmp(temp, fn));
			xr_strcpy(fn, temp);
			goto _DDS;
		}

		bool is_cubemap = false;
		bool is_volumap = false;

#ifdef _EDITOR
		if (UseRedImage)
		{
			is_cubemap = Image.IsCubeMap();
			is_volumap = false;
		}
		else
#endif
		{
			is_cubemap = (header->caps2 & DDS_CUBEMAP) == DDS_CUBEMAP;
			is_volumap = (header->flags & DDS_HEADER_FLAGS_VOLUME) == DDS_HEADER_FLAGS_VOLUME;
		}

		if (is_cubemap || is_volumap) {
			goto _DDS_CUBE;
		} else {
			goto _DDS_2D;
		}

	_DDS_CUBE:
		{
			HRESULT const result = CreateDDSTextureFromMemoryEx(RDevice,
				(uint8_t*)S->pointer(), S->length(), 0, D3DPOOL_DEFAULT, false, &pTexture3D);

			FS.r_close(S);

			if (FAILED(result))
			{
				PrintTextureError(result, fname, &bitData, bitSize, pTexture3D);

				string_path temp;
				R_ASSERT(FS.exist(temp, _game_textures_, "ed\\ed_not_existing_texture", ".dds"));
				R_ASSERT(xr_strcmp(temp, fn));
				xr_strcpy(fn, temp);
				goto _DDS;
			}

			mip_cnt = pTexture3D->GetLevelCount();

			ret_msize = calc_texture_size(img_loaded_lod, mip_cnt, img_size);
			return pTexture3D;
		}
	_DDS_2D:
		{
			strlwr(fn);
			ID3DTexture2D* T_sysmem = nullptr;
			HRESULT TextureApplyResult = S_OK;
#ifdef _EDITOR
			if (UseRedImage)
			{
				Image.Convert(RedImageTool::RedTexturePixelFormat::R8G8B8A8);

				TextureApplyResult = RDevice->CreateTexture
				(
					Image.GetWidth(), Image.GetHeight(),
					Image.GetMips(), 0, D3DFMT_A8R8G8B8, D3DPOOL_SYSTEMMEM, &T_sysmem, nullptr
				);

				if (SUCCEEDED(TextureApplyResult) && T_sysmem)
				{
					for (u32 level = 0; level < Image.GetMips(); ++level)
					{
						D3DLOCKED_RECT rect;
						if (SUCCEEDED(T_sysmem->LockRect(level, &rect, nullptr, 0)))
						{
							const u32 w = std::max((size_t)1, Image.GetWidth() >> level);
							const u32 h = std::max((size_t)1, Image.GetHeight() >> level);

							const u8* src = (const u8*)*Image;

							size_t mipOffset = 0;
							for (u32 l = 0; l < level; ++l)
							{
								mipOffset += (std::max((size_t)1, Image.GetWidth() >> l)) * (std::max((size_t)1, Image.GetHeight() >> l)) * 4;
							}

							src += mipOffset;

							for (u32 y = 0; y < h; ++y)
							{
								u8* rowDst = (u8*)rect.pBits + y * rect.Pitch;
								const u8* rowSrc = src + y * w * 4;

								for (u32 x = 0; x < w; ++x)
								{
									rowDst[x * 4 + 0] = rowSrc[x * 4 + 2]; // B
									rowDst[x * 4 + 1] = rowSrc[x * 4 + 1]; // G
									rowDst[x * 4 + 2] = rowSrc[x * 4 + 0]; // R
									rowDst[x * 4 + 3] = rowSrc[x * 4 + 3]; // A
								}
							}
							T_sysmem->UnlockRect(level);
						}
					}
				}
			}
			else
#endif
			{
				TextureApplyResult = CreateDDSTextureFromMemoryEx
				(
					RDevice,
					(u8*)S->pointer(),
					S->length(),
					0,
					D3DPOOL_SYSTEMMEM,
					false,
					&T_sysmem
				);
			}

			FS.r_close(S);

			if (T_sysmem != nullptr)
			{
				img_loaded_lod = get_texture_load_lod(fn);
				pTexture2D = TW_LoadTextureFromTexture(T_sysmem, img_loaded_lod);
				mip_cnt = pTexture2D->GetLevelCount();
			}

			if (FAILED(TextureApplyResult) || T_sysmem == nullptr)
			{
				PrintTextureError(TextureApplyResult, fname, bitData, bitSize, pTexture2D, T_sysmem != nullptr);

				string_path temp;
				R_ASSERT(FS.exist(temp, _game_textures_, "ed\\ed_not_existing_texture", ".dds"));
				R_ASSERT(xr_strcmp(temp, fn));
				xr_strcpy(fn, temp);
				goto _DDS;
			}
			_RELEASE(T_sysmem);

			ret_msize = calc_texture_size(img_loaded_lod, mip_cnt, img_size);
			return pTexture2D;
		}
	}
_BUMP_from_base:
	{
		//Msg("! Fallback to default bump map: %s", fname);
		if (strstr(fname, "_bump#"))
		{
			R_ASSERT2(FS.exist(fn, _game_textures_, "ed\\ed_dummy_bump#", ".dds"), "ed_dummy_bump#");
			S = FS.r_open(fn);
			R_ASSERT2(S, fn);
			img_size = S->length();
			goto _DDS;
		}

		Msg("! Fallback to default bump map: %s", fname);
		if (strstr(fname, "_bump"))
		{
			R_ASSERT2(FS.exist(fn, _game_textures_, "ed\\ed_dummy_bump", ".dds"), "ed_dummy_bump");
			S = FS.r_open(fn);
			R_ASSERT2(S, fn);
			img_size = S->length();
			goto _DDS;
		}
		if (S)
			FS.r_close(S);

		return nullptr;
	}
}
