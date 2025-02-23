#include "stdafx.h"

#include <memory>
#include <DirectXTex.h>
#include <magic_enum/magic_enum.hpp>

using namespace DirectX;

void fix_texture_name(LPSTR fn)
{
	auto _ext = strext(fn);
	if (_ext && (0 == _stricmp(_ext, ".tga") ||
				 0 == _stricmp(_ext, ".dds") ||
				 0 == _stricmp(_ext, ".bmp") ||
				 0 == _stricmp(_ext, ".ogm")))
	{
		*_ext = 0;
	}
}

int get_texture_load_lod(LPCSTR fn)
{
	auto& sect = pSettings->r_section("reduce_lod_texture_list");

	for (const auto& data : sect.Data)
	{
		if (strstr(fn, data.first.c_str()))
		{
			if (psTextureLOD < 1)
			{
				return 0;
			}
			else
			{
				if (psTextureLOD < 3)
				{
					return 1;
				}
				else
				{
					return 2;
				}
			}
		}
	}

	if (psTextureLOD < 2)
	{
		return 0;
	}
	else
	{
		if (psTextureLOD < 4)
		{
			return 1;
		}
		else
		{
			return 2;
		}
	}
}

u32 calc_texture_size(int lod, u32 mip_cnt, u32 orig_size)
{
	if (1 == mip_cnt)
	{
		return orig_size;
	}
	int _lod = lod;
	float res = float(orig_size);

	while (_lod > 0)
	{
		--_lod;
		res -= res / 1.333f;
	}
	return iFloor(res);
}

IC void	Reduce(size_t& w, size_t& h, size_t& l, int& skip)
{
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

IC void PrintLoadTextureError(HRESULT hr, TexMetadata& imageInfo, const char* fname, int img_loaded_lod, D3D11_USAGE usage)
{
	auto isPowerOfTwo = [](int x)
		{
			return (x && !(x & (x - 1)));
		};

	const char* sizeIssue = (!isPowerOfTwo(imageInfo.height) || !isPowerOfTwo(imageInfo.width))
		? "Non-power-of-2 texture dimensions"
		: "Unknown creation error";

	string1024 errorDetails;
	xr_sprintf(errorDetails,
		"Failed to create 2D texture: %s\n"
		"                         Error: %s"
		"                         Error Code: (0x%08X)\n"
		"                         Dimensions: %dx%d\n"
		"                         Mip levels: %d (loaded LOD: %d)\n"
		"                         Format: %s\n"
		"                         Memory usage: %s\n"
		"                         Issue: %s",
		fname,
		Debug.dxerror2string(hr), hr,
		imageInfo.width,
		imageInfo.height,
		imageInfo.mipLevels,
		img_loaded_lod,
		magic_enum::enum_name(imageInfo.format).data(),
		(usage == D3D_USAGE_STAGING) ? "STAGING" : "DEFAULT",
		sizeIssue
	);

	R_ASSERT3(false, errorDetails, "Texture creation failed");
	Msg("! TEXTURE CREATION ERROR: %s", errorDetails);
}

ID3DBaseTexture* CRender::texture_load(LPCSTR fRName, u32& ret_msize, bool bStaging)
{
	// Moved here just to avoid warning
	TexMetadata imageInfo{};

	// Staging control
	static bool bAllowStaging = !RImplementation.o.no_ram_textures;
	bStaging &= bAllowStaging;

	DDS_FLAGS textureFlag = DDS_FLAGS::DDS_FLAGS_NONE;
	ID3DBaseTexture* pTexture2D = nullptr;
	ScratchImage scratchImage = {};
	string_path fn = {};
	u32 img_size = 0;
	int img_loaded_lod = 0;
	u32 mip_cnt = u32(-1);
	// validation
	R_ASSERT(fRName || fRName[0]);

	bool FileExist = false;
	HRESULT hr;

	D3D11_USAGE usage = (bStaging) ? D3D_USAGE_STAGING : D3D_USAGE_DEFAULT;
	int bindFlags = (bStaging) ? 0 : D3D_BIND_SHADER_RESOURCE;
	int cpuAccessFlags = (bStaging) ? D3D_CPU_ACCESS_WRITE : 0;
	u32 miscFlags = imageInfo.miscFlags;

	// make file name
	string_path fname = {};
	xr_strcpy(fname, fRName);
	fix_texture_name(fname);
	IReader* reader = nullptr;
	if (!FS.exist(fn, "$game_textures$", fname, ".dds") && strstr(fname, "_bump"))
	{
		goto _BUMP_from_base;
	}
	if (FS.exist(fn, "$level$", fname, ".dds"))
	{
		goto _DDS;
	}
	if (FS.exist(fn, "$game_saves$", fname, ".dds"))
	{
		goto _DDS;
	}
	if (FS.exist(fn, "$game_textures$", fname, ".dds"))
	{
		goto _DDS;
	}

	Msg("! Can't find texture '%s'", fname);
	FileExist = FS.exist(fn, "$game_textures$", "ed\\ed_not_existing_texture", ".dds") != nullptr;
	R_ASSERT2(FileExist, "File not found: ed\\ed_not_existing_texture.dds");
	goto _DDS;

	_DDS: {
		// Load and get header
		reader = FS.r_open(fn);
#ifdef DEBUG
		Msg("* Loaded: %s[%d]b", fn, reader->length());
#endif // DEBUG
		img_size = reader->length();
		R_ASSERT(reader);
		hr = GetMetadataFromDDSMemory(reader->pointer(), reader->length(), textureFlag, imageInfo);

		if (imageInfo.width > D3D11_REQ_TEXTURE2D_U_OR_V_DIMENSION ||
			imageInfo.height > D3D11_REQ_TEXTURE2D_U_OR_V_DIMENSION)
		{
			string512 errMsg;
			xr_sprintf(errMsg, "Texture dimensions exceed hardware limits: %dx%d (Max: %d)",
				imageInfo.width, imageInfo.height,
				D3D11_REQ_TEXTURE2D_U_OR_V_DIMENSION);
			R_ASSERT3(false, errMsg, fname);
		}

		if (FAILED(hr))
		{
			string1024 errorMsg;
			xr_sprintf(errorMsg, "Failed to get DDS metadata for '%s'\n"
				"File size: %u bytes\n"
				"Error: %s (0x%08X)\n"
				"Possible causes:\n"
				"- Corrupted DDS header\n"
				"- Unsupported DDS variant", 
				fname, reader->length(),
				Debug.dxerror2string(hr), hr);

			VERIFY2(false, errorMsg);
			Msg("! DDS METADATA ERROR: %s", errorMsg);
			FS.r_close(reader);
			return nullptr;
		}

		{
			UINT flags = 0;
			UINT test_flags = D3D11_FORMAT_SUPPORT_SHADER_LOAD | D3D11_FORMAT_SUPPORT_SHADER_SAMPLE;
			RDevice->CheckFormatSupport(imageInfo.format, &flags);

			if (test_flags != (flags & test_flags))
			{
				string512 errorMsg;
				xr_sprintf(errorMsg, "Unsupported texture format for '%s'\n"
					"                       Format: %s (%d)\n"
					"                       Required flags: 0x%08X\n"
					"                       Supported flags: 0x%08X\n"
					"                       Attempting fallback to 16BPP",
					fname,
					magic_enum::enum_name(imageInfo.format).data(),
					imageInfo.format,
					test_flags,
					flags);

				Msg("! TEXTURE FORMAT ERROR: %s", errorMsg);
				textureFlag = DDS_FLAGS::DDS_FLAGS_NO_16BPP;
				R_ASSERT3(false, errorMsg, fname);
			}
		}

		hr = LoadFromDDSMemory(reader->pointer(), reader->length(), textureFlag, &imageInfo, scratchImage);
		FS.r_close(reader);

		if (FAILED(hr))
		{
			const char* formatName = magic_enum::enum_name(imageInfo.format).data();
			string1024 errorMsg;
			xr_sprintf(errorMsg, sizeof(errorMsg),
				"Failed to load texture '%s'\n"
				"                         Error: %s"
				"                         Error Code: (0x%08X)\n"
				"                         Format: %s\n"
				"                         Dimensions: %dx%d\n"
				"                         Mip-levels: %d\n"
				"                         Suggested solutions:\n"
				"                         1. Check texture conversion settings\n"
				"                         2. Verify GPU format support\n"
				"                         3. Use DirectX TexTool for inspection",
				fname,
				Debug.dxerror2string(hr), hr, formatName,
				imageInfo.width, imageInfo.height,
				imageInfo.mipLevels);

			R_ASSERT3(false, errorMsg, "Texture loading failed");
			Msg("! TEXTURE ERROR: %s", errorMsg);

			return nullptr;
		}

		if (imageInfo.IsCubemap() || imageInfo.IsVolumemap())
		{
			goto _DDS_CUBE;
		}
		else
		{
			goto _DDS_2D;
		}
	_DDS_CUBE:
		{
		hr = CreateTextureEx(RDevice, scratchImage.GetImages(), scratchImage.GetImageCount(),
			imageInfo, usage, bindFlags, cpuAccessFlags, miscFlags, CREATETEX_FLAGS::CREATETEX_DEFAULT, &pTexture2D);
		scratchImage.Release();

		if (FAILED(hr) || pTexture2D == nullptr)
		{
			PrintLoadTextureError(hr, imageInfo, fname, img_loaded_lod, usage);
		}

		mip_cnt = (int)imageInfo.mipLevels;
		ret_msize = calc_texture_size(img_loaded_lod, mip_cnt, img_size);
		return pTexture2D;
		}
	_DDS_2D: {
		// Check for LMAP and compress if needed
		_strlwr(fn);

		img_loaded_lod = get_texture_load_lod(fn);

		int old_mipmap_cnt = 0, mip_lod = 0;

		if (img_loaded_lod)
		{
			old_mipmap_cnt = (int)imageInfo.mipLevels;
			Reduce(imageInfo.width, imageInfo.height, imageInfo.mipLevels, img_loaded_lod);
			mip_lod = old_mipmap_cnt - (int)imageInfo.mipLevels;
		}

		hr = CreateTextureEx(RDevice, scratchImage.GetImages() + mip_lod, scratchImage.GetImageCount(),
			imageInfo, usage, bindFlags, cpuAccessFlags, miscFlags, CREATETEX_FLAGS::CREATETEX_DEFAULT, &pTexture2D);
		scratchImage.Release();
		
		if (FAILED(hr) || pTexture2D == nullptr)
		{
			PrintLoadTextureError(hr, imageInfo, fname, img_loaded_lod, usage);
		}

		mip_cnt = (int)imageInfo.mipLevels;
		ret_msize = calc_texture_size(img_loaded_lod, mip_cnt, img_size);
		return pTexture2D;
	}
}
	_BUMP_from_base: {
		Msg("! Fallback to default bump map: %s", fname);
		const char* bumpType = strstr(fname, "_bump#") ? "ed_dummy_bump#" : "ed_dummy_bump";
		bool FileExist = FS.exist(fn, "$game_textures$", bumpType, ".dds");

		if (!FileExist)
		{
			string512 errorMsg;
			xr_sprintf(errorMsg, "Fallback bump texture missing!\n"
				"Required file: %s.dds\n"
				"Original texture: %s",
				bumpType, fname);

			R_ASSERT2(FileExist, errorMsg);
			Msg("! CRITICAL TEXTURE ERROR: %s", errorMsg);
			return nullptr;
		}
	}

	return nullptr;
}
