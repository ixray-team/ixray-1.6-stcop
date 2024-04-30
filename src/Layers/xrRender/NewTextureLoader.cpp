#include "stdafx.h"

#include <memory>
#include <DirectXTex.h>

using namespace DirectX;

void fix_texture_name(LPSTR fn) {
    auto _ext = strext(fn);
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
    if (1 == mip_cnt) {
        return orig_size;
    }
    int _lod = lod;
    float res = float(orig_size);

    while (_lod > 0) {
        --_lod;
        res -= res / 1.333f;
    }
    return iFloor(res);
}

IC void	Reduce(size_t& w, size_t& h, size_t& l, int& skip) {
    while ((l > 1) && skip) {
        w /= 2;
        h /= 2;
        l -= 1;

        skip--;
    }
    if (w < 1) {
        w = 1;
    }
    if (h < 1) {
        h = 1;
    }
}

bool RHICreateTextureEx(
    const Image* srcImages,
    size_t nimages,
    const TexMetadata& metadata,
    ERHIUsage usage,
    unsigned int bindFlags,
    unsigned int cpuAccessFlags,
    unsigned int miscFlags,
    CREATETEX_FLAGS flags,
    LPIRHITEXTURE* ppResource) noexcept
{
    if (!srcImages || !nimages || !ppResource)
        return false;

    *ppResource = nullptr;

    if (!metadata.mipLevels || !metadata.arraySize)
        return false;

    if ((metadata.width > UINT32_MAX) || (metadata.height > UINT32_MAX)
        || (metadata.mipLevels > UINT16_MAX) || (metadata.arraySize > UINT16_MAX))
        return false;

    std::unique_ptr<SUBRESOURCE_DATA[]> initData(new SUBRESOURCE_DATA[metadata.mipLevels * metadata.arraySize]);
    if (!initData)
        return false;

    // Fill out subresource array
    if (metadata.IsVolumemap())
    {
        //--- Volume case -------------------------------------------------------------
        if (!metadata.depth)
            return E_INVALIDARG;

        if (metadata.depth > UINT16_MAX)
            return E_INVALIDARG;

        if (metadata.arraySize > 1)
            // Direct3D 11 doesn't support arrays of 3D textures
            return false;

        size_t depth = metadata.depth;

        size_t idx = 0;
        for (size_t level = 0; level < metadata.mipLevels; ++level)
        {
            const size_t index = metadata.ComputeIndex(level, 0, 0);
            if (index >= nimages)
                return E_FAIL;

            const Image& img = srcImages[index];

            if (img.format != metadata.format)
                return E_FAIL;

            if (!img.pixels)
                return E_POINTER;

            // Verify pixels in image 1 .. (depth-1) are exactly image->slicePitch apart
            // For 3D textures, this relies on all slices of the same miplevel being continous in memory
            // (this is how ScratchImage lays them out), which is why we just give the 0th slice to Direct3D 11
            const uint8_t* pSlice = img.pixels + img.slicePitch;
            for (size_t slice = 1; slice < depth; ++slice)
            {
                const size_t tindex = metadata.ComputeIndex(level, 0, slice);
                if (tindex >= nimages)
                    return E_FAIL;

                const Image& timg = srcImages[tindex];

                if (!timg.pixels)
                    return E_POINTER;

                if (timg.pixels != pSlice
                    || timg.format != metadata.format
                    || timg.rowPitch != img.rowPitch
                    || timg.slicePitch != img.slicePitch)
                    return E_FAIL;

                pSlice = timg.pixels + img.slicePitch;
            }

            assert(idx < (metadata.mipLevels * metadata.arraySize));

            initData[idx].pSysMem = img.pixels;
            initData[idx].SysMemPitch = static_cast<DWORD>(img.rowPitch);
            initData[idx].SysMemSlicePitch = static_cast<DWORD>(img.slicePitch);
            ++idx;

            if (depth > 1)
                depth >>= 1;
        }
    }
    else
    {
        //--- 1D or 2D texture case ---------------------------------------------------
        size_t idx = 0;
        for (size_t item = 0; item < metadata.arraySize; ++item)
        {
            for (size_t level = 0; level < metadata.mipLevels; ++level)
            {
                const size_t index = metadata.ComputeIndex(level, item, 0);
                if (index >= nimages)
                    return false;

                const Image& img = srcImages[index];

                if (img.format != metadata.format)
                    return false;

                if (!img.pixels)
                    return false;

                assert(idx < (metadata.mipLevels * metadata.arraySize));

                initData[idx].pSysMem = img.pixels;
                initData[idx].SysMemPitch = static_cast<DWORD>(img.rowPitch);
                initData[idx].SysMemSlicePitch = static_cast<DWORD>(img.slicePitch);
                ++idx;
            }
        }
    }

    // Create texture using static initialization data
    bool hr = false;

    DXGI_FORMAT format = metadata.format;
    if (flags & CREATETEX_FORCE_SRGB)
    {
        format = MakeSRGB(format);
    }
    else if (flags & CREATETEX_IGNORE_SRGB)
    {
        format = MakeLinear(format);
    }

    switch (metadata.dimension)
    {
    //case TEX_DIMENSION_TEXTURE1D:
    //{
    //    D3D11_TEXTURE1D_DESC desc = {};
    //    desc.Width = static_cast<UINT>(metadata.width);
    //    desc.MipLevels = static_cast<UINT>(metadata.mipLevels);
    //    desc.ArraySize = static_cast<UINT>(metadata.arraySize);
    //    desc.Format = format;
    //    desc.Usage = usage;
    //    desc.BindFlags = bindFlags;
    //    desc.CPUAccessFlags = cpuAccessFlags;
    //    desc.MiscFlags = miscFlags & ~static_cast<uint32_t>(D3D11_RESOURCE_MISC_TEXTURECUBE);

    //    hr = pDevice->CreateTexture1D(&desc, initData.get(), reinterpret_cast<ID3D11Texture1D**>(ppResource));
    //}
    //break;

    case TEX_DIMENSION_TEXTURE2D:
    {
        TextureDesc desc = {};
        desc.Width = static_cast<UINT>(metadata.width);
        desc.Height = static_cast<UINT>(metadata.height);
        desc.NumMips = static_cast<UINT>(metadata.mipLevels);
        desc.DepthOrSliceNum = static_cast<UINT>(metadata.arraySize);
        desc.IsCube = metadata.IsCubemap();
        desc.Format = g_RenderRHI->GetRHIFormatFromAPI(format);
        *ppResource = g_RenderRHI->CreateAPITexture(&desc, initData.get());
    }
    break;

    //case TEX_DIMENSION_TEXTURE3D:
    //{
    //    D3D11_TEXTURE3D_DESC desc = {};
    //    desc.Width = static_cast<UINT>(metadata.width);
    //    desc.Height = static_cast<UINT>(metadata.height);
    //    desc.Depth = static_cast<UINT>(metadata.depth);
    //    desc.MipLevels = static_cast<UINT>(metadata.mipLevels);
    //    desc.Format = format;
    //    desc.Usage = usage;
    //    desc.BindFlags = bindFlags;
    //    desc.CPUAccessFlags = cpuAccessFlags;
    //    desc.MiscFlags = miscFlags & ~static_cast<uint32_t>(D3D11_RESOURCE_MISC_TEXTURECUBE);

    //    hr = pDevice->CreateTexture3D(&desc, initData.get(), reinterpret_cast<ID3D11Texture3D**>(ppResource));
    //}
    //break;

    default:
        return false;
    }

    return hr;
}

IRHITexture* CRender::texture_load(LPCSTR fRName, u32& ret_msize
#ifdef USE_DX11
    , bool bStaging
#endif // USE_DX11
)
{
    // Moved here just to avoid warning
    TexMetadata imageInfo{};

    DDS_FLAGS textureFlag = DDS_FLAGS::DDS_FLAGS_NONE;
    IRHITexture* pTexture2D = nullptr;
    string_path fn;
    u32 img_size = 0;
    int img_loaded_lod = 0;
    u32 mip_cnt = u32(-1);
    // validation
    R_ASSERT(fRName);
    R_ASSERT(fRName[0]);

    bool FileExist = false;

    // make file name
    string_path fname;
    xr_strcpy(fname, fRName);
    fix_texture_name(fname);
    IReader* reader = nullptr;
    if (!FS.exist(fn, "$game_textures$", fname, ".dds") && strstr(fname, "_bump")) {
        goto _BUMP_from_base;
    }
    if (FS.exist(fn, "$level$", fname, ".dds")) {
        goto _DDS;
    }
    if (FS.exist(fn, "$game_saves$", fname, ".dds")) {
        goto _DDS;
    }
    if (FS.exist(fn, "$game_textures$", fname, ".dds")) {
        goto _DDS;
    }

#ifdef _EDITOR
    ELog.Msg(mtError, "Can't find texture '%s'", fname);
    return 0;
#else

    Msg("! Can't find texture '%s'", fname);
    FileExist = FS.exist(fn, "$game_textures$", "ed\\ed_not_existing_texture", ".dds") != nullptr;
    R_ASSERT2(FileExist, "File not found: ed\\ed_not_existing_texture.dds");
    goto _DDS;

#endif

    _DDS: {
        // Load and get header
        reader = FS.r_open(fn);
#ifdef DEBUG
        Msg("* Loaded: %s[%d]b", fn, reader->length());
#endif // DEBUG
        img_size = reader->length();
        R_ASSERT(reader);
        R_CHK2(GetMetadataFromDDSMemory(reader->pointer(), reader->length(), textureFlag, imageInfo), fn);

        if (imageInfo.IsCubemap() || imageInfo.IsVolumemap()) {
            goto _DDS_CUBE;
        } else {
            goto _DDS_2D;
        }
    _DDS_CUBE: {
        auto scratchImage = std::make_unique<ScratchImage>();
        HRESULT hr = LoadFromDDSMemory(reader->pointer(), reader->length(), textureFlag, &imageInfo, *scratchImage);
        auto usage = eUsageStatic;
        auto bindFlags = 0; // (bStaging) ? 0 : D3D_BIND_SHADER_RESOURCE;
        auto cpuAccessFlags = 0; //(bStaging) ? D3D_CPU_ACCESS_WRITE : 0;
        auto miscFlags = imageInfo.miscFlags;
        
        hr = RHICreateTextureEx(scratchImage->GetImages(), scratchImage->GetImageCount(),
            imageInfo, usage, bindFlags, cpuAccessFlags, miscFlags, CREATETEX_FLAGS::CREATETEX_DEFAULT, &pTexture2D);

        FS.r_close(reader);
        mip_cnt = (int)imageInfo.mipLevels;
        ret_msize = calc_texture_size(img_loaded_lod, mip_cnt, img_size);
        return pTexture2D;
        }
    _DDS_2D: {
        // Check for LMAP and compress if needed
        _strlwr(fn);

        img_loaded_lod = get_texture_load_lod(fn);

        auto scratchImage = std::make_unique<ScratchImage>();
        HRESULT hr = LoadFromDDSMemory(reader->pointer(), reader->length(), textureFlag, &imageInfo, *scratchImage);
        
        auto usage = eUsageStatic;
        auto bindFlags = 0; //(bStaging) ? 0 : D3D_BIND_SHADER_RESOURCE;
        auto cpuAccessFlags = 0;// (bStaging) ? D3D_CPU_ACCESS_WRITE : 0;
        auto miscFlags = imageInfo.miscFlags;
        int old_mipmap_cnt = 0, mip_lod = 0;
        if (img_loaded_lod) {
            old_mipmap_cnt = (int)imageInfo.mipLevels;
            Reduce(imageInfo.width, imageInfo.height, imageInfo.mipLevels, img_loaded_lod);
            mip_lod = old_mipmap_cnt - (int)imageInfo.mipLevels;
        }

        bool res = RHICreateTextureEx(scratchImage->GetImages() + mip_lod, scratchImage->GetImageCount(),
            imageInfo, usage, bindFlags, cpuAccessFlags, miscFlags, CREATETEX_FLAGS::CREATETEX_DEFAULT, &pTexture2D);
        FS.r_close(reader);
        
        mip_cnt = (int)imageInfo.mipLevels;
        ret_msize = calc_texture_size(img_loaded_lod, mip_cnt, img_size);
        return pTexture2D;
    }
}
    _BUMP_from_base: {
        Msg("! Fallback to default bump map: %s", fname);
        if (strstr(fname, "_bump#")) 
        {
            bool FileExist = FS.exist(fn, "$game_textures$", "ed\\ed_dummy_bump#", ".dds") != nullptr;
            R_ASSERT2(FileExist, "File not found: ed\\ed_dummy_bump#.dds");

            reader = FS.r_open(fn);
            R_ASSERT2(reader, fn);
            img_size = reader->length();
            goto _DDS_2D;
        }
        if (strstr(fname, "_bump")) 
        {
            bool FileExist = FS.exist(fn, "$game_textures$", "ed\\ed_dummy_bump", ".dds") != nullptr;
            R_ASSERT2(FileExist, "File not found: ed\\ed_dummy_bump.dds");

            reader = FS.r_open(fn);
            R_ASSERT2(reader, fn);
            img_size = reader->length();
            goto _DDS_2D;
        }
    }

    return nullptr;
}