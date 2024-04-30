// Texture.cpp: implementation of the CTexture class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#pragma hdrstop

#include "DDSTextureLoader9.h"
using namespace DirectX;

#ifndef _EDITOR
#include "../xrRender/dxRenderDeviceRender.h"
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
    if (FS.exist(fn, "$game_textures$", fname, ".dds"))
        goto _DDS;
    if (!FS.exist(fn, "$game_textures$", fname, ".dds") && strstr(fname, "_bump"))
        goto _BUMP_from_base;

    Msg("! Can't find texture '%s'", fname);

#ifdef _EDITOR
    return 0;
#else

    R_ASSERT(FS.exist(fn, "$game_textures$", "ed\\ed_not_existing_texture", ".dds"));
    goto _DDS;

#endif

_DDS:
    {
        S = FS.r_open(fn);
#ifdef DEBUG
        Msg("* Loaded: %s[%d]b", fn, S->length());
#endif // DEBUG
        img_size = S->length();
        R_ASSERT(S);

        // Validate DDS file in memory
        const DDS_HEADER* header = nullptr;
        const uint8_t* bitData = nullptr;
        size_t bitSize = 0;

        HRESULT const result = LoadTextureDataFromMemory((uint8_t*)S->pointer(), S->length(), &header, &bitData, &bitSize);
        if (FAILED(result)) {
            Msg("! Unsupported texture [%s]", fn);
            FS.r_close(S);

            string_path temp = "";
            R_ASSERT(FS.exist(temp, "$game_textures$", "ed\\ed_not_existing_texture", ".dds"));
            R_ASSERT(xr_strcmp(temp, fn));
            xr_strcpy(fn, temp);

            goto _DDS;
        }

        bool is_cubemap = (header->caps2 & DDS_CUBEMAP) == DDS_CUBEMAP;
        bool is_volumap = (header->flags & DDS_HEADER_FLAGS_VOLUME) == DDS_HEADER_FLAGS_VOLUME;

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

            if (FAILED(result)) {
                Msg("! Can't load texture '%s'", fn);
                string_path temp;
                R_ASSERT(FS.exist(temp, "$game_textures$", "ed\\ed_not_existing_texture", ".dds"));
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
            HRESULT const result = CreateDDSTextureFromMemoryEx(RDevice,
                (uint8_t*)S->pointer(), S->length(), 0, D3DPOOL_SYSTEMMEM, false, &T_sysmem);

            FS.r_close(S);

            if (FAILED(result)) {
                Msg("! Can't load texture '%s'", fn);
                string_path temp = "";
                R_ASSERT(FS.exist(temp, "$game_textures$", "ed\\ed_not_existing_texture", ".dds"));
                strlwr(temp);
                R_ASSERT(xr_strcmp(temp, fn));
                xr_strcpy(fn, temp);
                goto _DDS;
            }

            img_loaded_lod = get_texture_load_lod(fn);
            pTexture2D = TW_LoadTextureFromTexture(T_sysmem, img_loaded_lod);
            mip_cnt = pTexture2D->GetLevelCount();
            _RELEASE(T_sysmem);

            ret_msize = calc_texture_size(img_loaded_lod, mip_cnt, img_size);
            return pTexture2D;
        }
    }
_BUMP_from_base:
    {
        Msg("! auto-generated bump map: %s", fname);
#ifndef _EDITOR
        if (strstr(fname, "_bump#"))
        {
            R_ASSERT2(FS.exist(fn, "$game_textures$", "ed\\ed_dummy_bump#", ".dds"), "ed_dummy_bump#");
            S = FS.r_open(fn);
            R_ASSERT2(S, fn);
            img_size = S->length();
            goto _DDS;
        }
        if (strstr(fname, "_bump"))
        {
            R_ASSERT2(FS.exist(fn, "$game_textures$", "ed\\ed_dummy_bump", ".dds"), "ed_dummy_bump");
            S = FS.r_open(fn);

            R_ASSERT2(S, fn);

            img_size = S->length();
            goto _DDS;
        }
#endif
        if (S)
            FS.r_close(S);

        return nullptr;
    }
}

#if 0

#define ISBITMASK( r,g,b,a ) ( ddpf.RBitMask == r && ddpf.GBitMask == g && ddpf.BBitMask == b && ddpf.ABitMask == a )

D3DFORMAT GetD3D9FormatEx(const DDS_PIXELFORMAT& ddpf) noexcept
{
    if (ddpf.flags & DDS_RGB)
    {
        switch (ddpf.RGBBitCount)
        {
        case 32:
            if (ISBITMASK(0x00ff0000, 0x0000ff00, 0x000000ff, 0xff000000))
            {
                return D3DFMT_A8R8G8B8;
            }
            if (ISBITMASK(0x00ff0000, 0x0000ff00, 0x000000ff, 0))
            {
                return D3DFMT_X8R8G8B8;
            }
            if (ISBITMASK(0x000000ff, 0x0000ff00, 0x00ff0000, 0xff000000))
            {
                return D3DFMT_A8B8G8R8;
            }
            if (ISBITMASK(0x000000ff, 0x0000ff00, 0x00ff0000, 0))
            {
                return D3DFMT_X8B8G8R8;
            }

            // Note that many common DDS reader/writers (including D3DX) swap the
            // the RED/BLUE masks for 10:10:10:2 formats. We assume
            // below that the 'backwards' header mask is being used since it is most
            // likely written by D3DX.

            // For 'correct' writers this should be 0x3ff00000,0x000ffc00,0x000003ff for BGR data
            if (ISBITMASK(0x000003ff, 0x000ffc00, 0x3ff00000, 0xc0000000))
            {
                return D3DFMT_A2R10G10B10;
            }

            // For 'correct' writers this should be 0x000003ff,0x000ffc00,0x3ff00000 for RGB data
            if (ISBITMASK(0x3ff00000, 0x000ffc00, 0x000003ff, 0xc0000000))
            {
                return D3DFMT_A2B10G10R10;
            }

            if (ISBITMASK(0x0000ffff, 0xffff0000, 0x00000000, 0x00000000))
            {
                return D3DFMT_G16R16;
            }
            if (ISBITMASK(0xffffffff, 0x00000000, 0x00000000, 0x00000000))
            {
                return D3DFMT_R32F; // D3DX writes this out as a FourCC of 114
            }
            break;

        case 24:
            if (ISBITMASK(0xff0000, 0x00ff00, 0x0000ff, 0))
            {
                return D3DFMT_R8G8B8;
            }
            break;

        case 16:
            if (ISBITMASK(0xf800, 0x07e0, 0x001f, 0x0000))
            {
                return D3DFMT_R5G6B5;
            }
            if (ISBITMASK(0x7c00, 0x03e0, 0x001f, 0x8000))
            {
                return D3DFMT_A1R5G5B5;
            }
            if (ISBITMASK(0x7c00, 0x03e0, 0x001f, 0))
            {
                return D3DFMT_X1R5G5B5;
            }
            if (ISBITMASK(0x0f00, 0x00f0, 0x000f, 0xf000))
            {
                return D3DFMT_A4R4G4B4;
            }
            if (ISBITMASK(0x0f00, 0x00f0, 0x000f, 0))
            {
                return D3DFMT_X4R4G4B4;
            }
            if (ISBITMASK(0x00e0, 0x001c, 0x0003, 0xff00))
            {
                return D3DFMT_A8R3G3B2;
            }

            // NVTT versions 1.x wrote these as RGB instead of LUMINANCE
            if (ISBITMASK(0xffff, 0, 0, 0))
            {
                return D3DFMT_L16;
            }
            if (ISBITMASK(0x00ff, 0, 0, 0xff00))
            {
                return D3DFMT_A8L8;
            }
            break;

        case 8:
            if (ISBITMASK(0xe0, 0x1c, 0x03, 0))
            {
                return D3DFMT_R3G3B2;
            }

            // NVTT versions 1.x wrote these as RGB instead of LUMINANCE
            if (ISBITMASK(0xff, 0, 0, 0))
            {
                return D3DFMT_L8;
            }

            // Paletted texture formats are typically not supported on modern video cards aka D3DFMT_P8, D3DFMT_A8P8
            break;
        }
    }
    else if (ddpf.flags & DDS_LUMINANCE)
    {
        switch (ddpf.RGBBitCount)
        {
        case 16:
            if (ISBITMASK(0xffff, 0, 0, 0))
            {
                return D3DFMT_L16;
            }
            if (ISBITMASK(0x00ff, 0, 0, 0xff00))
            {
                return D3DFMT_A8L8;
            }
            break;

        case 8:
            if (ISBITMASK(0x0f, 0, 0, 0xf0))
            {
                return D3DFMT_A4L4;
            }
            if (ISBITMASK(0xff, 0, 0, 0))
            {
                return D3DFMT_L8;
            }
            if (ISBITMASK(0x00ff, 0, 0, 0xff00))
            {
                return D3DFMT_A8L8; // Some DDS writers assume the bitcount should be 8 instead of 16
            }
            break;
        }
    }
    else if (ddpf.flags & DDS_ALPHA)
    {
        if (8 == ddpf.RGBBitCount)
        {
            return D3DFMT_A8;
        }
    }
    else if (ddpf.flags & DDS_BUMPDUDV)
    {
        switch (ddpf.RGBBitCount)
        {
        case 32:
            if (ISBITMASK(0x000000ff, 0x0000ff00, 0x00ff0000, 0xff000000))
            {
                return D3DFMT_Q8W8V8U8;
            }
            if (ISBITMASK(0x0000ffff, 0xffff0000, 0x00000000, 0x00000000))
            {
                return D3DFMT_V16U16;
            }
            if (ISBITMASK(0x3ff00000, 0x000ffc00, 0x000003ff, 0xc0000000))
            {
                return D3DFMT_A2W10V10U10;
            }
            break;

        case 16:
            if (ISBITMASK(0x00ff, 0xff00, 0, 0))
            {
                return D3DFMT_V8U8;
            }
            break;
        }
    }
    else if (ddpf.flags & DDS_BUMPLUMINANCE)
    {
        switch (ddpf.RGBBitCount)
        {
        case 32:
            if (ISBITMASK(0x000000ff, 0x0000ff00, 0x00ff0000, 0))
            {
                return D3DFMT_X8L8V8U8;
            }
            break;

        case 16:
            if (ISBITMASK(0x001f, 0x03e0, 0xfc00, 0))
            {
                return D3DFMT_L6V5U5;
            }
            break;
        }
    }
    else if (ddpf.flags & DDS_FOURCC)
    {
        if (MAKEFOURCC('D', 'X', 'T', '1') == ddpf.fourCC)
        {
            return D3DFMT_DXT1;
        }
        if (MAKEFOURCC('D', 'X', 'T', '2') == ddpf.fourCC)
        {
            return D3DFMT_DXT2;
        }
        if (MAKEFOURCC('D', 'X', 'T', '3') == ddpf.fourCC)
        {
            return D3DFMT_DXT3;
        }
        if (MAKEFOURCC('D', 'X', 'T', '4') == ddpf.fourCC)
        {
            return D3DFMT_DXT4;
        }
        if (MAKEFOURCC('D', 'X', 'T', '5') == ddpf.fourCC)
        {
            return D3DFMT_DXT5;
        }

        if (MAKEFOURCC('R', 'G', 'B', 'G') == ddpf.fourCC)
        {
            return D3DFMT_R8G8_B8G8;
        }
        if (MAKEFOURCC('G', 'R', 'G', 'B') == ddpf.fourCC)
        {
            return D3DFMT_G8R8_G8B8;
        }

        if (MAKEFOURCC('U', 'Y', 'V', 'Y') == ddpf.fourCC)
        {
            return D3DFMT_UYVY;
        }
        if (MAKEFOURCC('Y', 'U', 'Y', '2') == ddpf.fourCC)
        {
            return D3DFMT_YUY2;
        }

        // Check for D3DFORMAT enums being set here
        switch (ddpf.fourCC)
        {
        case D3DFMT_A16B16G16R16:
        case D3DFMT_Q16W16V16U16:
        case D3DFMT_R16F:
        case D3DFMT_G16R16F:
        case D3DFMT_A16B16G16R16F:
        case D3DFMT_R32F:
        case D3DFMT_G32R32F:
        case D3DFMT_A32B32G32R32F:
        case D3DFMT_CxV8U8:
            return static_cast<D3DFORMAT>(ddpf.fourCC);
        }
    }

    return D3DFMT_UNKNOWN;
}

#undef ISBITMASK


//--------------------------------------------------------------------------------------
HRESULT CreateRHITextureFromDDS(
    _In_ const DDS_HEADER* header,
    _In_reads_bytes_(bitSize) const uint8_t* bitData,
    _In_ size_t bitSize,
    _In_ eUsage usage,
    _In_ D3DPOOL pool,
    _Outptr_ LPIRENDER_TEXTURE* texture,
    bool generateMipsIfMissing) noexcept
{
    HRESULT hr = S_OK;

    UINT iWidth = header->width;
    UINT iHeight = header->height;

    UINT iMipCount = header->mipMapCount;
    if (0 == iMipCount)
    {
        iMipCount = 1;
    }

    // Bound sizes (for security purposes we don't trust DDS file metadata larger than the D3D 10 hardware requirements)
    if (iMipCount > 14u /*D3D10_REQ_MIP_LEVELS*/)
    {
        return HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
    }

    // We could support a subset of 'DX10' extended header DDS files, but we'll assume here we are only
    // supporting legacy DDS files for a Direct3D9 device

    const D3DFORMAT fmt = GetD3D9FormatEx(header->ddspf);
    if (fmt == D3DFMT_UNKNOWN || BitsPerPixel(fmt) == 0)
    {
        return HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
    }

    // #TODO: Only 2D Textures for now
    
    //if (header->flags & DDS_HEADER_FLAGS_VOLUME)
    //{
    //    UINT iDepth = header->depth;

    //    if ((iWidth > 2048u /*D3D10_REQ_TEXTURE3D_U_V_OR_W_DIMENSION*/)
    //        || (iHeight > 2048u /*D3D10_REQ_TEXTURE3D_U_V_OR_W_DIMENSION*/)
    //        || (iDepth > 2048u /*D3D10_REQ_TEXTURE3D_U_V_OR_W_DIMENSION*/))
    //    {
    //        return HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
    //    }

    //    // Create the volume texture (let the runtime do the validation)
    //    ComPtr<IDirect3DVolumeTexture9> pTexture;
    //    hr = device->CreateVolumeTexture(iWidth, iHeight, iDepth, iMipCount,
    //        usage, fmt, pool, pTexture.GetAddressOf(), nullptr);
    //    if (FAILED(hr))
    //        return hr;

    //    ComPtr<IDirect3DVolumeTexture9> pStagingTexture;
    //    if (pool == D3DPOOL_DEFAULT)
    //    {
    //        hr = device->CreateVolumeTexture(iWidth, iHeight, iDepth, iMipCount,
    //            0u, fmt, D3DPOOL_SYSTEMMEM, pStagingTexture.GetAddressOf(), nullptr);
    //        if (FAILED(hr))
    //            return hr;
    //    }
    //    else
    //    {
    //        pStagingTexture = pTexture;
    //    }

    //    // Lock, fill, unlock
    //    size_t NumBytes = 0;
    //    size_t RowBytes = 0;
    //    size_t NumRows = 0;
    //    const uint8_t* pSrcBits = bitData;
    //    const uint8_t* pEndBits = bitData + bitSize;
    //    D3DLOCKED_BOX LockedBox = {};

    //    for (UINT i = 0; i < iMipCount; ++i)
    //    {
    //        GetSurfaceInfo(iWidth, iHeight, fmt, &NumBytes, &RowBytes, &NumRows);

    //        if (NumBytes > UINT32_MAX || RowBytes > UINT32_MAX)
    //            return HRESULT_FROM_WIN32(ERROR_ARITHMETIC_OVERFLOW);

    //        if ((pSrcBits + (NumBytes * iDepth)) > pEndBits)
    //        {
    //            return HRESULT_FROM_WIN32(ERROR_HANDLE_EOF);
    //        }

    //        if (SUCCEEDED(pStagingTexture->LockBox(i, &LockedBox, nullptr, 0)))
    //        {
    //            auto pDestBits = static_cast<uint8_t*>(LockedBox.pBits);

    //            for (UINT j = 0; j < iDepth; ++j)
    //            {
    //                uint8_t* dptr = pDestBits;
    //                const uint8_t* sptr = pSrcBits;

    //                // Copy stride line by line
    //                for (size_t h = 0; h < NumRows; h++)
    //                {
    //                    memcpy_s(dptr, static_cast<size_t>(LockedBox.RowPitch), sptr, RowBytes);
    //                    dptr += LockedBox.RowPitch;
    //                    sptr += RowBytes;
    //                }

    //                pDestBits += LockedBox.SlicePitch;
    //                pSrcBits += NumBytes;
    //            }

    //            pStagingTexture->UnlockBox(i);
    //        }

    //        iWidth = iWidth >> 1;
    //        iHeight = iHeight >> 1;
    //        iDepth = iDepth >> 1;
    //        if (iWidth == 0)
    //            iWidth = 1;
    //        if (iHeight == 0)
    //            iHeight = 1;
    //        if (iDepth == 0)
    //            iDepth = 1;
    //    }

    //    if (pool == D3DPOOL_DEFAULT)
    //    {
    //        hr = device->UpdateTexture(pStagingTexture.Get(), pTexture.Get());
    //        if (FAILED(hr))
    //            return hr;
    //    }

    //    *texture = pTexture.Detach();
    //}
    //else if (header->caps2 & DDS_CUBEMAP)
    //{
    //    if ((iWidth > 8192u /*D3D10_REQ_TEXTURECUBE_DIMENSION*/)
    //        || (iHeight > 8192u /*D3D10_REQ_TEXTURECUBE_DIMENSION*/))
    //    {
    //        return HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
    //    }

    //    // We require at least one face to be defined, and the faces must be square
    //    if ((header->caps2 & DDS_CUBEMAP_ALLFACES) == 0 || iHeight != iWidth)
    //    {
    //        return HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
    //    }

    //    // Create the cubemap (let the runtime do the validation)
    //    ComPtr<IDirect3DCubeTexture9> pTexture;
    //    hr = device->CreateCubeTexture(iWidth, iMipCount,
    //        usage, fmt, pool, pTexture.GetAddressOf(), nullptr);
    //    if (FAILED(hr))
    //        return hr;

    //    ComPtr<IDirect3DCubeTexture9> pStagingTexture;
    //    if (pool == D3DPOOL_DEFAULT)
    //    {
    //        hr = device->CreateCubeTexture(iWidth, iMipCount,
    //            0u, fmt, D3DPOOL_SYSTEMMEM, pStagingTexture.GetAddressOf(), nullptr);
    //        if (FAILED(hr))
    //            return hr;
    //    }
    //    else
    //    {
    //        pStagingTexture = pTexture;
    //    }

    //    // Lock, fill, unlock
    //    size_t NumBytes = 0;
    //    size_t RowBytes = 0;
    //    size_t NumRows = 0;
    //    const uint8_t* pSrcBits = bitData;
    //    const uint8_t* pEndBits = bitData + bitSize;
    //    D3DLOCKED_RECT LockedRect = {};

    //    UINT mask = DDS_CUBEMAP_POSITIVEX & ~DDS_CUBEMAP;
    //    for (UINT f = 0; f < 6; ++f, mask <<= 1)
    //    {
    //        if (!(header->caps2 & mask))
    //            continue;

    //        UINT w = iWidth;
    //        UINT h = iHeight;
    //        for (UINT i = 0; i < iMipCount; ++i)
    //        {
    //            GetSurfaceInfo(w, h, fmt, &NumBytes, &RowBytes, &NumRows);

    //            if (NumBytes > UINT32_MAX || RowBytes > UINT32_MAX)
    //                return HRESULT_FROM_WIN32(ERROR_ARITHMETIC_OVERFLOW);

    //            if ((pSrcBits + NumBytes) > pEndBits)
    //            {
    //                return HRESULT_FROM_WIN32(ERROR_HANDLE_EOF);
    //            }

    //            if (SUCCEEDED(pStagingTexture->LockRect(static_cast<D3DCUBEMAP_FACES>(f), i, &LockedRect, nullptr, 0)))
    //            {
    //                auto pDestBits = static_cast<uint8_t*>(LockedRect.pBits);

    //                // Copy stride line by line
    //                for (size_t r = 0; r < NumRows; r++)
    //                {
    //                    memcpy_s(pDestBits, static_cast<size_t>(LockedRect.Pitch), pSrcBits, RowBytes);
    //                    pDestBits += LockedRect.Pitch;
    //                    pSrcBits += RowBytes;
    //                }

    //                pStagingTexture->UnlockRect(static_cast<D3DCUBEMAP_FACES>(f), i);
    //            }

    //            w = w >> 1;
    //            h = h >> 1;
    //            if (w == 0)
    //                w = 1;
    //            if (h == 0)
    //                h = 1;
    //        }
    //    }

    //    if (pool == D3DPOOL_DEFAULT)
    //    {
    //        hr = device->UpdateTexture(pStagingTexture.Get(), pTexture.Get());
    //        if (FAILED(hr))
    //            return hr;
    //    }

    //    *texture = pTexture.Detach();
    //}
    //else
    //{
        if ((iWidth > 8192u /*D3D10_REQ_TEXTURE2D_U_OR_V_DIMENSION*/)
            || (iHeight > 8192u /*D3D10_REQ_TEXTURE2D_U_OR_V_DIMENSION*/))
        {
            return HRESULT_FROM_WIN32(ERROR_NOT_SUPPORTED);
        }

        // Create the texture (let the runtime do the validation)
        //if (generateMipsIfMissing)
        //    usage |= D3DUSAGE_AUTOGENMIPMAP;

        //ComPtr<IDirect3DTexture9> pTexture;
        //hr = device->CreateTexture(iWidth, iHeight, iMipCount,
        //    usage, fmt, pool,
        //    pTexture.GetAddressOf(), nullptr);
        //if (FAILED(hr))
        //    return hr;

        TextureDesc textureDesc;
        memset(&textureDesc, 0, sizeof(textureDesc));
        textureDesc.width = iWidth;
        textureDesc.height = iHeight;
        textureDesc.depthOrSliceNum = 0;
        textureDesc.numMips = iMipCount;
        textureDesc.usage = usage;


        IRHITexture* pTexture = nullptr;
        bool successed = g_RenderRHI->CreateAPITexture( &textureDesc, nullptr, 0 );
        R_ASSERT(successed);

        // #TODO: D3DPOOL_DEFAULT support
        
        //ComPtr<IDirect3DTexture9> pStagingTexture;
        //if (pool == D3DPOOL_DEFAULT)
        //{
        //    hr = device->CreateTexture(iWidth, iHeight, iMipCount,
        //        0u, fmt, D3DPOOL_SYSTEMMEM, pStagingTexture.GetAddressOf(), nullptr);
        //    if (FAILED(hr))
        //        return hr;
        //}
        //else
        //{
        //    pStagingTexture = pTexture;
        //}

        IRHITexture* pStagingTexture = pTexture;

        // Lock, fill, unlock
        size_t NumBytes = 0;
        size_t RowBytes = 0;
        size_t NumRows = 0;
        const uint8_t* pSrcBits = bitData;
        const uint8_t* pEndBits = bitData + bitSize;
        LOCKED_RECT LockedRect = {};

        for (UINT i = 0; i < iMipCount; ++i)
        {
            GetSurfaceInfo(iWidth, iHeight, fmt, &NumBytes, &RowBytes, &NumRows);

            if (NumBytes > UINT32_MAX || RowBytes > UINT32_MAX)
                return HRESULT_FROM_WIN32(ERROR_ARITHMETIC_OVERFLOW);

            if ((pSrcBits + NumBytes) > pEndBits)
            {
                return HRESULT_FROM_WIN32(ERROR_HANDLE_EOF);
            }

            if (pStagingTexture->LockRect(i, &LockedRect, nullptr, 0))
            {
                auto pDestBits = static_cast<uint8_t*>(LockedRect.pBits);

                // Copy stride line by line
                for (UINT h = 0; h < NumRows; h++)
                {
                    memcpy_s(pDestBits, static_cast<size_t>(LockedRect.Pitch), pSrcBits, RowBytes);
                    pDestBits += LockedRect.Pitch;
                    pSrcBits += RowBytes;
                }

                pStagingTexture->UnlockRect(i);
            }

            iWidth = iWidth >> 1;
            iHeight = iHeight >> 1;
            if (iWidth == 0)
                iWidth = 1;
            if (iHeight == 0)
                iHeight = 1;
        }

        // #TODO: D3DPOOL_DEFAULT support

        //if (pool == D3DPOOL_DEFAULT)
        //{
        //    hr = device->UpdateTexture(pStagingTexture.Get(), pTexture.Get());
        //    if (FAILED(hr))
        //        return hr;
        //}

        *texture = pTexture;
    //}

    return hr;
}

#endif
