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


#include <string>
#include <unordered_map>
#include <ddraw.h>

const char* D3DFormatToString(D3DFORMAT format) {
    static const xr_map<D3DFORMAT, const char*> formatMap = {
        {D3DFMT_UNKNOWN,             "D3DFMT_UNKNOWN"},
        {D3DFMT_R8G8B8,              "D3DFMT_R8G8B8"},
        {D3DFMT_A8R8G8B8,            "D3DFMT_A8R8G8B8"},
        {D3DFMT_X8R8G8B8,            "D3DFMT_X8R8G8B8"},
        {D3DFMT_R5G6B5,              "D3DFMT_R5G6B5"},
        {D3DFMT_X1R5G5B5,            "D3DFMT_X1R5G5B5"},
        {D3DFMT_A1R5G5B5,            "D3DFMT_A1R5G5B5"},
        {D3DFMT_A4R4G4B4,            "D3DFMT_A4R4G4B4"},
        {D3DFMT_R3G3B2,              "D3DFMT_R3G3B2"},
        {D3DFMT_A8,                  "D3DFMT_A8"},
        {D3DFMT_A8R3G3B2,            "D3DFMT_A8R3G3B2"},
        {D3DFMT_X4R4G4B4,            "D3DFMT_X4R4G4B4"},
        {D3DFMT_A2B10G10R10,         "D3DFMT_A2B10G10R10"},
        {D3DFMT_A8B8G8R8,            "D3DFMT_A8B8G8R8"},
        {D3DFMT_X8B8G8R8,            "D3DFMT_X8B8G8R8"},
        {D3DFMT_G16R16,              "D3DFMT_G16R16"},
        {D3DFMT_A2R10G10B10,         "D3DFMT_A2R10G10B10"},
        {D3DFMT_A16B16G16R16,        "D3DFMT_A16B16G16R16"},
        {D3DFMT_A8P8,                "D3DFMT_A8P8"},
        {D3DFMT_P8,                  "D3DFMT_P8"},
        {D3DFMT_L8,                  "D3DFMT_L8"},
        {D3DFMT_A8L8,                "D3DFMT_A8L8"},
        {D3DFMT_A4L4,                "D3DFMT_A4L4"},
        {D3DFMT_V8U8,                "D3DFMT_V8U8"},
        {D3DFMT_L6V5U5,              "D3DFMT_L6V5U5"},
        {D3DFMT_X8L8V8U8,            "D3DFMT_X8L8V8U8"},
        {D3DFMT_Q8W8V8U8,            "D3DFMT_Q8W8V8U8"},
        {D3DFMT_V16U16,              "D3DFMT_V16U16"},
        {D3DFMT_A2W10V10U10,         "D3DFMT_A2W10V10U10"},
        {D3DFMT_UYVY,                "D3DFMT_UYVY"},
        {D3DFMT_R8G8_B8G8,           "D3DFMT_R8G8_B8G8"},
        {D3DFMT_YUY2,                "D3DFMT_YUY2"},
        {D3DFMT_G8R8_G8B8,           "D3DFMT_G8R8_G8B8"},
        {D3DFMT_DXT1,                "D3DFMT_DXT1"},
        {D3DFMT_DXT2,                "D3DFMT_DXT2"},
        {D3DFMT_DXT3,                "D3DFMT_DXT3"},
        {D3DFMT_DXT4,                "D3DFMT_DXT4"},
        {D3DFMT_DXT5,                "D3DFMT_DXT5"},
        {D3DFMT_D16_LOCKABLE,        "D3DFMT_D16_LOCKABLE"},
        {D3DFMT_D32,                 "D3DFMT_D32"},
        {D3DFMT_D15S1,               "D3DFMT_D15S1"},
        {D3DFMT_D24S8,               "D3DFMT_D24S8"},
        {D3DFMT_D24X8,               "D3DFMT_D24X8"},
        {D3DFMT_D24X4S4,             "D3DFMT_D24X4S4"},
        {D3DFMT_D16,                 "D3DFMT_D16"},
        {D3DFMT_D32F_LOCKABLE,       "D3DFMT_D32F_LOCKABLE"},
        {D3DFMT_D24FS8,              "D3DFMT_D24FS8"},
        {D3DFMT_L16,                 "D3DFMT_L16"},
        {D3DFMT_VERTEXDATA,          "D3DFMT_VERTEXDATA"},
        {D3DFMT_INDEX16,             "D3DFMT_INDEX16"},
        {D3DFMT_INDEX32,             "D3DFMT_INDEX32"},
        {D3DFMT_Q16W16V16U16,        "D3DFMT_Q16W16V16U16"},
        {D3DFMT_MULTI2_ARGB8,        "D3DFMT_MULTI2_ARGB8"},
        {D3DFMT_R16F,                "D3DFMT_R16F"},
        {D3DFMT_G16R16F,             "D3DFMT_G16R16F"},
        {D3DFMT_A16B16G16R16F,       "D3DFMT_A16B16G16R16F"},
        {D3DFMT_R32F,                "D3DFMT_R32F"},
        {D3DFMT_G32R32F,             "D3DFMT_G32R32F"},
        {D3DFMT_A32B32G32R32F,       "D3DFMT_A32B32G32R32F"},
        {D3DFMT_CxV8U8,              "D3DFMT_CxV8U8"},
        {D3DFMT_A1,                  "D3DFMT_A1"},
        {D3DFMT_A2B10G10R10_XR_BIAS, "D3DFMT_A2B10G10R10_XR_BIAS"},
        {D3DFMT_BINARYBUFFER,        "D3DFMT_BINARYBUFFER"},
        {D3DFMT_FORCE_DWORD,         "D3DFMT_FORCE_DWORD"}
    };

    auto it = formatMap.find(format);
    return it != formatMap.end() ? it->second : "D3DFMT_UNKNOWN";
}

void PrintTextureError(HRESULT hr, const char* fname, const void* ddsData, size_t ddsSize, IDirect3DBaseTexture9* pTexture = nullptr)
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
    if (ddsData && ddsSize >= sizeof(DWORD) + sizeof(DDS_HEADER))
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
            Msg("  Actual Format: %s", D3DFormatToString(desc.Format));
            Msg("  Actual Size: %dx%d", desc.Width, desc.Height);
        }
    }

    Msg("--- Possible Solutions ---");
    Msg("1. Verify texture format is supported by GPU");
    Msg("2. Check texture dimensions (max %dx%d)", caps.MaxTextureWidth, caps.MaxTextureHeight);
    Msg("3. Try converting to DXT1/DXT5 format");
    Msg("4. Check mipmap chain consistency");
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
    if (FS.TryLoad(xr_string(fname) + ".dds"))
    {
        xr_string editor_name = xr_string(fname) + ".dds";
        xr_strcpy(fn, editor_name.c_str());
        goto _DDS;
    }
    Msg("! Can't find texture '%s'", fname);

#if 0 //def _EDITOR
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

        D3DCAPS9 d3dCaps;
        if (FAILED(RDevice->GetDeviceCaps(&d3dCaps)))
        {
            string512 errMsg;
            xr_sprintf(errMsg, "Failed to get device capabilities for texture size check.");
            R_ASSERT3(false, errMsg, fname);
        }

        const u32 maxTextureDimension = _max(d3dCaps.MaxTextureWidth, d3dCaps.MaxTextureHeight);

        if (header->width > maxTextureDimension || header->height > maxTextureDimension)
        {
            string512 errMsg;
            xr_sprintf(errMsg, "Texture dimensions exceed hardware limits: %dx%d (Max: %d)",
                header->width, header->height,
                maxTextureDimension);
            R_ASSERT3(false, errMsg, fname);
        }

        if (FAILED(result))
        {
            Msg("! Unsupported texture [%s]", fn);
            string1024 errorMsg;
            xr_sprintf(errorMsg, "Failed to get DDS metadata for '%s'\n"
                "File size: %u bytes\n"
                "Error: %s (0x%08X)\n"
                "Possible causes:\n"
                "- Corrupted DDS header\n"
                "- Unsupported DDS variant",
                fname, S->length(),
                Debug.dxerror2string(result), result);

            VERIFY2(false, errorMsg);
            Msg("! DDS METADATA ERROR: %s", errorMsg);
            FS.r_close(S);

            string_path temp;
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

            if (FAILED(result))
            {
                PrintTextureError(result, fname, &bitData, bitSize, pTexture3D);

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

            img_loaded_lod = get_texture_load_lod(fn);
            pTexture2D = TW_LoadTextureFromTexture(T_sysmem, img_loaded_lod);
            mip_cnt = pTexture2D->GetLevelCount();

            if (FAILED(result))
            {
                PrintTextureError(result, fname, bitData, bitSize, pTexture2D);

                string_path temp;
                R_ASSERT(FS.exist(temp, "$game_textures$", "ed\\ed_not_existing_texture", ".dds"));
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
            R_ASSERT2(FS.exist(fn, "$game_textures$", "ed\\ed_dummy_bump#", ".dds"), "ed_dummy_bump#");
            S = FS.r_open(fn);
            R_ASSERT2(S, fn);
            img_size = S->length();
            goto _DDS;
        }

        Msg("! Fallback to default bump map: %s", fname);
        if (strstr(fname, "_bump"))
        {
            R_ASSERT2(FS.exist(fn, "$game_textures$", "ed\\ed_dummy_bump", ".dds"), "ed_dummy_bump");
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
