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
#if 1 //ndef _EDITOR
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
