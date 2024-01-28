// Texture.cpp: implementation of the CTexture class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#pragma hdrstop

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
    while ((l > 1) && skip)
    {
        w /= 2;
        h /= 2;
        l -= 1;

        skip--;
    }
    if (w < 1)	w = 1;
    if (h < 1)	h = 1;
}

ID3DTexture2D* TW_LoadTextureFromTexture
(
    ID3DTexture2D* t_from,
    D3DFORMAT& t_dest_fmt,
    int						levels_2_skip,
    u32& w,
    u32& h
) {
    // Calculate levels & dimensions
    ID3DTexture2D* t_dest = nullptr;
    D3DSURFACE_DESC t_from_desc0 = {};
    R_CHK(t_from->GetLevelDesc(0, &t_from_desc0));
    int levels_exist = t_from->GetLevelCount();
    int top_width = t_from_desc0.Width;
    int top_height = t_from_desc0.Height;
    Reduce(top_width, top_height, levels_exist, levels_2_skip);

    // Create HW-surface
    if (D3DX_DEFAULT == t_dest_fmt)	t_dest_fmt = t_from_desc0.Format;
    R_CHK(D3DXCreateTexture(
        RDevice,
        top_width, top_height,
        levels_exist, 0, t_dest_fmt,
        (RImplementation.o.no_ram_textures ? D3DPOOL_DEFAULT : D3DPOOL_MANAGED),
        &t_dest
    ));

    // Copy surfaces & destroy temporary
    ID3DTexture2D* T_src = t_from;
    ID3DTexture2D* T_dst = t_dest;

    int L_src = T_src->GetLevelCount() - 1;
    int L_dst = T_dst->GetLevelCount() - 1;
    for (; L_dst >= 0; L_src--, L_dst--)
    {
        // Get surfaces
        IDirect3DSurface9* S_src{}, * S_dst{};
        R_CHK(T_src->GetSurfaceLevel(L_src, &S_src));
        R_CHK(T_dst->GetSurfaceLevel(L_dst, &S_dst));

        // Copy
        R_CHK(D3DXLoadSurfaceFromSurface(S_dst, nullptr, nullptr, S_src, nullptr, nullptr, D3DX_FILTER_NONE, 0));

        // Release surfaces
        _RELEASE(S_src);
        _RELEASE(S_dst);
    }

    // OK
    w = top_width;
    h = top_height;
    return					t_dest;
}

ID3DBaseTexture* CRender::texture_load(LPCSTR fRName, u32& ret_msize) {
    ID3DTexture2D* pTexture2D = nullptr;
    IDirect3DCubeTexture9* pTextureCUBE = nullptr;
    string_path fn = {};
    u32 dwWidth, dwHeight;
    u32 img_size = 0;
    int img_loaded_lod = 0;
    D3DFORMAT fmt = {};
    u32 mip_cnt = u32(-1);
    // validation
    R_ASSERT(fRName);
    R_ASSERT(fRName[0]);

    HRESULT result_ = {};
    // make file name
    string_path				fname;
    xr_strcpy(fname, fRName); //. andy if (strext(fname)) *strext(fname)=0;
    fix_texture_name(fname);
    IReader* S = nullptr;

    if (!FS.exist(fn, "$game_textures$", fname, ".dds") && strstr(fname, "_bump"))
        goto _BUMP_from_base;
    if (FS.exist(fn, "$level$", fname, ".dds"))
        goto _DDS;
    if (FS.exist(fn, "$game_saves$", fname, ".dds"))
        goto _DDS;
    if (FS.exist(fn, "$game_textures$", fname, ".dds"))
        goto _DDS;

#ifdef _EDITOR
    ELog.Msg(mtError, "Can't find texture '%s'", fname);
    return 0;
#else

    Msg("! Can't find texture '%s'", fname);
    R_ASSERT(FS.exist(fn, "$game_textures$", "ed\\ed_not_existing_texture", ".dds"));
    goto _DDS;

#endif

_DDS:
    {
        // Load and get header
        D3DXIMAGE_INFO IMG;
        S = FS.r_open(fn);
#ifdef DEBUG
        Msg("* Loaded: %s[%d]b", fn, S->length());
#endif // DEBUG
        img_size = S->length();
        R_ASSERT(S);
        result_ = D3DXGetImageInfoFromFileInMemory(S->pointer(), S->length(), &IMG);
        if (FAILED(result_)) {
            Msg("! Can't get image info for texture '%s'", fn);
            FS.r_close(S);
            string_path			temp;
            R_ASSERT(FS.exist(temp, "$game_textures$", "ed\\ed_not_existing_texture", ".dds"));
            R_ASSERT(xr_strcmp(temp, fn));
            xr_strcpy(fn, temp);
            goto _DDS;
        }

        if (IMG.ResourceType == D3DRTYPE_CUBETEXTURE) {
            goto _DDS_CUBE;
        } else {
            goto _DDS_2D;
        }

    _DDS_CUBE:
        {
            result_ =
                D3DXCreateCubeTextureFromFileInMemoryEx(
                    RDevice,
                    S->pointer(), S->length(),
                    D3DX_DEFAULT,
                    IMG.MipLevels, 0,
                    IMG.Format,
                    (RImplementation.o.no_ram_textures ? D3DPOOL_DEFAULT : D3DPOOL_MANAGED),
                    D3DX_DEFAULT,
                    D3DX_DEFAULT,
                    0, &IMG, 0,
                    &pTextureCUBE
                );
            FS.r_close(S);

            if (FAILED(result_)) {
                Msg("! Can't load texture '%s'", fn);
                string_path			temp;
                R_ASSERT(FS.exist(temp, "$game_textures$", "ed\\ed_not_existing_texture", ".dds"));
                R_ASSERT(xr_strcmp(temp, fn));
                xr_strcpy(fn, temp);
                goto _DDS;
            }

            // OK
            dwWidth = IMG.Width;
            dwHeight = IMG.Height;
            fmt = IMG.Format;
            ret_msize = calc_texture_size(img_loaded_lod, mip_cnt, img_size);
            mip_cnt = pTextureCUBE->GetLevelCount();
            return pTextureCUBE;
        }
    _DDS_2D:
        {
            _strlwr(fn);
            // Load   SYS-MEM-surface, bound to device restrictions
            ID3DTexture2D* T_sysmem;
            HRESULT const result_ =
                D3DXCreateTextureFromFileInMemoryEx(
                    RDevice, S->pointer(), S->length(),
                    D3DX_DEFAULT, D3DX_DEFAULT,
                    IMG.MipLevels, 0,
                    IMG.Format,
                    D3DPOOL_SYSTEMMEM,
                    D3DX_DEFAULT,
                    D3DX_DEFAULT,
                    0, &IMG, 0,
                    &T_sysmem
                );
            FS.r_close(S);

            if (FAILED(result_)) {
                Msg("! Can't load texture '%s'", fn);
                string_path temp{};
                R_ASSERT(FS.exist(temp, "$game_textures$", "ed\\ed_not_existing_texture", ".dds"));
                _strlwr(temp);
                R_ASSERT(xr_strcmp(temp, fn));
                xr_strcpy(fn, temp);
                goto _DDS;
            }

            img_loaded_lod = get_texture_load_lod(fn);
            pTexture2D = TW_LoadTextureFromTexture(T_sysmem, IMG.Format, img_loaded_lod, dwWidth, dwHeight);
            mip_cnt = pTexture2D->GetLevelCount();
            _RELEASE(T_sysmem);

            // OK
            fmt = IMG.Format;
            ret_msize = calc_texture_size(img_loaded_lod, mip_cnt, img_size);
            return					pTexture2D;
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
            goto		_DDS_2D;
        }
        if (strstr(fname, "_bump"))
        {
            R_ASSERT2(FS.exist(fn, "$game_textures$", "ed\\ed_dummy_bump", ".dds"), "ed_dummy_bump");
            S = FS.r_open(fn);

            R_ASSERT2(S, fn);

            img_size = S->length();
            goto		_DDS_2D;
        }
#endif

        return nullptr;
    }
}
