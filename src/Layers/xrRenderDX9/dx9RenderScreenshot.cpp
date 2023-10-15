#include "stdafx.h"

#include <DirectXPackedVector.h>

#include "../xrRender/tga.h"
#include "../../xrEngine/xrImage_Resampler.h"

using namespace DirectX;
using namespace DirectX::PackedVector;

extern int GAMESAVE_SIZE;
extern int SM_FOR_SEND_WIDTH;
extern int SM_FOR_SEND_HEIGHT;

void CRender::ScreenshotImpl(ScreenshotMode mode, LPCSTR name, CMemoryWriter* memory_writer) {
    if (!Device.b_is_Ready) {
        return;
    }
    u32* pPixel = nullptr;
    u32* pEnd   = nullptr;

    // Create temp-surface
    IDirect3DSurface9* pFB;
    D3DLOCKED_RECT D;
    HRESULT hr = HW.pDevice->CreateOffscreenPlainSurface(RCache.get_width(), RCache.get_height(), HW.DevPP.BackBufferFormat, D3DPOOL_SYSTEMMEM, &pFB, NULL);
    if (hr != D3D_OK) {
        return;
    }

    hr = HW.pDevice->GetRenderTargetData(HW.pBaseRT, pFB);
    if (hr != D3D_OK) {
        goto _end_;
    }

    hr = pFB->LockRect(&D, 0, D3DLOCK_NOSYSLOCK);
    if (hr != D3D_OK) {
        goto _end_;
    }

    // Image processing (gamma-correct)
    pPixel = (u32*)D.pBits;
    pEnd = pPixel + u32(RCache.get_width() * RCache.get_height());
    //	IGOR: Remove inverse color correction and kill alpha
    /*
    D3DGAMMARAMP	G;
    dxRenderDeviceRender::Instance().gammaGenLUT(G);
    for (int i=0; i<256; i++) {
        G.red	[i]	/= 256;
        G.green	[i]	/= 256;
        G.blue	[i]	/= 256;
    }
    for (;pPixel!=pEnd; pPixel++)	{
        u32 p = *pPixel;
        *pPixel = color_xrgb	(
            G.red	[color_get_R(p)],
            G.green	[color_get_G(p)],
            G.blue	[color_get_B(p)]
            );
    }
    */

    //	Kill alpha
    for (; pPixel != pEnd; pPixel++) {
        u32 p = *pPixel;
        *pPixel = color_xrgb(
            color_get_R(p),
            color_get_G(p),
            color_get_B(p)
        );
    }

    hr = pFB->UnlockRect();
    if (hr != D3D_OK) {
        goto _end_;
    }

    // Save
    switch (mode) {
    case IRender_interface::SM_FOR_GAMESAVE:
    {
        // texture
        ID3DTexture2D* texture = nullptr;
        hr = D3DXCreateTexture(HW.pDevice, GAMESAVE_SIZE, GAMESAVE_SIZE, 1, 0, D3DFMT_DXT1, D3DPOOL_SCRATCH, &texture);
        if (hr != D3D_OK) {
            goto _end_;
        }
        if (NULL == texture) {
            goto _end_;
        }

        // resize&convert to surface
        IDirect3DSurface9* surface = 0;
        hr = texture->GetSurfaceLevel(0, &surface);
        if (hr != D3D_OK) {
            goto _end_;
        }
        VERIFY(surface);
        hr = D3DXLoadSurfaceFromSurface(surface, 0, 0, pFB, 0, 0, D3DX_DEFAULT, 0);
        _RELEASE(surface);
        if (hr != D3D_OK) {
            goto _end_;
        }

        // save (logical & physical)
        ID3DXBuffer* saved = 0;
        hr = D3DXSaveTextureToFileInMemory(&saved, D3DXIFF_DDS, texture, 0);
        if (hr != D3D_OK) {
            goto _end_;
        }

        IWriter* fs = FS.w_open(name);
        if (fs) {
            fs->w(saved->GetBufferPointer(), saved->GetBufferSize());
            FS.w_close(fs);
        }
        _RELEASE(saved);

        // cleanup
        _RELEASE(texture);
    }
    break;
    case IRender_interface::SM_FOR_MPSENDING:
    {
        // texture
        ID3DTexture2D* texture = nullptr;
        hr = D3DXCreateTexture(HW.pDevice, SM_FOR_SEND_WIDTH, SM_FOR_SEND_HEIGHT, 1, 0, D3DFMT_R8G8B8, D3DPOOL_SCRATCH, &texture);
        if (hr != D3D_OK) {
            goto _end_;
        }
        if (NULL == texture) {
            goto _end_;
        }

        // resize&convert to surface
        IDirect3DSurface9* surface = nullptr;
        hr = texture->GetSurfaceLevel(0, &surface);
        if (hr != D3D_OK) {
            goto _end_;
        }
        VERIFY(surface);
        hr = D3DXLoadSurfaceFromSurface(surface, 0, 0, pFB, 0, 0, D3DX_DEFAULT, 0);
        _RELEASE(surface);
        if (hr != D3D_OK) {
            goto _end_;
        }

        // save (logical & physical)
        ID3DXBuffer* saved = nullptr;
        hr = D3DXSaveTextureToFileInMemory(&saved, D3DXIFF_DDS, texture, 0);
        if (hr != D3D_OK) {
            goto _end_;
        }

        if (!memory_writer) {
            IWriter* fs = FS.w_open(name);
            if (fs) {
                fs->w(saved->GetBufferPointer(), saved->GetBufferSize());
                FS.w_close(fs);
            }
        } else {
            memory_writer->w(saved->GetBufferPointer(), saved->GetBufferSize());
        }

        _RELEASE(saved);

        // cleanup
        _RELEASE(texture);

    }break;
    case IRender_interface::SM_NORMAL:
    {
        string64 t_stemp;
        string_path buf;
        xr_sprintf(buf, sizeof(buf), "ss_%s_%s_(%s).jpg", Core.UserName, timestamp(t_stemp), (g_pGameLevel) ? g_pGameLevel->name().c_str() : "mainmenu");
        ID3DXBuffer* saved = nullptr;
        CHK_DX(D3DXSaveSurfaceToFileInMemory(&saved, D3DXIFF_JPG, pFB, 0, 0));
        IWriter* fs = FS.w_open("$screenshots$", buf); R_ASSERT(fs);
        fs->w(saved->GetBufferPointer(), saved->GetBufferSize());
        FS.w_close(fs);
        _RELEASE(saved);
        if (strstr(Core.Params, "-ss_tga")) { // hq
            xr_sprintf(buf, sizeof(buf), "ssq_%s_%s_(%s).tga", Core.UserName, timestamp(t_stemp), (g_pGameLevel) ? g_pGameLevel->name().c_str() : "mainmenu");
            saved = nullptr;
            CHK_DX(D3DXSaveSurfaceToFileInMemory(&saved, D3DXIFF_TGA, pFB, 0, 0));
            fs = FS.w_open("$screenshots$", buf); 
            R_ASSERT(fs);
            fs->w(saved->GetBufferPointer(), saved->GetBufferSize());
            FS.w_close(fs);
            _RELEASE(saved);
        }
    }
    break;
    case IRender_interface::SM_FOR_LEVELMAP:
    case IRender_interface::SM_FOR_CUBEMAP:
    {
        string_path buf;
        VERIFY(name);
        strconcat(sizeof(buf), buf, name, ".tga");
        IWriter* fs = FS.w_open("$screenshots$", buf); R_ASSERT(fs);
        TGAdesc p;
        p.format = IMG_24B;

        //	TODO: DX10: This is totally incorrect but mimics 
        //	original behavior. Fix later.
        hr = pFB->LockRect(&D, 0, D3DLOCK_NOSYSLOCK);
        if (hr != D3D_OK) {
            return;
        }
        hr = pFB->UnlockRect();
        if (hr != D3D_OK) {
            goto _end_;
        }

        // save
        u32* data = (u32*)xr_malloc(RCache.get_height() * RCache.get_height() * 4);
        imf_Process(data, RCache.get_height(), RCache.get_height(), (u32*)D.pBits,  RCache.get_width(), RCache.get_height(), imf_lanczos3);
        p.scanlenght = RCache.get_height() * 4;
        p.width = RCache.get_height();
        p.height = RCache.get_height();
        p.data = data;
        p.maketga(*fs);
        xr_free(data);

        FS.w_close(fs);
    }
    break;
    }

_end_:
    _RELEASE(pFB);
}

void CRender::ScreenshotAsyncEnd(CMemoryWriter& memory_writer) {
    if (!Device.b_is_Ready) {
        return;
    }
    VERIFY(!m_bMakeAsyncSS);

    D3DLOCKED_RECT D;
    IDirect3DSurface9* pFB = Target->pFB;

    HRESULT hr = pFB->LockRect(&D, 0, D3DLOCK_NOSYSLOCK);
    if (hr != D3D_OK) {
        return;
    }

#if	RENDER == R_R1
    u32 rtWidth = Target->get_rtwidth();
    u32 rtHeight = Target->get_rtheight();
#else	//	RENDER != R_R1
    u32 rtWidth =  RCache.get_width();
    u32 rtHeight = RCache.get_height();
#endif	//	RENDER != R_R1

    // Image processing (gamma-correct)
    auto pPixel = static_cast<u32*>(D.pBits);
    auto pOrigin = pPixel;
    auto pEnd = pPixel + (rtWidth * rtHeight);

    //	Kill alpha
#if	RENDER != R_R1
    if (Target->rt_Color->fmt == D3DFMT_A16B16G16R16F)
    {
        static const int iMaxPixelsInARow = 1024;
        auto pPixelElement16 = (float*)pPixel;

        HALF tmpArray[4 * iMaxPixelsInARow]{};
        while (pPixel != pEnd) {
            const int iProcessPixels = _min(iMaxPixelsInARow, (s32)(pEnd - pPixel));

            XMConvertFloatToHalfStream(tmpArray, sizeof(tmpArray[0]), pPixelElement16, sizeof(pPixelElement16[0]), iProcessPixels * 4);

            for (int i = 0; i < iProcessPixels; ++i) {
                *pPixel = color_argb_f(
                    1.0f,
                    tmpArray[i * 4],
                    tmpArray[i * 4 + 1],
                    tmpArray[i * 4 + 2]
                );

                ++pPixel;
            }

            pPixelElement16 += iProcessPixels * 4;
        }
    }
    else
#endif	//	RENDER != R_R1
    {
        for (; pPixel != pEnd; pPixel++) {
            u32 p = *pPixel;
            *pPixel = color_xrgb(
                color_get_R(p),
                color_get_G(p),
                color_get_B(p)
            );
        }

        memory_writer.w(&rtWidth, sizeof(rtWidth));
        memory_writer.w(&rtHeight, sizeof(rtHeight));
        memory_writer.w(pOrigin, (rtWidth * rtHeight) * 4);
    }

    hr = pFB->UnlockRect();
}
