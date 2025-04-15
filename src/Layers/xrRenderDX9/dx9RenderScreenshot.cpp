#include "stdafx.h"

#include <DirectXPackedVector.h>

#include <memory>
#include <wincodec.h>
#include <DirectXTex.h>

using namespace DirectX;

#include "../xrRender/tga.h"
#include "xrImage_Resampler.h"

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
    u32* pDst = nullptr;
    
    // Create temp-surface
    IDirect3DSurface9* pFB = nullptr;
    D3DLOCKED_RECT D;
    ScratchImage scratchImage;
    ScratchImage smallScratchImage;

    HRESULT hr = RDevice->CreateOffscreenPlainSurface(RCache.get_width(), RCache.get_height(), D3DFMT_X8R8G8B8, D3DPOOL_SYSTEMMEM, &pFB, nullptr);
    if (hr != D3D_OK) {
        return;
    }

    hr = RDevice->GetRenderTargetData(RTarget, pFB);
    if (hr != D3D_OK) {
        goto _end_;
    }

    hr = pFB->LockRect(&D, 0, D3DLOCK_NOSYSLOCK);
    if (hr != D3D_OK) {
        goto _end_;
    }

    hr = scratchImage.Initialize2D(DXGI_FORMAT_B8G8R8A8_UNORM, RCache.get_width(), RCache.get_height(), 1, 1);
    if (hr != D3D_OK)
        goto _end_;

    pDst = (u32*)scratchImage.GetPixels();

    // Image processing (gamma-correct)
    pPixel = (u32*)D.pBits;
    pEnd = pPixel + u32(RCache.get_width() * RCache.get_height());

    //	Kill alpha
    for (; pPixel != pEnd; ++pPixel, ++pDst)
    {
        u32 p = *pPixel;

        *pPixel = color_xrgb(
            color_get_R(p),
            color_get_G(p),
            color_get_B(p)
        );

        *pDst = color_xrgb(
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
        // Create a smaller texture
        hr = Resize(*scratchImage.GetImage(0, 0, 0), GAMESAVE_SIZE, GAMESAVE_SIZE, 
            TEX_FILTER_FLAGS::TEX_FILTER_DEFAULT, smallScratchImage);
        if (FAILED(hr))
            goto _end_;

        // Save to memory
        Blob saved;
        auto hr = SaveToDDSMemory(*smallScratchImage.GetImage(0, 0, 0), DirectX::DDS_FLAGS_NONE, saved);
        if (hr == D3D_OK) {
            auto fs = FS.w_open(name);
            if (fs) {
                fs->w(saved.GetBufferPointer(), (u32)saved.GetBufferSize());
                FS.w_close(fs);
            }
        }
    }
    break;
    case IRender_interface::SM_FOR_MPSENDING:
    {
        // Create a smaller texture
        Resize(*scratchImage.GetImage(0, 0, 0), SM_FOR_SEND_WIDTH, SM_FOR_SEND_HEIGHT, 
            TEX_FILTER_FLAGS::TEX_FILTER_DEFAULT, smallScratchImage);

        // Save to memory
        Blob saved;
        auto hr = SaveToDDSMemory(*smallScratchImage.GetImage(0, 0, 0), DDS_FLAGS::DDS_FLAGS_NONE, saved);

        if (hr != D3D_OK) {
            goto _end_;
        }
        if (!memory_writer) {
            auto fs = FS.w_open(name);
            if (fs) {
                fs->w(saved.GetBufferPointer(), (u32)saved.GetBufferSize());
                FS.w_close(fs);
            }
        }
        else {
            memory_writer->w(saved.GetBufferPointer(), (u32)saved.GetBufferSize());
        }
    }break;
    case IRender_interface::SM_NORMAL:
    {
        string64 t_stemp;
        string_path buf;
        Blob saved;
        IWriter* fs;
        if (ps_screenshot_format == 0) // jpg screenshots - default one
        {
            xr_sprintf(buf, sizeof(buf), "ss_%s_%s_(%s).jpg", Core.UserName, timestamp(t_stemp), (g_pGameLevel) ? g_pGameLevel->name().c_str() : "mainmenu");
            R_CHK(SaveToWICMemory(*scratchImage.GetImage(0, 0, 0), WIC_FLAGS::WIC_FLAGS_NONE, GUID_ContainerFormatJpeg, saved));

            fs = FS.w_open("$screenshots$", buf); R_ASSERT(fs);
            fs->w(saved.GetBufferPointer(), (u32)saved.GetBufferSize());
            FS.w_close(fs);

        }
        else if (ps_screenshot_format == 1) // tga screenshots
        {
            xr_sprintf(buf, sizeof(buf), "ss_%s_%s_(%s).tga", Core.UserName, timestamp(t_stemp), (g_pGameLevel) ? g_pGameLevel->name().c_str() : "mainmenu");

            SaveToTGAMemory(*scratchImage.GetImage(0, 0, 0), TGA_FLAGS::TGA_FLAGS_NONE, saved);

            fs = FS.w_open("$screenshots$", buf); R_ASSERT(fs);
            fs->w(saved.GetBufferPointer(), (u32)saved.GetBufferSize());
            FS.w_close(fs);
        }
        else if (ps_screenshot_format == 2) // png screenshots
        {
            xr_sprintf(buf, sizeof(buf), "ss_%s_%s_(%s).png", Core.UserName, timestamp(t_stemp), (g_pGameLevel) ? g_pGameLevel->name().c_str() : "mainmenu");

            R_CHK(SaveToWICMemory(*scratchImage.GetImage(0, 0, 0), WIC_FLAGS::WIC_FLAGS_NONE, GUID_ContainerFormatPng, saved));

            fs = FS.w_open("$screenshots$", buf); R_ASSERT(fs);
            fs->w(saved.GetBufferPointer(), (u32)saved.GetBufferSize());
            FS.w_close(fs);
        }
    }
    break;
    case IRender_interface::SM_FOR_LEVELMAP:
    case IRender_interface::SM_FOR_CUBEMAP:
    {
        string_path buf;
        VERIFY(name);
        xr_strconcat(buf, name, ".tga");
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
