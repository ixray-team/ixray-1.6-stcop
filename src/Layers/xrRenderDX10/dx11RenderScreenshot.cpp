#include "stdafx.h"

#include <memory>
#include <wincodec.h>
#include <DirectXTex.h>

using namespace DirectX;

extern int GAMESAVE_SIZE;
extern int SM_FOR_SEND_WIDTH;
extern int SM_FOR_SEND_HEIGHT;

void CRender::ScreenshotImpl(ScreenshotMode mode, LPCSTR name, CMemoryWriter* memory_writer) {
    ID3DResource* pSrcTexture = nullptr;
    RTarget->GetResource(&pSrcTexture);

    VERIFY(pSrcTexture);

    // Create a texture object
    ScratchImage scratchImage = {}, smallScratchImage = {};
    Blob saved = {};

    HRESULT hr = CaptureTexture(RDevice, RContext, pSrcTexture, scratchImage);
    if (FAILED(hr))
    {
        Msg("[ScreenshotImpl] CaptureTexture failed with HRESULT: 0x%08X", hr);
        _RELEASE(pSrcTexture);
        return;
    }

    // Save
    switch (mode) {
    case IRender_interface::SM_FOR_GAMESAVE:
    {
        CHK_DX(Resize(*scratchImage.GetImage(0, 0, 0), GAMESAVE_SIZE, GAMESAVE_SIZE,
            TEX_FILTER_FLAGS::TEX_FILTER_DEFAULT, smallScratchImage));
        CHK_DX(SaveToDDSMemory(*smallScratchImage.GetImage(0, 0, 0), DDS_FLAGS::DDS_FLAGS_NONE, saved));

        auto fs = FS.w_open(name); R_ASSERT(fs);
        fs->w(saved.GetBufferPointer(), (u32)saved.GetBufferSize());
        FS.w_close(fs);
    }
    break;
    case IRender_interface::SM_FOR_MPSENDING:
    {
        CHK_DX(Resize(*scratchImage.GetImage(0, 0, 0), SM_FOR_SEND_WIDTH, SM_FOR_SEND_HEIGHT,
            TEX_FILTER_FLAGS::TEX_FILTER_DEFAULT, smallScratchImage));
        CHK_DX(SaveToDDSMemory(*smallScratchImage.GetImage(0, 0, 0), DDS_FLAGS::DDS_FLAGS_NONE, saved));

        if (!memory_writer)
        {
            auto fs = FS.w_open(name); R_ASSERT(fs);
            fs->w(saved.GetBufferPointer(), (u32)saved.GetBufferSize());
            FS.w_close(fs);
        } else {
            memory_writer->w(saved.GetBufferPointer(), (u32)saved.GetBufferSize());
        }
    }
    break;
    case IRender_interface::SM_NORMAL:
    {
        string64 t_stemp = {};
        string_path buf = {};

        if (ps_screenshot_format == 0) // jpg screenshots - default one
        {
            xr_sprintf(buf, sizeof(buf), "ss_%s_%s_(%s).jpg", Core.UserName, timestamp(t_stemp), (g_pGameLevel) ? g_pGameLevel->name().c_str() : "mainmenu");
            CHK_DX(SaveToWICMemory(*scratchImage.GetImage(0, 0, 0), WIC_FLAGS::WIC_FLAGS_NONE, GUID_ContainerFormatJpeg, saved));
        }
        else if (ps_screenshot_format == 1) // tga screenshots
        {
            xr_sprintf(buf, sizeof(buf), "ss_%s_%s_(%s).tga", Core.UserName, timestamp(t_stemp), (g_pGameLevel) ? g_pGameLevel->name().c_str() : "mainmenu");
            CHK_DX(SaveToTGAMemory(*scratchImage.GetImage(0, 0, 0), TGA_FLAGS::TGA_FLAGS_NONE, saved));
        }
        else if (ps_screenshot_format == 2) // png screenshots
        {
            xr_sprintf(buf, sizeof(buf), "ss_%s_%s_(%s).png", Core.UserName, timestamp(t_stemp), (g_pGameLevel) ? g_pGameLevel->name().c_str() : "mainmenu");
            CHK_DX(SaveToWICMemory(*scratchImage.GetImage(0, 0, 0), WIC_FLAGS::WIC_FLAGS_NONE, GUID_ContainerFormatPng, saved));
        }

        auto fs = FS.w_open("$screenshots$", buf); R_ASSERT(fs);
        fs->w(saved.GetBufferPointer(), (u32)saved.GetBufferSize());
        FS.w_close(fs);
    }
    break;
    case IRender_interface::SM_FOR_LEVELMAP:
    case IRender_interface::SM_FOR_CUBEMAP:
    {
        CHK_DX(Resize(*scratchImage.GetImage(0, 0, 0), Device.TargetHeight, Device.TargetHeight, TEX_FILTER_FLAGS::TEX_FILTER_LINEAR, smallScratchImage));
        CHK_DX(SaveToTGAMemory(*smallScratchImage.GetImage(0, 0, 0), TGA_FLAGS::TGA_FLAGS_NONE, saved));

        string_path buf;
        VERIFY(name);
        xr_strconcat(buf, name, ".tga");
        auto fs = FS.w_open("$screenshots$", buf); R_ASSERT(fs);
        fs->w(saved.GetBufferPointer(), (u32)saved.GetBufferSize());
        FS.w_close(fs);
    }
    break;
    }

    _RELEASE(pSrcTexture);
}

void CRender::ScreenshotAsyncEnd(CMemoryWriter& memory_writer) {
    VERIFY(!m_bMakeAsyncSS);

    //	Don't own. No need to release.
    auto pTex = Target->t_ss_async;
    D3D_MAPPED_TEXTURE2D MappedData;
    CHK_DX(RContext->Map(pTex, 0, D3D_MAP_READ, 0, &MappedData));
    {
        auto pPixel = (u32*)MappedData.pData;
        u32 Width = (u32)(RCache.get_target_width());
        u32 Height = (u32)(RCache.get_target_height());
        auto pEnd = pPixel + Width * Height;

        //	Kill alpha and swap r and b.
        for (; pPixel != pEnd; pPixel++) {
            auto p = *pPixel;
            *pPixel = color_xrgb(
                color_get_B(p),
                color_get_G(p),
                color_get_R(p)
            );
        }

        memory_writer.w(&Width, sizeof(Width));
        memory_writer.w(&Height, sizeof(Height));
        memory_writer.w(MappedData.pData, (Width * Height) * 4);
    }

    RContext->Unmap(pTex, 0);
}