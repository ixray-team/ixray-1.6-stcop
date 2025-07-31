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

    // Save
    switch (mode) {
    case IRender_interface::SM_FOR_GAMESAVE:
    {
        // Create a texture object
        auto pScratchImage = std::make_unique<ScratchImage>();
        auto pSmallScratchImage = std::make_unique<ScratchImage>();

        // Load source texture
        CHK_DX(CaptureTexture(RDevice, RContext, pSrcTexture, *pScratchImage));

        // Create a smaller texture
        Resize(*pScratchImage->GetImage(0, 0, 0), GAMESAVE_SIZE, GAMESAVE_SIZE, 
            TEX_FILTER_FLAGS::TEX_FILTER_DEFAULT, *pSmallScratchImage);

        // Save to memory
        auto saved = std::make_unique<Blob>();
        auto hr = SaveToDDSMemory(*pSmallScratchImage->GetImage(0, 0, 0), DDS_FLAGS::DDS_FLAGS_NONE, *saved);
        if (hr == D3D_OK) {
            auto fs = FS.w_open(name);
            if (fs) {
                fs->w(saved->GetBufferPointer(), (u32)saved->GetBufferSize());
                FS.w_close(fs);
            }
        }
    }
    break;
    case IRender_interface::SM_FOR_MPSENDING:
    {
        // Create a texture object
        auto scratchImage = std::make_unique<ScratchImage>();
        auto smallScratchImage = std::make_unique<ScratchImage>();

        // Load source texture
        CaptureTexture(RDevice, RContext, pSrcTexture, *scratchImage);

        // Create a smaller texture
        Resize(*scratchImage->GetImage(0, 0, 0), SM_FOR_SEND_WIDTH, SM_FOR_SEND_HEIGHT, TEX_FILTER_FLAGS::TEX_FILTER_DEFAULT, *smallScratchImage);

        // Save to memory
        auto saved = std::make_unique<Blob>();
        auto hr = SaveToDDSMemory(*smallScratchImage->GetImage(0, 0, 0), DDS_FLAGS::DDS_FLAGS_NONE, *saved);

        if (hr == D3D_OK) {
            if (!memory_writer) {
                auto fs = FS.w_open(name);
                if (fs) {
                    fs->w(saved->GetBufferPointer(), (u32)saved->GetBufferSize());
                    FS.w_close(fs);
                }
            } else {
                memory_writer->w(saved->GetBufferPointer(), (u32)saved->GetBufferSize());
            }
        }
    }
    break;
    case IRender_interface::SM_NORMAL:
    {
        string64 t_stemp;
        string_path buf;

        auto scratchImage = std::make_unique<ScratchImage>();
        auto saved = std::make_unique<Blob>();
        CaptureTexture(RDevice, RContext, pSrcTexture, *scratchImage);
        if (ps_screenshot_format == 0) // jpg screenshots - default one
        {
            xr_sprintf(buf, sizeof(buf), "ss_%s_%s_(%s).jpg", Core.UserName, timestamp(t_stemp), (g_pGameLevel) ? g_pGameLevel->name().c_str() : "mainmenu");
            SaveToWICMemory(*scratchImage->GetImage(0, 0, 0), WIC_FLAGS::WIC_FLAGS_NONE, GUID_ContainerFormatJpeg, *saved);

            auto fs = FS.w_open("$screenshots$", buf); R_ASSERT(fs);
            fs->w(saved->GetBufferPointer(), (u32)saved->GetBufferSize());
            FS.w_close(fs);

        }
        else if (ps_screenshot_format == 1) // tga screenshots
        {
            xr_sprintf(buf, sizeof(buf), "ss_%s_%s_(%s).tga", Core.UserName, timestamp(t_stemp), (g_pGameLevel) ? g_pGameLevel->name().c_str() : "mainmenu");

            SaveToTGAMemory(*scratchImage->GetImage(0, 0, 0), TGA_FLAGS::TGA_FLAGS_NONE, *saved);

            auto fs = FS.w_open("$screenshots$", buf); R_ASSERT(fs);
            fs->w(saved->GetBufferPointer(), (u32)saved->GetBufferSize());
            FS.w_close(fs);
        }
        else if (ps_screenshot_format == 2) // png screenshots
        {
            xr_sprintf(buf, sizeof(buf), "ss_%s_%s_(%s).png", Core.UserName, timestamp(t_stemp), (g_pGameLevel) ? g_pGameLevel->name().c_str() : "mainmenu");

            SaveToWICMemory(*scratchImage->GetImage(0, 0, 0), WIC_FLAGS::WIC_FLAGS_NONE, GUID_ContainerFormatPng, *saved);

            auto fs = FS.w_open("$screenshots$", buf); R_ASSERT(fs);
            fs->w(saved->GetBufferPointer(), (u32)saved->GetBufferSize());
            FS.w_close(fs);
        }
    }
    break;
    case IRender_interface::SM_FOR_LEVELMAP:
    case IRender_interface::SM_FOR_CUBEMAP:
    {
        // Create a texture object
        auto pScratchImage = std::make_unique<DirectX::ScratchImage>();
        auto pSmallScratchImage = std::make_unique<DirectX::ScratchImage>();

        // Load source texture
        CHK_DX(CaptureTexture(RDevice, RContext, pSrcTexture, *pScratchImage));

        // Create a smaller texture
        CHK_DX(Resize(*pScratchImage->GetImage(0, 0, 0), Device.TargetHeight, Device.TargetHeight, TEX_FILTER_FLAGS::TEX_FILTER_LINEAR, *pSmallScratchImage));

        // Save to memory
        auto saved = std::make_unique<Blob>();
        auto hr = SaveToTGAMemory(*pSmallScratchImage->GetImage(0, 0, 0), TGA_FLAGS::TGA_FLAGS_NONE, *saved);

        if (hr == D3D_OK) {
            string_path buf;
            VERIFY(name);
            xr_strconcat(buf, name, ".tga");
            auto fs = FS.w_open("$screenshots$", buf);
            if (fs) {
                fs->w(saved->GetBufferPointer(), (u32)saved->GetBufferSize());
                FS.w_close(fs);
            }
        }
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
    RContext->Map(pTex, 0, D3D_MAP_READ, 0, &MappedData);
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