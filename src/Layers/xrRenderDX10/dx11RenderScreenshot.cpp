#include "stdafx.h"
#include "d3dx11tex.h"

using namespace DirectX;

extern int GAMESAVE_SIZE;
extern int SM_FOR_SEND_WIDTH;
extern int SM_FOR_SEND_HEIGHT;

void CRender::ScreenshotImpl(ScreenshotMode mode, LPCSTR name, CMemoryWriter* memory_writer) {
    ID3DResource* pSrcTexture = nullptr;
    HW.pBaseRT->GetResource(&pSrcTexture);

    VERIFY(pSrcTexture);

    // Save
    switch (mode) {
    case IRender_interface::SM_FOR_GAMESAVE:
    {
        ID3DTexture2D* pSrcSmallTexture = nullptr;

        D3D_TEXTURE2D_DESC desc;
        ZeroMemory(&desc, sizeof(desc));
        desc.Width = GAMESAVE_SIZE;
        desc.Height = GAMESAVE_SIZE;
        desc.MipLevels = 1;
        desc.ArraySize = 1;
        desc.Format = DXGI_FORMAT_BC1_UNORM;
        desc.SampleDesc.Count = 1;
        desc.Usage = D3D_USAGE_DEFAULT;
        desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;
        CHK_DX(HW.pDevice->CreateTexture2D(&desc, NULL, &pSrcSmallTexture));

        //	D3DX10_TEXTURE_LOAD_INFO *pLoadInfo

        CHK_DX(D3DX11LoadTextureFromTexture(HW.pContext, pSrcTexture,
            NULL, pSrcSmallTexture));

        // save (logical & physical)
        ID3DBlob* saved = nullptr;
        HRESULT hr = D3DX11SaveTextureToMemory(HW.pContext, pSrcSmallTexture, D3DX11_IFF_DDS, &saved, 0);

        if (hr == D3D_OK) {
            IWriter* fs = FS.w_open(name);
            if (fs) {
                fs->w(saved->GetBufferPointer(), (u32)saved->GetBufferSize());
                FS.w_close(fs);
            }
        }
        _RELEASE(saved);

        // cleanup
        _RELEASE(pSrcSmallTexture);
    }
    break;
    case IRender_interface::SM_FOR_MPSENDING:
    {
        ID3DTexture2D* pSrcSmallTexture = nullptr;

        D3D_TEXTURE2D_DESC desc;
        ZeroMemory(&desc, sizeof(desc));
        desc.Width = SM_FOR_SEND_WIDTH;
        desc.Height = SM_FOR_SEND_HEIGHT;
        desc.MipLevels = 1;
        desc.ArraySize = 1;
        desc.Format = DXGI_FORMAT_BC1_UNORM;
        desc.SampleDesc.Count = 1;
        desc.Usage = D3D_USAGE_DEFAULT;
        desc.BindFlags = D3D_BIND_SHADER_RESOURCE;
        CHK_DX(HW.pDevice->CreateTexture2D(&desc, NULL, &pSrcSmallTexture));

        //	D3DX10_TEXTURE_LOAD_INFO *pLoadInfo

        CHK_DX(D3DX11LoadTextureFromTexture(HW.pContext, pSrcTexture,
            NULL, pSrcSmallTexture));

        // save (logical & physical)
        ID3DBlob* saved = nullptr;
        HRESULT hr = D3DX11SaveTextureToMemory(HW.pContext, pSrcSmallTexture, D3DX11_IFF_DDS, &saved, 0);

        if (hr == D3D_OK) {
            if (!memory_writer) {
                IWriter* fs = FS.w_open(name);
                if (fs) {
                    fs->w(saved->GetBufferPointer(), (u32)saved->GetBufferSize());
                    FS.w_close(fs);
                }
            } else {
                memory_writer->w(saved->GetBufferPointer(), (u32)saved->GetBufferSize());
            }
        }
        _RELEASE(saved);

        // cleanup
        _RELEASE(pSrcSmallTexture);

    }
    break;
    case IRender_interface::SM_NORMAL:
    {
        string64 t_stemp;
        string_path buf;
        xr_sprintf(buf, sizeof(buf), "ss_%s_%s_(%s).jpg", Core.UserName, timestamp(t_stemp), (g_pGameLevel) ? g_pGameLevel->name().c_str() : "mainmenu");
        ID3DBlob* saved = nullptr;

        CHK_DX(D3DX11SaveTextureToMemory(HW.pContext, pSrcTexture, D3DX11_IFF_JPG, &saved, 0));

        IWriter* fs = FS.w_open("$screenshots$", buf); R_ASSERT(fs);
        fs->w(saved->GetBufferPointer(), (u32)saved->GetBufferSize());
        FS.w_close(fs);
        _RELEASE(saved);

        if (strstr(Core.Params, "-ss_tga")) { // hq
            xr_sprintf(buf, sizeof(buf), "ssq_%s_%s_(%s).tga", Core.UserName, timestamp(t_stemp), (g_pGameLevel) ? g_pGameLevel->name().c_str() : "mainmenu");
            saved = nullptr;

            CHK_DX(D3DX11SaveTextureToMemory(HW.pContext, pSrcTexture, D3DX11_IFF_BMP, &saved, 0));

            fs = FS.w_open("$screenshots$", buf); R_ASSERT(fs);
            fs->w(saved->GetBufferPointer(), (u32)saved->GetBufferSize());
            FS.w_close(fs);
            _RELEASE(saved);
        }
    }
    break;
    case IRender_interface::SM_FOR_LEVELMAP:
    case IRender_interface::SM_FOR_CUBEMAP:
    {
        ID3DTexture2D* pSrcSmallTexture = nullptr;

        D3D_TEXTURE2D_DESC desc;
        ZeroMemory(&desc, sizeof(desc));
        desc.Width = Device.dwHeight;
        desc.Height = Device.dwHeight;
        desc.MipLevels = 1;
        desc.ArraySize = 1;
        desc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
        desc.SampleDesc.Count = 1;
        desc.Usage = D3D_USAGE_DEFAULT;
        desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;

        CHK_DX(HW.pDevice->CreateTexture2D(&desc, NULL, &pSrcSmallTexture));

        CHK_DX(D3DX11LoadTextureFromTexture(HW.pContext, pSrcTexture,
            NULL, pSrcSmallTexture));

        // save (logical & physical)
        ID3DBlob* saved = nullptr;

        HRESULT hr = D3DX11SaveTextureToMemory(HW.pContext, pSrcSmallTexture, D3DX11_IFF_DDS, &saved, 0);

        if (hr == D3D_OK) {
            string_path buf;
            VERIFY(name);
            strconcat(sizeof(buf), buf, name, ".dds");
            IWriter* fs = FS.w_open("$screenshots$", buf);
            if (fs) {
                fs->w(saved->GetBufferPointer(), (u32)saved->GetBufferSize());
                FS.w_close(fs);
            }
        }
        _RELEASE(saved);

        // cleanup
        _RELEASE(pSrcSmallTexture);
    }
    break;
    }

    _RELEASE(pSrcTexture);
}

void CRender::ScreenshotAsyncEnd(CMemoryWriter& memory_writer) {
    VERIFY(!m_bMakeAsyncSS);

    //	Don't own. No need to release.
    ID3DTexture2D* pTex = Target->t_ss_async;
    D3D_MAPPED_TEXTURE2D MappedData;
    HW.pContext->Map(pTex, 0, D3D_MAP_READ, 0, &MappedData);
    {
        u32* pPixel = (u32*)MappedData.pData;
        u32* pEnd = pPixel + (Device.dwWidth * Device.dwHeight);

        //	Kill alpha and swap r and b.
        for (; pPixel != pEnd; pPixel++) {
            u32 p = *pPixel;
            *pPixel = color_xrgb(
                color_get_B(p),
                color_get_G(p),
                color_get_R(p)
            );
        }

        memory_writer.w(&Device.dwWidth, sizeof(Device.dwWidth));
        memory_writer.w(&Device.dwHeight, sizeof(Device.dwHeight));
        memory_writer.w(MappedData.pData, (Device.dwWidth * Device.dwHeight) * 4);
    }

    HW.pContext->Unmap(pTex, 0);
}