#include "stdafx.h"

#include <DirectXPackedVector.h>
#include "../../xrEngine/string_table.h"
#include <memory>
#include <wincodec.h>
#include <DirectXTex.h>

using namespace DirectX;

#include "../xrRender/tga.h"
#include "xrImage_Resampler.h"

using namespace DirectX;
using namespace DirectX::PackedVector;

#include "../xrRender/ScreenshotManager.h"

void CRender::ScreenshotImpl(ScreenshotMode Mode, LPCSTR Name, CMemoryWriter* MemoryWriter)
{
    ScreenshotManager::SaveScreenshot(Mode, Name, MemoryWriter);
}

void CRender::ScreenshotAsyncEnd(CMemoryWriter& memory_writer) 
{
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
    if (Target->rt_Color->fmt == ERHI_FORMAT::R16G16B16A16_FLOAT)
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
