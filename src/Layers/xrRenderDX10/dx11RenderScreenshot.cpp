#include "stdafx.h"
#include "../../xrEngine/string_table.h"
#include "../xrRender/ScreenshotManager.h"

void CRender::ScreenshotImpl(ScreenshotMode Mode, LPCSTR Name, CMemoryWriter* MemoryWriter)
{
	ScreenshotManager::SaveScreenshot(Mode, Name, MemoryWriter);
}

void CRender::ScreenshotAsyncEnd(CMemoryWriter& memory_writer)
{
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