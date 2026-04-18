#include "stdafx.h"
#include "../../xrEngine/string_table.h"
#include "ScreenshotManager.h"

void CRender::ScreenshotImpl(ScreenshotMode Mode, const char* Name, CMemoryWriter* MemoryWriter)
{
	ScreenshotManager::SaveScreenshot(Mode, Name, MemoryWriter);
}

void CRender::ScreenshotAsyncEnd(CMemoryWriter& memory_writer)
{
	VERIFY(!m_bMakeAsyncSS);

	IRHIRenderTargetView* Rtv = GRHI->GetRenderTargetView(0);
	if (!Rtv)
	{
		return;
	}

	u32 Width = 0;
	u32 Height = 0;
	u32 RowPitch = 0;

	u32 MaxSize = 4096 * 4096 * 4;
	xr_unique_ptr<u8[]> Buffer(new u8[MaxSize]);

	bool Ok = GRHI->DevicePtr->ReadRenderTargetPixels(Rtv, Buffer.get(), MaxSize, Width, Height, RowPitch);
	if (!Ok)
	{
		return;
	}

	// Convert BGRA -> RGBA (swap B and R) and ensure alpha 0xFF
	for (u32 Y = 0; Y < Height; ++Y)
	{
		u8* RowPtr = Buffer.get() + (size_t)Y * RowPitch;
		for (u32 X = 0; X < Width; ++X)
		{
			u8* Pixel = RowPtr + X * 4;
			u8 Temp = Pixel[0];
			Pixel[0] = Pixel[2];
			Pixel[2] = Temp;
			Pixel[3] = 0xFF;
		}
	}

	memory_writer.w(&Width, sizeof(Width));
	memory_writer.w(&Height, sizeof(Height));
	memory_writer.w(Buffer.get(), (Width * Height) * 4);
}