#include "stdafx.h"
#include "../../xrEngine/string_table.h"
#include "ScreenshotManager.h"

void CRender::ScreenshotImpl(ScreenshotMode Mode, const char* Name, CMemoryWriter* MemoryWriter)
{
	ScreenshotManager::SaveScreenshot(Mode, Name, MemoryWriter);
}

void CRender::ScreenshotAsyncEnd(CMemoryWriter& memory_writer)
{
	RenderThread::WaitIdle();
	VERIFY(!m_bMakeAsyncSS);

	// The game's picture, as the screenshot command takes it
	IRHIRenderTargetView* Rtv = RTarget;
	if (Rtv == nullptr)
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

	const Irect View = Rtv == RSwapchainTarget ? Device.PresentRect : Irect{ 0, 0, int(Device.TargetWidth), int(Device.TargetHeight) };
	const u32 Left = std::min(u32(std::max(View.x1, 0)), Width);
	const u32 Top = std::min(u32(std::max(View.y1, 0)), Height);
	const u32 AreaWidth = std::clamp(u32(std::max(View.x2, 0)), Left, Width) - Left;
	const u32 AreaHeight = std::clamp(u32(std::max(View.y2, 0)), Top, Height) - Top;

	memory_writer.w(&AreaWidth, sizeof(AreaWidth));
	memory_writer.w(&AreaHeight, sizeof(AreaHeight));

	// Convert BGRA -> RGBA (swap B and R) and ensure alpha 0xFF
	for (u32 Y = Top; Y < Top + AreaHeight; ++Y)
	{
		u8* RowPtr = Buffer.get() + (size_t)Y * RowPitch + size_t(Left) * 4;
		for (u32 X = 0; X < AreaWidth; ++X)
		{
			u8* Pixel = RowPtr + X * 4;
			u8 Temp = Pixel[0];
			Pixel[0] = Pixel[2];
			Pixel[2] = Temp;
			Pixel[3] = 0xFF;
		}

		memory_writer.w(RowPtr, AreaWidth * 4);
	}
}
