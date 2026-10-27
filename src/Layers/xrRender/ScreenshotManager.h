#pragma once

#include "../../xrEngine/string_table.h"
#include "../../xrEngine/Render.h"
#include "../../xrRHI/RHI.h"

#include <DirectXTex.h>

class ScreenshotManager
{
private:
	static bool HasPendingScreenshot;

public:
	static void RequestScreenshot();
	static void CapturePendingScreenshot();
	static void Flush();
	static void Shutdown();

	// Game captures use the game target; cubemaps and level maps use RTV 0.
	static bool SaveScreenshot(IRender_interface::ScreenshotMode Mode, const char* Name, CMemoryWriter* MemoryWriter = nullptr);
};
