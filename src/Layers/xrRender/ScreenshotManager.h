#pragma once

#include "../../xrEngine/string_table.h"
#include "../../xrEngine/Render.h"
#include "../../xrRHI/RHI.h"

#include <DirectXTex.h>

class ScreenshotManager
{
public:
    // Save screenshot by reading pixels from current RHI render target (RTV 0)
    static bool SaveScreenshot(IRender_interface::ScreenshotMode Mode, LPCSTR Name, CMemoryWriter* MemoryWriter = nullptr);
};
