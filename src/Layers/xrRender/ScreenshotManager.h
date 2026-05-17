#pragma once

#include "../../xrEngine/string_table.h"
#include "../../xrEngine/Render.h"
#include "../../xrRHI/RHI.h"

#ifdef IXR_WINDOWS
#include <DirectXTex.h>
#endif

class ScreenshotManager
{
public:
    // Save screenshot by reading pixels from current RHI render target (RTV 0)
    static bool SaveScreenshot(IRender_interface::ScreenshotMode Mode, const char* Name, CMemoryWriter* MemoryWriter = nullptr);
};
