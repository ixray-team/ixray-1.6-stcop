download_single_header(
    "https://raw.githubusercontent.com/nothings/stb/refs/heads/master/stb_image.h"
    "stb/stb_image.h"
)

# AI: the write half of stb is used by the DXVK Native hosts (the screenshot writer in
# AI: xrRender/ScreenshotManager.cpp), so macOS needs the header downloaded as well.
if (LINUX OR APPLE)
    download_single_header(
        "https://raw.githubusercontent.com/nothings/stb/refs/heads/master/stb_image_write.h"
        "stb/stb_image_write.h"
    )
endif()