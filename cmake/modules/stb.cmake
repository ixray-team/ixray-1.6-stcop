download_single_header(
    "https://raw.githubusercontent.com/nothings/stb/refs/heads/master/stb_image.h"
    "stb/stb_image.h"
)

if (NOT WIN32)
    download_single_header(
        "https://raw.githubusercontent.com/nothings/stb/refs/heads/master/stb_image_write.h"
        "stb/stb_image_write.h"
    )
endif()