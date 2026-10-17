# AI: sse2neon translates SSE intrinsics to NEON on ARM64. xrCore and the renderers need it
# AI: on Apple Silicon and Linux ARM64: the engine uses SSE intrinsics and on ARM they arrive
# AI: through this header (the same path is used on Linux ARM64).
# AI: Layout <sse2neon/sse2neon.h> matches the include in PlatformInit.h.
download_single_header(
    "https://raw.githubusercontent.com/DLTcollab/sse2neon/master/sse2neon.h"
    "sse2neon/sse2neon.h"
)
