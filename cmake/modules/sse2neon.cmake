# sse2neon — трансляция SSE-интринсиков в NEON для ARM64.
# Нужен xrCore и рендерерам на Apple Silicon / Linux ARM64: движок использует SSE-интринсики,
# и на ARM они приходят через этот заголовок (на Linux-ARM64 — тот же путь).
# Раскладка: <sse2neon/sse2neon.h>, чтобы совпадать с инклюдом в PlatformInit.h.
download_single_header(
    "https://raw.githubusercontent.com/DLTcollab/sse2neon/master/sse2neon.h"
    "sse2neon/sse2neon.h"
)
