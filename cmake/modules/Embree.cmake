# Embree 4.4.1 prebuilt for Windows x64 (SSE2, TBB tasking).
# Other platforms build it from src/3rd-party/embree.
set(EMBREE_SDK_ZIP ${DEP_DIR}/Embree.4.4.1.zip)
set(EMBREE_SDK_URL https://github.com/ixray-team/ixray-packages/releases/download/d2024.5.3/Embree.4.4.1.zip)
set(EMBREE_SDK_DIR ${DEP_DIR}/embree)
download_and_extract_sdk(${EMBREE_SDK_URL} ${EMBREE_SDK_ZIP} ${EMBREE_SDK_DIR})

add_imported_lib(
    embree
    "${EMBREE_SDK_DIR}/include"
    "${EMBREE_SDK_DIR}/lib/embree4.lib"
    "${EMBREE_SDK_DIR}/bin/embree4.dll"
)
