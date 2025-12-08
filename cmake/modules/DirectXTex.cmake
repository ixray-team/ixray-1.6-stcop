# DirectXMesh
set(DXTEX_INCLUDE "${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2024.6.5.1/include/")
set(DXTEX_LIB     "${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2024.6.5.1/native/lib/x64/Release/DirectXTex.lib")

add_imported_lib(DirectX::Tex "${DXTEX_INCLUDE}" "${DXTEX_LIB}" "")