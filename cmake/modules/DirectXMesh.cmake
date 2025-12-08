# DirectXMesh
set(DXMESH_INCLUDE "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_2019.2024.6.5.1/include/")
set(DXMESH_LIB     "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_2019.2024.6.5.1/native/lib/x64/Release/DirectXMesh.lib")

add_imported_lib(DirectX::Mesh "${DXMESH_INCLUDE}" "${DXMESH_LIB}" "")