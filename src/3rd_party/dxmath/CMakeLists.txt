add_library(dxmath INTERFACE)
add_library(DirectXMath::dxmath ALIAS dxmath)
target_include_directories(dxmath INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)