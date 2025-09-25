project(xrRHI)
set(XRHI_SRC_FILES)

# Files import
file(GLOB_RECURSE XRHI_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.*"
)
file(GLOB_RECURSE XRHI_SOURCE_D3D11_FILES "D3D11/*.*")
file(GLOB_RECURSE XRHI_SOURCE_D3D9_FILES "D3D9/*.*")

# File groups
source_group("Core" FILES ${XRHI_SOURCE_ALL_FILES})
source_group("Interface/D3D11" FILES ${XRHI_SOURCE_D3D11_FILES})
source_group("Interface/D3D9" FILES ${XRHI_SOURCE_D3D9_FILES})

# Apply list
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_ALL_FILES})

# xrRHI project
add_library(xrRHI SHARED ${XRHI_SOURCE_ALL_FILES})
target_include_directories(xrRHI PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrRHI PRIVATE "${IXRAY_SDK_INC}")
target_compile_definitions(xrRHI PRIVATE XR_RHI_EXPORTS)
set_target_properties(xrRHI PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

# Project defines
target_compile_definitions(xrRHI PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrRHI PUBLIC xrCore)
target_link_libraries(xrRHI PUBLIC "d3d11.lib")
target_link_libraries(xrRHI PUBLIC "d3d9.lib")