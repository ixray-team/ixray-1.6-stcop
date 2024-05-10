project(ParticlesEditor)

# Files import
file(GLOB_RECURSE EPE_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.*"
)

set(EPE_SRC_FILES)

source_group("src" FILES ${EPE_SOURCE_ALL_FILES})

# Apply list
list(APPEND EPE_SRC_FILES ${EPE_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER EPE_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# ParticlesEditor project
add_executable(ParticlesEditor WIN32 ${EPE_SRC_FILES})
target_include_directories(ParticlesEditor PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(ParticlesEditor PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(ParticlesEditor PRIVATE _PARTICLE_EDITOR)
target_compile_definitions(ParticlesEditor PRIVATE _WINDOWS)
target_compile_definitions(ParticlesEditor PRIVATE _USRDLL)
target_compile_definitions(ParticlesEditor PUBLIC XR_EDITOR_NEW)

add_compile_options(/fp:fast)
target_compile_options(ParticlesEditor PRIVATE /W0)
target_compile_definitions(ParticlesEditor PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(ParticlesEditor PUBLIC xrCore)
target_link_libraries(ParticlesEditor PUBLIC xrEUI)
target_link_libraries(ParticlesEditor PUBLIC xrEProps)
target_link_libraries(ParticlesEditor PUBLIC xrParticles)
target_link_libraries(ParticlesEditor PUBLIC xrECore)

# Move precompiled header
set_target_properties(ParticlesEditor PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

file(GLOB_RECURSE EPE_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${EPE_SOURCE_PCH_FILES})

target_include_directories(ParticlesEditor PUBLIC "${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/include/")
target_include_directories(ParticlesEditor PUBLIC "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_win10.2023.4.28.1/include/")