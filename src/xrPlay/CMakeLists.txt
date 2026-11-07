project(xrEngine)
set(XPLAY_SRC_FILES)

# Files import
file(GLOB_RECURSE XPLAY_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.*"
)
file(GLOB_RECURSE XPLAY_SOURCE_CPP_FILES
    "*.cpp"
    "*.h"
)
file(GLOB_RECURSE XPLAY_SOURCE_RES_FILES
    "*.ico"
    "*.rc"
    "*.bmp"
)
file(GLOB_RECURSE XPLAY_SOURCE_UI_FILES
    "UI*.cpp"
    "UI*.h"
)

# File groups
source_group("Source Files" FILES ${XPLAY_SOURCE_CPP_FILES})
source_group("Resources" FILES ${XPLAY_SOURCE_RES_FILES})
source_group("UI" FILES ${XPLAY_SOURCE_UI_FILES})

# Apply list
list(APPEND XPLAY_SRC_FILES ${XPLAY_SOURCE_ALL_FILES})

# xrEngine project
add_executable(xrEngine WIN32 ${XPLAY_SOURCE_ALL_FILES})
target_include_directories(xrEngine PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrEngine PRIVATE "${IXRAY_SDK_INC}")
set_target_properties(xrEngine PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

# Project defines
target_compile_definitions(xrEngine PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrEngine PUBLIC xrEngineCore)
target_link_libraries(xrEngine PUBLIC xrScripts)
target_link_libraries(xrEngine PUBLIC Steamworks::Steamworks)

if(IXRAY_USE_R1)
    add_dependencies(xrEngine xrRender_R1)
   endif()

if (WIN32)
    add_dependencies(xrEngine xrGame xrRender_R4)
    if(IXRAY_USE_R2)
        add_dependencies(xrEngine xrRender_R2)
    endif()
endif()

if(IXRAY_MP)
    add_dependencies(xrEngine xrGameSpy)
endif()

if(APPLE)
    # LuaJIT is built outside the engine output directory.
    add_custom_command(TARGET xrEngine POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "$<TARGET_FILE:libluajit>" "$<TARGET_FILE_DIR:xrEngine>/libluajit.2.dylib"
    )
    foreach(DXMT_LIBRARY IN ITEMS "${DXMT_D3D11_LIBRARY}" "${DXMT_DXGI_LIBRARY}" "${DXMT_METAL_LIBRARY}")
        target_copy_dependency(xrEngine "${DXMT_LIBRARY}")
    endforeach()
    add_custom_command(TARGET xrEngine POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E make_directory "$<TARGET_FILE_DIR:xrEngine>/licenses/DXMT" "$<TARGET_FILE_DIR:xrEngine>/licenses/vkd3d" "$<TARGET_FILE_DIR:xrEngine>/licenses/LLVM" "$<TARGET_FILE_DIR:xrEngine>/licenses/LuaJIT"
        COMMAND ${CMAKE_COMMAND} -E copy_if_different "${dxmt_SOURCE_DIR}/LICENSE" "${dxmt_SOURCE_DIR}/COPYING.LIB" "${dxmt_SOURCE_DIR}/LICENSE.OLD" "$<TARGET_FILE_DIR:xrEngine>/licenses/DXMT/"
        COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_BINARY_DIR}/_deps/vkd3d/src/vkd3d_native/COPYING" "$<TARGET_FILE_DIR:xrEngine>/licenses/vkd3d/"
        COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_SOURCE_DIR}/doc/licenses/LLVM-15.0.7.txt" "$<TARGET_FILE_DIR:xrEngine>/licenses/LLVM/"
        COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_SOURCE_DIR}/doc/licenses/DirectXTex.txt" "$<TARGET_FILE_DIR:xrEngine>/licenses/DirectXTex.txt"
        COMMAND ${CMAKE_COMMAND} -E copy_if_different "${DirectXMesh_SOURCE_DIR}/LICENSE" "$<TARGET_FILE_DIR:xrEngine>/licenses/DirectXMesh.txt"
        COMMAND ${CMAKE_COMMAND} -E copy_if_different "${luajit_SOURCE_DIR}/COPYRIGHT" "$<TARGET_FILE_DIR:xrEngine>/licenses/LuaJIT/"
        COMMAND ${CMAKE_COMMAND} -E copy_if_different "${dxvk_SOURCE_DIR}/LICENSE" "$<TARGET_FILE_DIR:xrEngine>/licenses/DXVK.txt"
    )
    set_property(TARGET xrEngine APPEND PROPERTY BUILD_RPATH "@executable_path;${VULKAN_LOADER_PREFIX}/lib")
    add_dependencies(xrEngine xrGame xrRender_R4)
endif()
