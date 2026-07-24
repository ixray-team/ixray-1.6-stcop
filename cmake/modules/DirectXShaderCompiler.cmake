include(FetchContent)

FetchContent_Declare(DirectXShaderCompiler
    URL "https://github.com/microsoft/DirectXShaderCompiler/releases/download/v1.10.2605.24/dxc_preview_2026_05_22.zip"
)
FetchContent_MakeAvailable(DirectXShaderCompiler)

add_library(DirectX::ShaderCompiler SHARED IMPORTED)

set_target_properties(DirectX::ShaderCompiler PROPERTIES
    IMPORTED_LOCATION "${directxshadercompiler_SOURCE_DIR}/bin/x64/dxcompiler.dll"
    IMPORTED_IMPLIB "${directxshadercompiler_SOURCE_DIR}/lib/x64/dxcompiler.lib"
    INTERFACE_INCLUDE_DIRECTORIES "${directxshadercompiler_SOURCE_DIR}/inc"
)
set(IXRAY_DXIL_RUNTIME "${directxshadercompiler_SOURCE_DIR}/bin/x64/dxil.dll")

function(ixray_copy_dxc_runtime TARGET_NAME)
    if(WIN32)
        add_custom_command(TARGET ${TARGET_NAME}
            POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                "$<TARGET_FILE:DirectX::ShaderCompiler>" "$<TARGET_FILE_DIR:${TARGET_NAME}>"
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                "${IXRAY_DXIL_RUNTIME}" "$<TARGET_FILE_DIR:${TARGET_NAME}>"
        )
    endif()
endfunction()
