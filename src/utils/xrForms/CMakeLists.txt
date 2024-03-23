project(xrForms)
set(UTILS_FORMS_SRC_FILES)

# Files import
file(GLOB_RECURSE UTILS_FORMS_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
    "*.hpp"
)

file(GLOB_RECURSE UTILS_FORMS_RES_FILES
    "*.ico"
    "*.rc"
    "*.bmp"
)

# Source groups
source_group("core" FILES ${UTILS_FORMS_SOURCE_ALL_FILES})
source_group("res" FILES ${UTILS_FORMS_RES_FILES})

# Apply list
list(APPEND UTILS_FORMS_SRC_FILES ${UTILS_FORMS_SOURCE_ALL_FILES})
list(APPEND UTILS_FORMS_SRC_FILES ${UTILS_FORMS_RES_FILES})

# Remove unused files
#list(FILTER UTILS_FORMS_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

# xrForms project
add_executable(xrForms WIN32 ${UTILS_FORMS_SRC_FILES})
target_include_directories(xrForms PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_link_directories(xrForms PUBLIC "${IXRAY_SDK_LIB}/")
set_target_properties(xrForms PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
target_include_directories(xrForms PRIVATE "../../xrServerEntities/")

# Project defines
target_compile_definitions(xrForms PRIVATE UTILS_FORMS_EXPORTS)
target_compile_definitions(xrForms PRIVATE _USRDLL)
target_compile_definitions(xrForms PRIVATE ECORE_API=)
target_compile_definitions(xrForms PRIVATE ENGINE_API=)
target_compile_definitions(xrForms PRIVATE XRLC_LIGHT_API=)
target_compile_definitions(xrForms PRIVATE AI_COMPILER)

add_compile_options(/fp:fast)
target_compile_definitions(xrForms PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrForms PUBLIC xrCore)
target_link_libraries(xrForms PUBLIC xrAI)
target_link_libraries(xrForms PUBLIC xrLC)
target_link_libraries(xrForms PUBLIC "Winmm.lib")
target_link_libraries(xrForms PUBLIC "comctl32.lib")

# MagicFM
add_custom_command(TARGET ${PROJECT_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXRAY_SDK_BIN}MagicFM.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)

# NuGet
set(FREEIMAGE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.FreeImage.WinMerge.3.18.0.20230819)
set(WXWIDGETS ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.WxWidgets.Vcpkg.3.2.4.1)

add_custom_command(TARGET ${PROJECT_NAME}
    PRE_BUILD
    COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
)

if(NOT "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
    add_custom_command(TARGET ${PROJECT_NAME}
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${FREEIMAGE}/native/bin/x86/Release/FreeImage.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/

        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/jpeg62.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/libexpat.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/liblzma.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/libpng16.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/pcre2-8.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/pcre2-16.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/pcre2-32.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/pcre2-posix.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/tiff.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/turbojpeg.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxbase32u_net_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxbase32u_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxbase32u_xml_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxmsw32u_adv_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxmsw32u_aui_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxmsw32u_core_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxmsw32u_gl_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxmsw32u_html_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxmsw32u_media_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxmsw32u_propgrid_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxmsw32u_qa_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxmsw32u_ribbon_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxmsw32u_richtext_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxmsw32u_stc_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/wxmsw32u_xrc_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/Release/zlib1.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/

        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x86/wxrc.exe ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    )
else()
    add_custom_command(TARGET ${PROJECT_NAME}
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${FREEIMAGE}/native/bin/x64/Release/FreeImage.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/

        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/jpeg62.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/libexpat.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/liblzma.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/libpng16.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/pcre2-8.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/pcre2-16.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/pcre2-32.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/pcre2-posix.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/tiff.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/turbojpeg.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxbase32u_net_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxbase32u_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxbase32u_xml_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxmsw32u_adv_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxmsw32u_aui_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxmsw32u_core_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxmsw32u_gl_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxmsw32u_html_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxmsw32u_media_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxmsw32u_propgrid_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxmsw32u_qa_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxmsw32u_ribbon_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxmsw32u_richtext_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxmsw32u_stc_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/wxmsw32u_xrc_vc_custom.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/Release/zlib1.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/

        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${WXWIDGETS}/native/bin/x64/wxrc.exe ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    )
endif()
target_include_directories(xrForms PUBLIC "${WXWIDGETS}/native/include/")

if(NOT "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/GlU32.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/jpeg.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/libexpat.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/libpng16.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/lzma.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/nanosvg.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/nanosvgrast.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/OpenGL32.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/pcre2-8.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/pcre2-16.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/pcre2-32.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/pcre2-posix.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/tiff.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/turbojpeg.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxbase32u.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxbase32u_net.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxbase32u_xml.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxmsw32u_adv.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxmsw32u_aui.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxmsw32u_core.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxmsw32u_gl.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxmsw32u_html.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxmsw32u_media.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxmsw32u_propgrid.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxmsw32u_qa.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxmsw32u_ribbon.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxmsw32u_richtext.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxmsw32u_stc.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/wxmsw32u_xrc.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x86/Release/zlib.lib)
else()
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/GlU32.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/jpeg.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/libexpat.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/libpng16.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/lzma.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/nanosvg.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/nanosvgrast.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/OpenGL32.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/pcre2-8.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/pcre2-16.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/pcre2-32.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/pcre2-posix.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/tiff.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/turbojpeg.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxbase32u.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxbase32u_net.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxbase32u_xml.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxmsw32u_adv.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxmsw32u_aui.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxmsw32u_core.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxmsw32u_gl.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxmsw32u_html.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxmsw32u_media.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxmsw32u_propgrid.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxmsw32u_qa.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxmsw32u_ribbon.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxmsw32u_richtext.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxmsw32u_stc.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/wxmsw32u_xrc.lib)
    target_link_libraries(xrForms PUBLIC ${WXWIDGETS}native/lib/x64/Release/zlib.lib)
endif()
