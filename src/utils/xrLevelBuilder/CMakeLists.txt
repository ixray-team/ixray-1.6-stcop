project(xrLevelBuilder)

set(XLB_PACKAGES "${CMAKE_BINARY_DIR}/packages")
set(XLB_EMBREE "${XLB_PACKAGES}/ImeSense.Packages.Embree.Runtimes.win-x64.4.3.3")

# Files import: UI front-end
file(GLOB UTILS_FORMS_SOURCE_ROOT_FILES CONFIGURE_DEPENDS "*.cpp" "*.h")
file(GLOB_RECURSE UTILS_FORMS_SOURCE_FILES CONFIGURE_DEPENDS
    "Common/*.cpp" "Common/*.h"
    "Startup/*.cpp" "Startup/*.h"
    "UI/*.cpp" "UI/*.h" "UI/*.hpp"
    "Resources/*.h" "Resources/*.ico" "Resources/*.rc" "Resources/*.bmp"
)
list(APPEND UTILS_FORMS_SOURCE_FILES ${UTILS_FORMS_SOURCE_ROOT_FILES})

# Files import: xrLC (geometry and light compiler)
file(GLOB_RECURSE XLC_SOURCE_FILES CONFIGURE_DEPENDS "xrLC/*.cpp" "xrLC/*.h")
file(GLOB_RECURSE XLC_SOURCE_EDITOR_PUBLIC_FILES CONFIGURE_DEPENDS
    "../../Editors/Public/VIMP_Processor.*"
    "../../Editors/Public/object_sliding.*"
)
file(GLOB XLC_SOURCE_CUDA_FILES "${CMAKE_CURRENT_SOURCE_DIR}/xrLC/Light/CUDA/*.cu" "${CMAKE_CURRENT_SOURCE_DIR}/xrLC/Light/CUDA/*.cuh")

list(FILTER XLC_SOURCE_FILES EXCLUDE REGEX "xrGameMaterials.h")
list(FILTER XLC_SOURCE_FILES EXCLUDE REGEX "std_classes.h")
list(FILTER XLC_SOURCE_FILES EXCLUDE REGEX "cl_collector.*")

# Files import: xrAI (AI map, graph and spawn compiler)
file(GLOB_RECURSE XAI_SOURCE_FILES CONFIGURE_DEPENDS "xrAI/*.cpp" "xrAI/*.h")
file(GLOB XAI_SOURCE_ENTT_FILES
    "../../xrServerEntities/alife_human_brain.cpp"
    "../../xrServerEntities/alife_monster_brain.cpp"
    "../../xrServerEntities/alife_space.cpp"
    "../../xrServerEntities/PHNetState.cpp"
    "../../xrServerEntities/PHSynchronize.cpp"
    "../../xrServerEntities/smart_cast.cpp"
    "../../xrServerEntities/smart_cast_stats.cpp"
    "../../xrServerEntities/xrServer_Objects.cpp"
    "../../xrServerEntities/xrServer_Objects_Abstract.cpp"
    "../../xrServerEntities/xrServer_Objects_ALife.cpp"
    "../../xrServerEntities/xrServer_Objects_ALife_Items.cpp"
    "../../xrServerEntities/xrServer_Objects_ALife_Monsters.cpp"
    "../../xrServerEntities/xrServer_Object_Base.cpp"
    "../../xrServerEntities/xrServer_Space.cpp"
    "../../xrServerEntities/server_entity_wrapper.h"
    "../../xrServerEntities/server_entity_wrapper.cpp"
    "../../xrServerEntities/server_entity_wrapper_inline.h"
    "../../xrGame/game_level_cross_table.cpp"
)

# Check CUDA + OptiX
set(OPTIX_AVAILABLE OFF)

if (IXRAY_UTILS_OPTIX)
    include("${CMAKE_SOURCE_DIR}/cmake/modules/NvidiaOptix.cmake")
    PrepareNvidiaOptix()
    message(STATUS "CUDA enabled: ${CUDAToolkit_VERSION}")
    message(STATUS "OptiX enabled: ${OptiX_VERSION}")
    set(OPTIX_AVAILABLE ON)
else()
    message(STATUS "OptiX manually disabled")
endif()

if(OPTIX_AVAILABLE)
    list(APPEND XLC_SOURCE_FILES ${XLC_SOURCE_CUDA_FILES})
    set_source_files_properties(${XLC_SOURCE_CUDA_FILES} PROPERTIES HEADER_FILE_ONLY TRUE)
else()
    list(FILTER XLC_SOURCE_FILES EXCLUDE REGEX "xrDeflectorLight_Packed.*")
    list(FILTER XLC_SOURCE_FILES EXCLUDE REGEX "xrDeflector_GPU.*")
    list(FILTER XLC_SOURCE_FILES EXCLUDE REGEX "/CUDA/.*")
endif()

# Check Vulkan ray query
set(VULKAN_RT_AVAILABLE OFF)

if (IXRAY_UTILS_VULKAN)
    find_package(Vulkan COMPONENTS glslangValidator)

    if (Vulkan_FOUND AND Vulkan_GLSLANG_VALIDATOR_EXECUTABLE)
        message(STATUS "Vulkan ray query enabled: ${Vulkan_VERSION}")
        set(VULKAN_RT_AVAILABLE ON)
    else()
        message(WARNING "Vulkan requested but the Vulkan SDK (with glslangValidator) was not found!")
    endif()
endif()

set(XLC_VULKAN_SPIRV "${CMAKE_CURRENT_BINARY_DIR}/generated/VkTrace.spv.h")
if (VULKAN_RT_AVAILABLE)
    add_custom_command(
        OUTPUT "${XLC_VULKAN_SPIRV}"
        COMMAND ${Vulkan_GLSLANG_VALIDATOR_EXECUTABLE}
            -V --target-env vulkan1.2
            --vn VkTraceSpv
            -o "${XLC_VULKAN_SPIRV}"
            "${CMAKE_CURRENT_SOURCE_DIR}/xrLC/Light/Vulkan/VkTrace.comp"
        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/xrLC/Light/Vulkan/VkTrace.comp"
        COMMENT "Compiling VkTrace.comp to SPIR-V"
        VERBATIM
    )
    list(APPEND XLC_SOURCE_FILES "${CMAKE_CURRENT_SOURCE_DIR}/xrLC/Light/Vulkan/VkTrace.comp")
    set_source_files_properties("${CMAKE_CURRENT_SOURCE_DIR}/xrLC/Light/Vulkan/VkTrace.comp" PROPERTIES HEADER_FILE_ONLY TRUE)
else()
    list(FILTER XLC_SOURCE_FILES EXCLUDE REGEX "/Vulkan/.*")
endif()

# Shared GPU lighting (any GPU tracer)
if (NOT OPTIX_AVAILABLE AND NOT VULKAN_RT_AVAILABLE)
    list(FILTER XLC_SOURCE_FILES EXCLUDE REGEX "/Light/GPU/.*")
endif()

# Source groups
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}" FILES ${UTILS_FORMS_SOURCE_FILES} ${XLC_SOURCE_FILES} ${XAI_SOURCE_FILES})
source_group("xrLC/Editor" FILES ${XLC_SOURCE_EDITOR_PUBLIC_FILES})
source_group("xrAI/Entities" FILES ${XAI_SOURCE_ENTT_FILES})

list(APPEND XLC_SOURCE_FILES ${XLC_SOURCE_EDITOR_PUBLIC_FILES})
list(APPEND XAI_SOURCE_FILES ${XAI_SOURCE_ENTT_FILES})

# xrLevelBuilder project
add_executable(xrLevelBuilder WIN32 ${UTILS_FORMS_SOURCE_FILES} ${XLC_SOURCE_FILES} ${XAI_SOURCE_FILES})
target_include_directories(xrLevelBuilder PRIVATE
    "${CMAKE_CURRENT_SOURCE_DIR}"
    "${CMAKE_CURRENT_SOURCE_DIR}/../../xrCore/"
    "${CMAKE_CURRENT_SOURCE_DIR}/../../xrEngine/"
    "${CMAKE_CURRENT_SOURCE_DIR}/../../xrServerEntities/"
    "${IXRAY_SDK_INC}"
    "${XLB_PACKAGES}/directxmesh_desktop_2019.2024.6.5.1/include/"
    "${XLB_PACKAGES}/ImeSense.Packages.Zlib.1.2.13.1/native/include/"
    "${XLB_EMBREE}/build/native/include/"
)
target_link_directories(xrLevelBuilder PRIVATE "${IXRAY_SDK_LIB}/")
target_compile_definitions(xrLevelBuilder PRIVATE AI_COMPILER)

# Each part keeps its own defines and precompiled header
set_source_files_properties(${UTILS_FORMS_SOURCE_FILES} PROPERTIES
    COMPILE_DEFINITIONS "UTILS_FORMS_EXPORTS;ECORE_API=;ENGINE_API=;XRLC_LIGHT_API="
)
set_source_files_properties(${XLC_SOURCE_FILES} PROPERTIES
    COMPILE_DEFINITIONS "_USRDLL;LCLIGHT_BUILD"
    INCLUDE_DIRECTORIES "${CMAKE_CURRENT_SOURCE_DIR}/xrLC"
)
set_source_files_properties(${XAI_SOURCE_FILES} PROPERTIES
    COMPILE_DEFINITIONS "dSINGLE;$<$<CONFIG:Release>:LUABIND_NO_ERROR_CHECKING>"
    INCLUDE_DIRECTORIES "${CMAKE_CURRENT_SOURCE_DIR}/xrAI"
)

function(xlb_precompiled_header Name Header Creator)
    set(Sources ${ARGN})
    list(FILTER Sources INCLUDE REGEX "\\.cpp$")
    set(Pch "${CMAKE_CURRENT_BINARY_DIR}/${Name}.$<CONFIG>.pch")
    set_source_files_properties(${Sources} PROPERTIES COMPILE_OPTIONS "/Yu${Header};/Fp${Pch};/FI${Header}")
    set_source_files_properties(${Creator} PROPERTIES COMPILE_OPTIONS "/Yc${Header};/Fp${Pch};/FI${Header}")
endfunction()

if (MSVC)
    xlb_precompiled_header(xrLC "${CMAKE_CURRENT_SOURCE_DIR}/xrLC/StdAfx.h" "${CMAKE_CURRENT_SOURCE_DIR}/xrLC/StdAfx.cpp" ${XLC_SOURCE_FILES})
    xlb_precompiled_header(xrAI "${CMAKE_CURRENT_SOURCE_DIR}/xrAI/StdAfx.h" "${CMAKE_CURRENT_SOURCE_DIR}/xrAI/StdAfx.cpp" ${XAI_SOURCE_FILES})
endif()

# Apply GPU tracers
if (OPTIX_AVAILABLE OR VULKAN_RT_AVAILABLE)
    target_compile_definitions(xrLevelBuilder PRIVATE LCGPU_BUILD)
endif()

if (VULKAN_RT_AVAILABLE)
    target_sources(xrLevelBuilder PRIVATE "${XLC_VULKAN_SPIRV}")
    source_group("xrLC/Light/Vulkan" FILES "${XLC_VULKAN_SPIRV}")
    target_compile_definitions(xrLevelBuilder PRIVATE LCVULKAN_BUILD)
    target_include_directories(xrLevelBuilder PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/generated")
    target_link_libraries(xrLevelBuilder PRIVATE Vulkan::Vulkan)
endif()

# Apply CUDA/OptiX
if(OPTIX_AVAILABLE)
    target_compile_definitions(xrLevelBuilder PRIVATE LCCUDA_BUILD)
    target_include_directories(xrLevelBuilder PRIVATE ${OptiX_INCLUDE_DIRS})
    # Generate an import-only library: newer SDK cuda.lib files also contain driver wrapper code.
    set(XlcCudaDriverLibrary "${CMAKE_CURRENT_BINARY_DIR}/generated/$<CONFIG>/NvidiaDriver.lib")
    add_custom_command(
        OUTPUT "${XlcCudaDriverLibrary}"
        COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_CURRENT_BINARY_DIR}/generated/$<CONFIG>"
        COMMAND "${CMAKE_AR}" /nologo /machine:x64
            "/def:${CMAKE_CURRENT_SOURCE_DIR}/xrLC/Light/CUDA/NvidiaDriver.def"
            "/out:${XlcCudaDriverLibrary}"
        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/xrLC/Light/CUDA/NvidiaDriver.def"
        COMMENT "Generating NVIDIA driver import library"
        VERBATIM
    )
    target_link_libraries(xrLevelBuilder PRIVATE CUDA::cudart_static CUDA::nvml "${XlcCudaDriverLibrary}")

    # nvcuda.dll and nvml.dll ship only with the NVIDIA driver: load them lazily so the tool starts on AMD/Intel
    if (MSVC)
        target_link_options(xrLevelBuilder PRIVATE "/DELAYLOAD:nvcuda.dll" "/DELAYLOAD:nvml.dll")
        target_link_libraries(xrLevelBuilder PRIVATE delayimp.lib)
    endif()

    set(XlcPtx "${CMAKE_CURRENT_BINARY_DIR}/generated/$<CONFIG>/CuTrace.ptx")
    add_custom_command(
        OUTPUT "${XlcPtx}"
        COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_CURRENT_BINARY_DIR}/generated/$<CONFIG>"
        COMMAND "${CUDAToolkit_NVCC_EXECUTABLE}"
            --ptx
            "${CMAKE_CURRENT_SOURCE_DIR}/xrLC/Light/CUDA/CuTrace.cu"
            -o "${XlcPtx}"
            --compiler-bindir "${CMAKE_CXX_COMPILER}"
            --std=c++17 --machine=64
            "-I${OptiX_INCLUDE_DIRS}"
            --gpu-architecture=compute_75
        DEPENDS ${XLC_SOURCE_CUDA_FILES} ${IXRAY_OPTIX_HEADERS} "${CUDAToolkit_NVCC_EXECUTABLE}"
        COMMENT "Compiling CuTrace.cu to PTX"
        VERBATIM
    )
    set(XlcPublishedPtx "${IXRAY_OUTPUT}/CuTrace.ptx")
    set(XlcNvidiaNotice "${IXRAY_OUTPUT}/licenses/NVIDIA/README.txt")
    file(GLOB XlcNvidiaLicenseFiles CONFIGURE_DEPENDS "${IXRAY_NVIDIA_LICENSE_DIR}/*")
    add_custom_command(
        OUTPUT "${XlcPublishedPtx}" "${XlcNvidiaNotice}"
        COMMAND ${CMAKE_COMMAND} -E make_directory "${IXRAY_OUTPUT}"
        COMMAND ${CMAKE_COMMAND} -E copy_if_different "${XlcPtx}" "${XlcPublishedPtx}"
        COMMAND ${CMAKE_COMMAND} -E rm -rf "${IXRAY_OUTPUT}/licenses/NVIDIA"
        COMMAND ${CMAKE_COMMAND} -E copy_directory "${IXRAY_NVIDIA_LICENSE_DIR}" "${IXRAY_OUTPUT}/licenses/NVIDIA"
        DEPENDS "${XlcPtx}" ${XlcNvidiaLicenseFiles}
        COMMENT "Copying NVIDIA runtime artifacts"
        VERBATIM
    )
    set(XlcNvidiaArtifacts "${XlcCudaDriverLibrary}" "${XlcPtx}" "${XlcPublishedPtx}" "${XlcNvidiaNotice}")
    set_source_files_properties(${XlcNvidiaArtifacts} PROPERTIES HEADER_FILE_ONLY TRUE)
    target_sources(xrLevelBuilder PRIVATE ${XlcNvidiaArtifacts})
    source_group("xrLC/Light/CUDA" FILES ${XlcNvidiaArtifacts})
endif()

# Linker list
target_link_libraries(xrLevelBuilder PRIVATE
    xrCore
    xrEngineCore
    xrSE_Factory
    xrDXT
    luabind
    imgui
    MagicFM
    NvMender2003
    NvTriStrip
    "rpcrt4.lib"
    "${XLB_PACKAGES}/ImeSense.Packages.Zlib.1.2.13.1/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/zlib.lib"
    "${XLB_EMBREE}/runtimes/win-x64/native/Release/embree4.lib"
)

if (WIN32)
    target_link_libraries(xrLevelBuilder PRIVATE DirectX::Mesh)
    target_link_libraries(xrLevelBuilder PRIVATE WindowsSDK::WinMM)
endif()

add_custom_command(TARGET xrLevelBuilder POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${XLB_EMBREE}/runtimes/win-x64/native/Release/embree4.dll" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/"
)
