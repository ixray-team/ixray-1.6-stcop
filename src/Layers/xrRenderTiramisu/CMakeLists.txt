project(xrRenderTiramisu)
set(XRDS0_SRC_FILES)

# Files import
file(GLOB_RECURSE XRDSR0_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)

# Source groups
source_group("render/src" FILES ${XRDSR0_SOURCE_ALL_FILES})

# Apply list
list(APPEND XRDS0_SRC_FILES ${XRDSR0_SOURCE_ALL_FILES})
list(FILTER XRDS0_SRC_FILES EXCLUDE REGEX "/tests/")
# Editor adapter sources are injected only by the editor build after xrEUI and
# xrTiramisuMaterialEditorCore targets exist.
list(FILTER XRDS0_SRC_FILES EXCLUDE REGEX "/Editor/")

# xrRenderTiramisu project
add_library(xrRenderTiramisu SHARED ${XRDS0_SRC_FILES})
target_precompile_headers(xrRenderTiramisu PRIVATE "stdafx.h")

# Include directories
target_include_directories(xrRenderTiramisu PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# Project defines
target_compile_definitions(xrRenderTiramisu PRIVATE xrRenderTiramisu_EXPORTS)
target_compile_definitions(xrRenderTiramisu PRIVATE _USRDLL)
target_link_libraries(xrRenderTiramisu PRIVATE xrTiramisuMaterialCore)
target_link_libraries(xrRenderTiramisu PUBLIC RedImage::RedImage)

# Linker list
target_link_libraries(xrRenderTiramisu PRIVATE xrCore)
target_link_libraries(xrRenderTiramisu PRIVATE xrEngineCore)

## RenderDoc
target_include_directories(xrRenderTiramisu PUBLIC "${RENDERDOC_API}")

target_link_libraries(xrRenderTiramisu PRIVATE DirectX::ShaderCompiler)
target_link_libraries(xrRenderTiramisu PRIVATE NRI)
target_compile_definitions(xrRenderTiramisu PRIVATE IXR_ENABLE_SHADER_COMPILER)
ixray_copy_dxc_runtime(xrRenderTiramisu)

if (IXRAY_BUILD_TESTS)
    add_executable(xrTiramisuAdapterSelectionTests
        Core/TiramisuRenderAdapterSelection.cpp
        Core/tests/TiramisuRenderAdapterSelectionTests.cpp
    )
    target_include_directories(xrTiramisuAdapterSelectionTests PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
    target_compile_features(xrTiramisuAdapterSelectionTests PRIVATE cxx_std_23)
    target_link_libraries(xrTiramisuAdapterSelectionTests PRIVATE xrCore)
    add_test(NAME xrTiramisuAdapterSelectionTests COMMAND xrTiramisuAdapterSelectionTests -rdbg)
    set_tests_properties(xrTiramisuAdapterSelectionTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrTiramisuAdapterSelectionTests PROPERTIES FOLDER "Tests")

    add_executable(xrTiramisuStaticMeshTypesTests
        Scene/StaticMesh/TStaticMeshTypes.cpp
        Scene/StaticMesh/tests/TStaticMeshTypesTests.cpp
    )
    target_include_directories(xrTiramisuStaticMeshTypesTests PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
    target_compile_features(xrTiramisuStaticMeshTypesTests PRIVATE cxx_std_23)
    target_link_libraries(xrTiramisuStaticMeshTypesTests PRIVATE xrCore)
    add_test(NAME xrTiramisuStaticMeshTypesTests COMMAND xrTiramisuStaticMeshTypesTests -rdbg)
    set_tests_properties(xrTiramisuStaticMeshTypesTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrTiramisuStaticMeshTypesTests PROPERTIES FOLDER "Tests")

    add_executable(xrTiramisuThreadAffinityTests
        Core/tests/TThreadAffinityTests.cpp
    )
    target_include_directories(xrTiramisuThreadAffinityTests PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
    target_compile_features(xrTiramisuThreadAffinityTests PRIVATE cxx_std_23)
    target_link_libraries(xrTiramisuThreadAffinityTests PRIVATE xrCore)
    add_test(NAME xrTiramisuThreadAffinityTests COMMAND xrTiramisuThreadAffinityTests -rdbg)
    set_tests_properties(xrTiramisuThreadAffinityTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrTiramisuThreadAffinityTests PROPERTIES FOLDER "Tests")

    add_executable(xrTiramisuRenderCommandQueueTests
        RenderCommandQueueCore.cpp
        Core/tests/TiramisuRenderCommandQueueTests.cpp
    )
    target_include_directories(xrTiramisuRenderCommandQueueTests PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
    target_compile_features(xrTiramisuRenderCommandQueueTests PRIVATE cxx_std_23)
    target_link_libraries(xrTiramisuRenderCommandQueueTests PRIVATE xrCore)
    add_test(NAME xrTiramisuRenderCommandQueueTests COMMAND xrTiramisuRenderCommandQueueTests -rdbg)
    set_tests_properties(xrTiramisuRenderCommandQueueTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrTiramisuRenderCommandQueueTests PROPERTIES FOLDER "Tests")

    add_executable(xrTiramisuDeferredDeletionTests
        Core/TiramisuDeferredDeletionQueue.cpp
        Core/tests/TiramisuDeferredDeletionQueueTests.cpp
    )
    target_include_directories(xrTiramisuDeferredDeletionTests PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
    target_compile_features(xrTiramisuDeferredDeletionTests PRIVATE cxx_std_23)
    target_link_libraries(xrTiramisuDeferredDeletionTests PRIVATE xrCore)
    add_test(NAME xrTiramisuDeferredDeletionTests COMMAND xrTiramisuDeferredDeletionTests -rdbg)
    set_tests_properties(xrTiramisuDeferredDeletionTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrTiramisuDeferredDeletionTests PROPERTIES FOLDER "Tests")

    add_executable(xrTiramisuRenderGraphTests
        Core/RenderGraph/TiramisuRenderGraph.cpp
        Core/RenderGraph/TiramisuRenderGraphNri.cpp
        Core/RenderGraph/tests/TiramisuRenderGraphTests.cpp
    )
    target_include_directories(xrTiramisuRenderGraphTests PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
    target_compile_features(xrTiramisuRenderGraphTests PRIVATE cxx_std_23)
    target_link_libraries(xrTiramisuRenderGraphTests PRIVATE xrCore NRI)
    add_test(NAME xrTiramisuRenderGraphTests COMMAND xrTiramisuRenderGraphTests -rdbg)
    set_tests_properties(xrTiramisuRenderGraphTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrTiramisuRenderGraphTests PROPERTIES FOLDER "Tests")

    add_executable(xrTiramisuRenderGraphExecutorTests
        Core/RenderGraph/TiramisuRenderGraph.cpp
        Core/RenderGraph/TiramisuRenderGraphNri.cpp
        Core/RenderGraph/TiramisuRenderGraphNriExecutor.cpp
        Core/RenderGraph/tests/TiramisuRenderGraphNriExecutorTests.cpp
    )
    target_include_directories(xrTiramisuRenderGraphExecutorTests PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
    target_compile_features(xrTiramisuRenderGraphExecutorTests PRIVATE cxx_std_23)
    target_link_libraries(xrTiramisuRenderGraphExecutorTests PRIVATE xrCore NRI)
    add_test(NAME xrTiramisuRenderGraphExecutorTests COMMAND xrTiramisuRenderGraphExecutorTests -rdbg)
    set_tests_properties(xrTiramisuRenderGraphExecutorTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrTiramisuRenderGraphExecutorTests PROPERTIES FOLDER "Tests")
endif()