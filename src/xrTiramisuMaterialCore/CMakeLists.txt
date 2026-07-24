project(xrTiramisuMaterialCore)

set(MATERIAL_CORE_SOURCES
    MaterialBundle.cpp
    TiramisuMaterialShaderLibrary.cpp
    MaterialGraphSchema.cpp
    MaterialAsset.cpp
    MaterialGraph.cpp
    MaterialTypes.cpp
    TiramisuMaterialShaderCompiler.cpp
    MaterialPass.cpp
    MaterialParameterLayout.cpp
    TiramisuMaterialPipelineCache.cpp
    LegacyMaterialResolver.cpp
)

set(MATERIAL_CORE_HEADERS
    TiramisuMaterialCoreTypes.h
    MaterialBundle.h
    TiramisuMaterialShaderLibrary.h
    MaterialGraphSchema.h
    MaterialAsset.h
    MaterialGraph.h
    MaterialRuntime.h
    MaterialTypes.h
    TiramisuMaterialShaderCompiler.h
    MaterialPass.h
    MaterialParameterLayout.h
    TiramisuMaterialPipelineCache.h
    LegacyMaterialResolver.h
)

add_library(xrTiramisuMaterialCore STATIC ${MATERIAL_CORE_SOURCES} ${MATERIAL_CORE_HEADERS})
target_include_directories(xrTiramisuMaterialCore
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE
        ${EXTERNAL_INCLUDE_DIR}
)
target_compile_features(xrTiramisuMaterialCore PUBLIC cxx_std_23)
# MSVC can emit corrupt type records when these frequently rebuilt static
# libraries combine LTCG, /Zi and parallel compilation. Keep /MP and the
# normal program database, but do not feed these objects through LTCG.
set_property(TARGET xrTiramisuMaterialCore PROPERTY IXRAY_DISABLE_LTCG TRUE)
target_link_libraries(xrTiramisuMaterialCore
    PUBLIC xrCore
    PRIVATE DirectX::ShaderCompiler
)
set_target_properties(xrTiramisuMaterialCore PROPERTIES POSITION_INDEPENDENT_CODE ON)

source_group("Source Files" FILES ${MATERIAL_CORE_SOURCES})
source_group("Header Files" FILES ${MATERIAL_CORE_HEADERS})

if(IXRAY_UTILS OR IXRAY_BUILD_TESTS)
    add_executable(xrMaterialCooker tools/MaterialCookerMain.cpp)
    target_link_libraries(xrMaterialCooker PRIVATE xrTiramisuMaterialCore)
    target_include_directories(xrMaterialCooker PRIVATE ${EXTERNAL_INCLUDE_DIR})
    target_compile_features(xrMaterialCooker PRIVATE cxx_std_23)
    set_target_properties(xrMaterialCooker PROPERTIES FOLDER "Utilities")
    ixray_copy_dxc_runtime(xrMaterialCooker)
endif()

if(IXRAY_BUILD_TESTS)
    function(add_xr_material_test TARGET_NAME SOURCE_FILE)
        add_executable(${TARGET_NAME} ${SOURCE_FILE})
        target_link_libraries(${TARGET_NAME} PRIVATE xrTiramisuMaterialCore)
        target_include_directories(${TARGET_NAME} PRIVATE ${EXTERNAL_INCLUDE_DIR})
        target_compile_features(${TARGET_NAME} PRIVATE cxx_std_23)
        add_test(NAME ${TARGET_NAME} COMMAND ${TARGET_NAME} -rdbg)
        set_tests_properties(${TARGET_NAME} PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
        set_target_properties(${TARGET_NAME} PROPERTIES FOLDER "Tests")
    endfunction()

    add_xr_material_test(xrRenderTiramisuTests tests/MaterialCoreTests.cpp)
    add_xr_material_test(xrMaterialAssetTests tests/MaterialAssetTests.cpp)
    add_xr_material_test(xrMaterialGraphTests tests/MaterialGraphTests.cpp)
    add_xr_material_test(xrMaterialGraphSchemaTests tests/MaterialGraphSchemaTests.cpp)
    add_xr_material_test(xrMaterialBundleTests tests/MaterialBundleTests.cpp)
    add_xr_material_test(xrMaterialShaderLibraryTests tests/MaterialShaderLibraryTests.cpp)
    add_xr_material_test(xrMaterialShaderCompilerTests tests/MaterialShaderCompilerTests.cpp)
    add_xr_material_test(xrMaterialPassCompilerTests tests/MaterialPassCompilerTests.cpp)
    add_xr_material_test(xrMaterialPipelineCacheTests tests/MaterialPipelineCacheTests.cpp)
    add_xr_material_test(xrMaterialParameterLayoutTests tests/MaterialParameterLayoutTests.cpp)
    add_xr_material_test(xrLegacyMaterialBridgeTests tests/LegacyMaterialBridgeTests.cpp)
    add_xr_material_test(xrTiramisuShaderFoundationTests tests/ShaderFoundationTests.cpp)
    ixray_copy_dxc_runtime(xrMaterialShaderCompilerTests)
    ixray_copy_dxc_runtime(xrMaterialPassCompilerTests)
    ixray_copy_dxc_runtime(xrTiramisuShaderFoundationTests)

    add_executable(xrMaterialCookerBundleTests tests/MaterialCookerBundleTests.cpp)
    target_link_libraries(xrMaterialCookerBundleTests PRIVATE xrTiramisuMaterialCore)
    target_include_directories(xrMaterialCookerBundleTests PRIVATE ${EXTERNAL_INCLUDE_DIR})
    target_compile_features(xrMaterialCookerBundleTests PRIVATE cxx_std_23)
    set_target_properties(xrMaterialCookerBundleTests PROPERTIES FOLDER "Tests")

    add_test(
        NAME xrMaterialCookerValidation
        COMMAND $<TARGET_FILE:xrMaterialCooker>
            --input ${CMAKE_SOURCE_DIR}/gamedata/render_materials
            --validate-only
            -rdbg
    )
    add_test(
        NAME xrMaterialCookerDeterminism
        COMMAND ${CMAKE_COMMAND}
            -DCOOKER=$<TARGET_FILE:xrMaterialCooker>
            -DINPUT=${CMAKE_SOURCE_DIR}/gamedata/render_materials
            -DOUTPUT_DIR=${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/$<CONFIG>
            -DTEST_DEBUG_FLAG=-rdbg
            -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/RunCookerDeterminism.cmake
    )
    add_test(
        NAME xrMaterialCookerShaderBundle
        COMMAND ${CMAKE_COMMAND}
            -DCOOKER=$<TARGET_FILE:xrMaterialCooker>
            -DINSPECTOR=$<TARGET_FILE:xrMaterialCookerBundleTests>
            -DINPUT=${CMAKE_SOURCE_DIR}/gamedata/render_materials
            -DOUTPUT=${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/$<CONFIG>/materials-shaders.xrm
            -DTEST_DEBUG_FLAG=-rdbg
            -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/RunCookerShaderBundle.cmake
    )
    add_test(
        NAME xrMaterialCookerRejectsInvalidAssets
        COMMAND $<TARGET_FILE:xrMaterialCooker>
            --input ${CMAKE_CURRENT_SOURCE_DIR}/tests/fixtures/invalid
            --validate-only
            -rdbg
    )
    set_tests_properties(
        xrMaterialCookerValidation
        xrMaterialCookerDeterminism
        xrMaterialCookerShaderBundle
        xrMaterialCookerRejectsInvalidAssets
        PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    )
    set_tests_properties(xrMaterialCookerRejectsInvalidAssets PROPERTIES WILL_FAIL TRUE)

    add_dependencies(xrRenderTiramisuTests
        xrMaterialAssetTests xrMaterialGraphTests xrMaterialGraphSchemaTests
        xrMaterialBundleTests xrMaterialShaderLibraryTests xrMaterialCooker xrMaterialCookerBundleTests)
    add_dependencies(xrRenderTiramisuTests xrMaterialShaderCompilerTests)
    add_dependencies(xrRenderTiramisuTests xrMaterialPassCompilerTests xrMaterialPipelineCacheTests)
    add_dependencies(xrRenderTiramisuTests xrMaterialParameterLayoutTests)
    add_dependencies(xrRenderTiramisuTests xrLegacyMaterialBridgeTests xrTiramisuShaderFoundationTests)
endif()
