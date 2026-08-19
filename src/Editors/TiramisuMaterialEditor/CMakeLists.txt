project(xrTiramisuMaterialEditorCore)

add_library(xrTiramisuMaterialEditorCore STATIC
    TiramisuMaterialEditorTypes.h
    EditorViewportMaterialResolver.cpp
    EditorViewportMaterialResolver.h
    MaterialDependencyWatcher.cpp
    MaterialDependencyWatcher.h
    MaterialEditorDocument.cpp
    MaterialEditorDocument.h
    MaterialEditorAssetRouting.cpp
    MaterialEditorAssetRouting.h
    MaterialEditorFileIO.cpp
    MaterialEditorFileIO.h
    MaterialInstanceEditorDocument.cpp
    MaterialInstanceEditorDocument.h
    MaterialInstanceParentResolver.cpp
    MaterialInstanceParentResolver.h
    LegacyObjectMaterialMigration.cpp
    LegacyObjectMaterialMigration.h
    MaterialPreviewCompiler.cpp
    MaterialPreviewCompiler.h
    MaterialPreviewAssets.cpp
    MaterialPreviewAssets.h
)
target_include_directories(xrTiramisuMaterialEditorCore
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE ${EXTERNAL_INCLUDE_DIR})
target_compile_features(xrTiramisuMaterialEditorCore PUBLIC cxx_std_23)
set_property(TARGET xrTiramisuMaterialEditorCore PROPERTY IXRAY_DISABLE_LTCG TRUE)
target_link_libraries(xrTiramisuMaterialEditorCore PUBLIC xrTiramisuMaterialCore)
set_target_properties(xrTiramisuMaterialEditorCore PROPERTIES FOLDER "Editors/Core")

if(IXRAY_BUILD_TESTS)
    add_executable(xrMaterialEditorDocumentTests tests/MaterialEditorDocumentTests.cpp)
    target_link_libraries(xrMaterialEditorDocumentTests PRIVATE xrTiramisuMaterialEditorCore)
    target_include_directories(xrMaterialEditorDocumentTests PRIVATE
        ${CMAKE_SOURCE_DIR}/src/xrTiramisuMaterialCore/tests)
    target_compile_features(xrMaterialEditorDocumentTests PRIVATE cxx_std_23)
    add_test(NAME xrMaterialEditorDocumentTests COMMAND xrMaterialEditorDocumentTests -rdbg)
    set_tests_properties(xrMaterialEditorDocumentTests PROPERTIES
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrMaterialEditorDocumentTests PROPERTIES FOLDER "Tests")

    add_executable(xrMaterialEditorAssetRoutingTests
        tests/MaterialEditorAssetRoutingTests.cpp)
    target_link_libraries(xrMaterialEditorAssetRoutingTests PRIVATE
        xrTiramisuMaterialEditorCore)
    target_include_directories(xrMaterialEditorAssetRoutingTests PRIVATE
        ${CMAKE_SOURCE_DIR}/src/xrTiramisuMaterialCore/tests)
    target_compile_features(xrMaterialEditorAssetRoutingTests PRIVATE cxx_std_23)
    add_test(NAME xrMaterialEditorAssetRoutingTests
        COMMAND xrMaterialEditorAssetRoutingTests -rdbg)
    set_tests_properties(xrMaterialEditorAssetRoutingTests PROPERTIES
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrMaterialEditorAssetRoutingTests PROPERTIES
        FOLDER "Tests")

    add_executable(xrMaterialInstanceEditorDocumentTests
        tests/MaterialInstanceEditorDocumentTests.cpp)
    target_link_libraries(xrMaterialInstanceEditorDocumentTests PRIVATE xrTiramisuMaterialEditorCore)
    target_include_directories(xrMaterialInstanceEditorDocumentTests PRIVATE
        ${CMAKE_SOURCE_DIR}/src/xrTiramisuMaterialCore/tests)
    target_compile_features(xrMaterialInstanceEditorDocumentTests PRIVATE cxx_std_23)
    add_test(NAME xrMaterialInstanceEditorDocumentTests
        COMMAND xrMaterialInstanceEditorDocumentTests -rdbg)
    set_tests_properties(xrMaterialInstanceEditorDocumentTests PROPERTIES
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrMaterialInstanceEditorDocumentTests PROPERTIES FOLDER "Tests")

    add_executable(xrMaterialPreviewCompilerTests tests/MaterialPreviewCompilerTests.cpp)
    target_link_libraries(xrMaterialPreviewCompilerTests PRIVATE xrTiramisuMaterialEditorCore)
    target_include_directories(xrMaterialPreviewCompilerTests PRIVATE
        ${CMAKE_SOURCE_DIR}/src/xrTiramisuMaterialCore/tests)
    target_compile_features(xrMaterialPreviewCompilerTests PRIVATE cxx_std_23)
    add_test(NAME xrMaterialPreviewCompilerTests COMMAND xrMaterialPreviewCompilerTests -rdbg)
    set_tests_properties(xrMaterialPreviewCompilerTests PROPERTIES
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrMaterialPreviewCompilerTests PROPERTIES FOLDER "Tests")
    ixray_copy_dxc_runtime(xrMaterialPreviewCompilerTests)

    add_executable(xrMaterialDependencyWatcherTests
        tests/MaterialDependencyWatcherTests.cpp)
    target_link_libraries(xrMaterialDependencyWatcherTests PRIVATE xrTiramisuMaterialEditorCore)
    target_include_directories(xrMaterialDependencyWatcherTests PRIVATE
        ${CMAKE_SOURCE_DIR}/src/xrTiramisuMaterialCore/tests)
    target_compile_features(xrMaterialDependencyWatcherTests PRIVATE cxx_std_23)
    add_test(NAME xrMaterialDependencyWatcherTests
        COMMAND xrMaterialDependencyWatcherTests -rdbg)
    set_tests_properties(xrMaterialDependencyWatcherTests PROPERTIES
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrMaterialDependencyWatcherTests PROPERTIES FOLDER "Tests")

    add_executable(xrEditorViewportMaterialResolverTests
        tests/EditorViewportMaterialResolverTests.cpp)
    target_link_libraries(xrEditorViewportMaterialResolverTests PRIVATE
        xrTiramisuMaterialEditorCore)
    target_include_directories(xrEditorViewportMaterialResolverTests PRIVATE
        ${CMAKE_SOURCE_DIR}/src/xrTiramisuMaterialCore/tests)
    target_compile_features(xrEditorViewportMaterialResolverTests PRIVATE cxx_std_23)
    add_test(NAME xrEditorViewportMaterialResolverTests
        COMMAND xrEditorViewportMaterialResolverTests -rdbg)
    set_tests_properties(xrEditorViewportMaterialResolverTests PROPERTIES
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrEditorViewportMaterialResolverTests PROPERTIES FOLDER "Tests")

    add_executable(xrLegacyObjectMaterialMigrationTests
        tests/LegacyObjectMaterialMigrationTests.cpp)
    target_link_libraries(xrLegacyObjectMaterialMigrationTests PRIVATE
        xrTiramisuMaterialEditorCore)
    target_include_directories(xrLegacyObjectMaterialMigrationTests PRIVATE
        ${CMAKE_SOURCE_DIR}/src/xrTiramisuMaterialCore/tests)
    target_compile_features(xrLegacyObjectMaterialMigrationTests PRIVATE cxx_std_23)
    add_test(NAME xrLegacyObjectMaterialMigrationTests
        COMMAND xrLegacyObjectMaterialMigrationTests -rdbg)
    set_tests_properties(xrLegacyObjectMaterialMigrationTests PROPERTIES
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrLegacyObjectMaterialMigrationTests PROPERTIES FOLDER "Tests")
endif()
