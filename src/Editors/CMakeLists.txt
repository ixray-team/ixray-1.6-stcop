set(IXRAY_FOLDER_EDITORS FOLDER "Editors/")
set(IXRAY_FOLDER_EDITOR_CORE FOLDER "Editors/Core")
set(IXRAY_FOLDER_EDITOR_EXCORE FOLDER "Editors/External")

# Core libs 
if (IXRAY_EDITORS OR IXRAY_UTILS)
    add_subdirectory("xrEUI")
    add_subdirectory("xrEProps")
endif()

if (IXRAY_EDITORS)
    add_subdirectory("TiramisuMaterialEditor")
    add_subdirectory("xrECore")

    # NRI/GPU implementation belongs to xrRenderTiramisu. Editors compile only
    # authoring code and communicate through renderer-neutral interfaces.
    if(TARGET xrRenderTiramisu)
        target_sources(xrRenderTiramisu PRIVATE
            ${CMAKE_SOURCE_DIR}/src/Layers/xrRenderTiramisu/Editor/TiramisuEditorRendererFactory.cpp
            ${CMAKE_SOURCE_DIR}/src/Layers/xrRenderTiramisu/Editor/TiramisuEditorRenderBridge.cpp
            ${CMAKE_SOURCE_DIR}/src/Layers/xrRenderTiramisu/Editor/TiramisuEditorViewportScenePicker.cpp)
        target_link_libraries(xrRenderTiramisu PRIVATE
            xrTiramisuMaterialEditorCore)
    endif()

    # Main Editors
    add_subdirectory("ParticleEditor")
    add_subdirectory("ActorEditor")
    add_subdirectory("ShaderEditor")
    add_subdirectory("LevelEditor")
endif()

if (IXRAY_EDITORS AND IXRAY_BUILD_TESTS)
    add_executable(xrEditorRenderBackendTests tests/EditorRenderBackendTests.cpp)
    target_link_libraries(xrEditorRenderBackendTests PRIVATE xrECore)
    target_compile_features(xrEditorRenderBackendTests PRIVATE cxx_std_23)
    add_test(NAME xrEditorRenderBackendTests COMMAND xrEditorRenderBackendTests -rdbg)
    set_tests_properties(xrEditorRenderBackendTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrEditorRenderBackendTests PROPERTIES FOLDER "Tests")

    add_executable(xrMaterialPreviewRendererTests tests/MaterialPreviewRendererTests.cpp)
    target_link_libraries(xrMaterialPreviewRendererTests PRIVATE xrECore)
    target_compile_features(xrMaterialPreviewRendererTests PRIVATE cxx_std_23)
    add_test(NAME xrMaterialPreviewRendererTests COMMAND xrMaterialPreviewRendererTests -rdbg)
    set_tests_properties(xrMaterialPreviewRendererTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrMaterialPreviewRendererTests PROPERTIES FOLDER "Tests")

    add_executable(xrUIRenderBackendTests tests/XrUIRenderBackendTests.cpp)
    target_link_libraries(xrUIRenderBackendTests PRIVATE xrEUI)
    target_compile_features(xrUIRenderBackendTests PRIVATE cxx_std_23)
    add_test(NAME xrUIRenderBackendTests COMMAND xrUIRenderBackendTests -rdbg)
    set_tests_properties(xrUIRenderBackendTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrUIRenderBackendTests PROPERTIES FOLDER "Tests")

    add_executable(xrEditorNriFrameSchedulerTests tests/EditorNriFrameSchedulerTests.cpp)
    target_compile_features(xrEditorNriFrameSchedulerTests PRIVATE cxx_std_23)
    add_test(NAME xrEditorNriFrameSchedulerTests COMMAND xrEditorNriFrameSchedulerTests -rdbg)
    set_tests_properties(xrEditorNriFrameSchedulerTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrEditorNriFrameSchedulerTests PROPERTIES FOLDER "Tests")

    add_executable(xrEditorBoundedAsyncQueueTests
        tests/EditorBoundedAsyncQueueTests.cpp)
    target_compile_features(xrEditorBoundedAsyncQueueTests PRIVATE cxx_std_23)
    add_test(NAME xrEditorBoundedAsyncQueueTests
        COMMAND xrEditorBoundedAsyncQueueTests -rdbg)
    set_tests_properties(xrEditorBoundedAsyncQueueTests PROPERTIES
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrEditorBoundedAsyncQueueTests PROPERTIES
        FOLDER "Tests")

    add_executable(xrEditorNriStartupTests tests/EditorNriStartupTests.cpp)
    target_compile_features(xrEditorNriStartupTests PRIVATE cxx_std_23)
    add_test(NAME xrEditorNriStartupTests COMMAND xrEditorNriStartupTests -rdbg)
    set_tests_properties(xrEditorNriStartupTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrEditorNriStartupTests PROPERTIES FOLDER "Tests")

    add_executable(xrRenderStatisticsTests tests/RenderStatisticsTests.cpp)
    target_compile_features(xrRenderStatisticsTests PRIVATE cxx_std_23)
    add_test(NAME xrRenderStatisticsTests COMMAND xrRenderStatisticsTests -rdbg)
    set_tests_properties(xrRenderStatisticsTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrRenderStatisticsTests PROPERTIES FOLDER "Tests")

    add_executable(xrEditorNriTextureRegistryTests tests/EditorNriTextureRegistryTests.cpp)
    target_compile_features(xrEditorNriTextureRegistryTests PRIVATE cxx_std_23)
    add_test(NAME xrEditorNriTextureRegistryTests COMMAND xrEditorNriTextureRegistryTests -rdbg)
    set_tests_properties(xrEditorNriTextureRegistryTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrEditorNriTextureRegistryTests PROPERTIES FOLDER "Tests")

    add_executable(xrEditorViewportSceneMailboxTests tests/EditorViewportSceneMailboxTests.cpp)
    target_compile_features(xrEditorViewportSceneMailboxTests PRIVATE cxx_std_23)
    add_test(NAME xrEditorViewportSceneMailboxTests COMMAND xrEditorViewportSceneMailboxTests -rdbg)
    set_tests_properties(xrEditorViewportSceneMailboxTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrEditorViewportSceneMailboxTests PROPERTIES FOLDER "Tests")

    add_executable(xrEditorViewportScenePickerTests
        tests/EditorViewportScenePickerTests.cpp
        ${CMAKE_SOURCE_DIR}/src/Layers/xrRenderTiramisu/Editor/TiramisuEditorViewportScenePicker.cpp)
    target_compile_features(xrEditorViewportScenePickerTests PRIVATE cxx_std_23)
    add_test(NAME xrEditorViewportScenePickerTests
        COMMAND xrEditorViewportScenePickerTests -rdbg)
    set_tests_properties(xrEditorViewportScenePickerTests PROPERTIES
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrEditorViewportScenePickerTests PROPERTIES FOLDER "Tests")

    add_executable(xrEditorNativeSceneDocumentTests
        tests/EditorNativeSceneDocumentTests.cpp
        LevelEditor/Renderer/Tiramisu/TiramisuEditorNativeScene.cpp)
    target_link_libraries(xrEditorNativeSceneDocumentTests PRIVATE
        xrTiramisuSceneCore xrTiramisuMaterialCore xrTiramisuMaterialEditorCore)
    target_compile_features(xrEditorNativeSceneDocumentTests PRIVATE cxx_std_23)
    add_test(NAME xrEditorNativeSceneDocumentTests
        COMMAND xrEditorNativeSceneDocumentTests -rdbg)
    set_tests_properties(xrEditorNativeSceneDocumentTests PROPERTIES
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrEditorNativeSceneDocumentTests PROPERTIES
        FOLDER "Tests")

    add_executable(xrEditorTextureMailboxTests tests/EditorTextureMailboxTests.cpp)
    target_compile_features(xrEditorTextureMailboxTests PRIVATE cxx_std_23)
    add_test(NAME xrEditorTextureMailboxTests COMMAND xrEditorTextureMailboxTests -rdbg)
    set_tests_properties(xrEditorTextureMailboxTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrEditorTextureMailboxTests PROPERTIES FOLDER "Tests")

    add_executable(xrEditorViewportSceneShaderTests tests/EditorViewportSceneShaderTests.cpp)
    target_link_libraries(xrEditorViewportSceneShaderTests PRIVATE xrTiramisuMaterialCore)
    target_compile_features(xrEditorViewportSceneShaderTests PRIVATE cxx_std_23)
    add_test(NAME xrEditorViewportSceneShaderTests COMMAND xrEditorViewportSceneShaderTests -rdbg)
    set_tests_properties(xrEditorViewportSceneShaderTests PROPERTIES WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrEditorViewportSceneShaderTests PROPERTIES FOLDER "Tests")
    ixray_copy_dxc_runtime(xrEditorViewportSceneShaderTests)
    foreach(TiramisuEditorTestTarget IN ITEMS
        xrEditorRenderBackendTests
        xrMaterialPreviewRendererTests
        xrUIRenderBackendTests
        xrEditorNriFrameSchedulerTests
        xrEditorBoundedAsyncQueueTests
        xrEditorNriStartupTests
        xrRenderStatisticsTests
        xrEditorNriTextureRegistryTests
        xrEditorViewportSceneMailboxTests
        xrEditorViewportScenePickerTests
        xrEditorNativeSceneDocumentTests
        xrEditorTextureMailboxTests
        xrEditorViewportSceneShaderTests)
        target_link_libraries(${TiramisuEditorTestTarget} PRIVATE xrCore)
    endforeach()
endif()

# Groups
if (IXRAY_EDITORS OR IXRAY_UTILS)
    set_target_properties(xrEUI PROPERTIES ${IXRAY_FOLDER_EDITOR_CORE})
    set_target_properties(xrEProps PROPERTIES ${IXRAY_FOLDER_EDITOR_CORE})
endif()

if (IXRAY_EDITORS)
    set_target_properties(xrECore PROPERTIES ${IXRAY_FOLDER_EDITOR_CORE})

    set_target_properties(ParticlesEditor PROPERTIES ${IXRAY_FOLDER_EDITORS})
    set_target_properties(ActorEditor PROPERTIES ${IXRAY_FOLDER_EDITORS})
    set_target_properties(ShaderEditor PROPERTIES ${IXRAY_FOLDER_EDITORS})
    set_target_properties(LevelEditor PROPERTIES ${IXRAY_FOLDER_EDITORS})

    if (NOT IXRAY_UTILS)
        set_target_properties(xrSE_Factory PROPERTIES ${IXRAY_FOLDER_EDITOR_EXCORE})
        set_target_properties(xrDXT PROPERTIES ${IXRAY_FOLDER_EDITOR_EXCORE})
    endif()
endif()
