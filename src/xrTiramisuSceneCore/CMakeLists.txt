project(xrTiramisuSceneCore)

add_library(xrTiramisuSceneCore STATIC
    TiramisuSceneTypes.h
    SceneJsonHelpers.h
    SceneAsset.cpp
    SceneAsset.h
    SceneConversionDump.cpp
    SceneConversionDump.h
)
target_include_directories(xrTiramisuSceneCore
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE
        ${EXTERNAL_INCLUDE_DIR}
)
target_compile_features(xrTiramisuSceneCore PUBLIC cxx_std_23)
target_link_libraries(xrTiramisuSceneCore PUBLIC xrCore)
set_property(TARGET xrTiramisuSceneCore PROPERTY IXRAY_DISABLE_LTCG TRUE)
set_target_properties(xrTiramisuSceneCore PROPERTIES POSITION_INDEPENDENT_CODE ON)

if(IXRAY_BUILD_TESTS)
    add_executable(xrSceneAssetTests tests/SceneAssetTests.cpp)
    target_link_libraries(xrSceneAssetTests PRIVATE xrTiramisuSceneCore)
    target_include_directories(xrSceneAssetTests PRIVATE ${EXTERNAL_INCLUDE_DIR})
    target_compile_features(xrSceneAssetTests PRIVATE cxx_std_23)
    add_test(NAME xrSceneAssetTests COMMAND xrSceneAssetTests -rdbg)
    set_tests_properties(xrSceneAssetTests PROPERTIES
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
    set_target_properties(xrSceneAssetTests PROPERTIES FOLDER "Tests")
endif()
