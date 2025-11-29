# YAML-CPP
if (WIN32)
    # NuGet paths
    set(YAML_CORE ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.YamlCpp.Runtimes.win-x64.0.8.0)
    set(YAML_INCL ${YAML_CORE}/build/native/include)
    set(YAML_LIB  ${YAML_CORE}/runtimes/win-x64/native/Release/yaml-cpp.lib)
    set(YAML_BIN  ${YAML_CORE}/runtimes/win-x64/native/Release/yaml-cpp.dll)

    add_imported_lib(
        yaml-cpp
        "${YAML_INCL}"
        "${YAML_LIB}"
        "${YAML_BIN}"
    )

else()
    FetchContent_Declare(
        yaml-cpp
        GIT_REPOSITORY https://github.com/jbeder/yaml-cpp.git
        GIT_TAG 0.8.0
    )
    FetchContent_MakeAvailable(yaml-cpp)

    if(NOT TARGET yaml-cpp)
        add_library(yaml-cpp INTERFACE IMPORTED)
        set_target_properties(yaml-cpp PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${yaml-cpp_SOURCE_DIR}/include"
            INTERFACE_LINK_LIBRARIES yaml-cpp
        )
    endif()
endif()
