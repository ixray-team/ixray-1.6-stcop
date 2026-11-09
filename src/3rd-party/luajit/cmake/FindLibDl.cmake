if(NOT LINUX)
    return()
endif()

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(LibDl
    REQUIRED_VARS
        CMAKE_DL_LIBS
)

mark_as_advanced(CMAKE_DL_LIBS)

if(NOT LibDl::Library)
    add_library(LibDl::Library INTERFACE IMPORTED)
    target_link_libraries(LibDl::Library INTERFACE ${CMAKE_DL_LIBS})
endif()
