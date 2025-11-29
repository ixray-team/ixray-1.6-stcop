# TBB
if (WIN32)
    set(IXR_TBB_SDK ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.OneTbb.Runtimes.win7-${NUGET_PACKAGE_PLATFORM}.2021.11.0)
    set(IXR_TBB_INC ${IXR_TBB_SDK}/build/native/include)
    set(IXR_TBB_LIB ${IXR_TBB_SDK}/runtimes/win7-${NUGET_PACKAGE_PLATFORM}/native/Release/tbb12.lib)
    set(IXR_TBB_BIN ${IXR_TBB_SDK}/runtimes/win7-${NUGET_PACKAGE_PLATFORM}/native/Release/tbb12.dll)

    add_imported_lib(
        TBB::tbb
        "${IXR_TBB_INC}"
        "${IXR_TBB_LIB}"
        "${IXR_TBB_BIN}"
    )
else()
    find_package(TBB REQUIRED)
endif()