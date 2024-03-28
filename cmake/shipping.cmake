## Shipping
if (DEVIXRAY_ENABLE_SHIPPING)
    SET(CMAKE_CXX_FLAGS_SHIPPING "${CMAKE_CXX_FLAGS}" CACHE STRING "Flags used by the C++ compiler during coverage builds." FORCE )
    SET(CMAKE_C_FLAGS_SHIPPING "${CMAKE_C_FLAGS}" CACHE STRING "Flags used by the C compiler during coverage builds." FORCE )
    SET(CMAKE_EXE_LINKER_FLAGS_SHIPPING "" CACHE STRING "Flags used for linking binaries during coverage builds." FORCE )
    SET(CMAKE_SHARED_LINKER_FLAGS_SHIPPING "" CACHE STRING "Flags used by the shared libraries linker during coverage builds." FORCE )
    MARK_AS_ADVANCED(CMAKE_CXX_FLAGS_SHIPPING CMAKE_C_FLAGS_SHIPPING CMAKE_EXE_LINKER_FLAGS_SHIPPING CMAKE_SHARED_LINKER_FLAGS_SHIPPING )
    
    SET(IXR_CONFIGURATIONS_STR "Debug;RelWithDebInfo;Release;Shipping" CACHE STRING "" FORCE)
else()
    SET(IXR_CONFIGURATIONS_STR "Debug;RelWithDebInfo;Release" CACHE STRING "" FORCE)
endif()

# Build config
if (DEVIXRAY_ENABLE_SHIPPING)
    add_compile_options("$<$<CONFIG:Shipping>:/wd4530>" "$<$<CONFIG:Shipping>:/wd4251>" 
                        "$<$<CONFIG:Shipping>:/wd4275>" "$<$<CONFIG:Shipping>:/wd4577>" 
                        "$<$<CONFIG:Shipping>:/Ob2>" "$<$<CONFIG:Shipping>:/WX>")
endif()