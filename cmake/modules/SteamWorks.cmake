set(STEAMWORKS_SDK_URL "https://github.com/ixray-team/ixray-packages/releases/download/d2024.5.3/steamworks_sdk_164.zip")
set(STEAMWORKS_SDK_ZIP "${DEP_DIR}/steamworks_sdk_164.zip")
set(STEAMWORKS_SDK_DIR "${DEP_DIR}/steamworks_sdk_164")
download_and_extract_sdk(${STEAMWORKS_SDK_URL} ${STEAMWORKS_SDK_ZIP} ${STEAMWORKS_SDK_DIR})
set(STEAMWORKS_ROOT ${STEAMWORKS_SDK_DIR})

file(GLOB EXTRACTED_DIRS "${STEAMWORKS_SDK_DIR}/*")
foreach(DIR ${EXTRACTED_DIRS})
	if(IS_DIRECTORY ${DIR})
		if(EXISTS "${DIR}/public/steam/steam_api.h" OR EXISTS "${DIR}/steam/steam_api.h")
			set(STEAMWORKS_ROOT ${DIR})
			break()
		endif()
	endif()
endforeach()

if(CMAKE_SIZEOF_VOID_P EQUAL 8)
	set(STEAMWORKS_ARCH "x64")
	set(STEAMWORKS_ARCH_DIR "x86_64")
else()
	set(STEAMWORKS_ARCH "x86")
	set(STEAMWORKS_ARCH_DIR "x86")
endif()

if(WIN32)
	set(STEAMWORKS_PLATFORM "windows")
	set(STEAMWORKS_PLATFORM_SHORT "win")
	set(STEAMWORKS_LIB_EXT ".lib")
	set(STEAMWORKS_DLL_EXT ".dll")
elseif(APPLE)
	set(STEAMWORKS_PLATFORM "macos")
	set(STEAMWORKS_PLATFORM_SHORT "osx")
	set(STEAMWORKS_LIB_EXT ".a")
	set(STEAMWORKS_DLL_EXT ".dylib")
elseif(UNIX)
	set(STEAMWORKS_PLATFORM "linux")
	set(STEAMWORKS_PLATFORM_SHORT "linux")
	set(STEAMWORKS_LIB_EXT ".a")
	set(STEAMWORKS_DLL_EXT ".so")
endif()

find_path(STEAMWORKS_INCLUDE_DIR
	NAMES steam/steam_api.h
	PATHS
		${STEAMWORKS_ROOT}
		${STEAMWORKS_ROOT}/public
		${STEAMWORKS_ROOT}/include
	PATH_SUFFIXES
		public
		include
		.
	NO_DEFAULT_PATH
)

if(NOT STEAMWORKS_INCLUDE_DIR)
	find_path(STEAMWORKS_INCLUDE_DIR
		NAMES steam/steam_api.h
		PATHS
			${STEAMWORKS_ROOT}
			${STEAMWORKS_ROOT}/public
			${STEAMWORKS_ROOT}/include
		PATH_SUFFIXES
			public
			include
			.
	)
endif()

if(WIN32)
	if(STEAMWORKS_ARCH STREQUAL "x64")
		set(STEAMWORKS_LIB_NAMES steam_api64.lib steam_api.lib)
	else()
		set(STEAMWORKS_LIB_NAMES steam_api.lib steam_api64.lib)
	endif()
elseif(APPLE)
	set(STEAMWORKS_LIB_NAMES libsteam_api.a steam_api.a)
else()
	set(STEAMWORKS_LIB_NAMES libsteam_api.a steam_api.a)
endif()

if(WIN32)
	if(CMAKE_SIZEOF_VOID_P EQUAL 8)
		set(STEAMWORKS_LIBRARY "${STEAMWORKS_ROOT}/redistributable_bin/win64/steam_api64.lib")
		set(STEAMWORKS_DLL     "${STEAMWORKS_ROOT}/redistributable_bin/win64/steam_api64.dll")
	else()
		set(STEAMWORKS_LIBRARY "${STEAMWORKS_ROOT}/redistributable_bin/steam_api.lib")
		set(STEAMWORKS_DLL     "${STEAMWORKS_ROOT}/redistributable_bin/steam_api.dll")
	endif()

elseif(APPLE)
	set(STEAMWORKS_LIBRARY "${STEAMWORKS_ROOT}/redistributable_bin/osx/libsteam_api.dylib")
	set(STEAMWORKS_DLL     "${STEAMWORKS_LIBRARY}")
elseif(UNIX)
	if(CMAKE_SIZEOF_VOID_P EQUAL 8)
		set(STEAMWORKS_LIBRARY "${STEAMWORKS_ROOT}/redistributable_bin/linux64/libsteam_api.so")
		set(STEAMWORKS_DLL     "${STEAMWORKS_LIBRARY}")
	else()
		set(STEAMWORKS_LIBRARY "${STEAMWORKS_ROOT}/redistributable_bin/linux32/libsteam_api.so")
		set(STEAMWORKS_DLL     "${STEAMWORKS_LIBRARY}")
	endif()
endif()

if(WIN32)
	find_file(STEAMWORKS_DLL
		NAMES steam_api64.dll steam_api.dll
		PATHS
			${STEAMWORKS_ROOT}
			${STEAMWORKS_ROOT}/redistributable_bin
		PATH_SUFFIXES
			${STEAMWORKS_ARCH}
			${STEAMWORKS_ARCH_DIR}
			${STEAMWORKS_PLATFORM}
			${STEAMWORKS_PLATFORM_SHORT}
			${STEAMWORKS_PLATFORM_SHORT}${STEAMWORKS_ARCH}
			${STEAMWORKS_PLATFORM_SHORT}/${STEAMWORKS_ARCH}
			redistributable_bin/${STEAMWORKS_PLATFORM}
			redistributable_bin/${STEAMWORKS_PLATFORM_SHORT}
			redistributable_bin/${STEAMWORKS_PLATFORM_SHORT}${STEAMWORKS_ARCH}
			redistributable_bin/${STEAMWORKS_PLATFORM_SHORT}/${STEAMWORKS_ARCH}
			.
		NO_DEFAULT_PATH
	)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Steamworks
	REQUIRED_VARS STEAMWORKS_LIBRARY STEAMWORKS_INCLUDE_DIR
	HANDLE_COMPONENTS
)

if(Steamworks_FOUND AND NOT TARGET Steamworks::Steamworks)
	set(STEAMWORKS_LIBRARIES "${STEAMWORKS_LIBRARY}")

	if(WIN32)
		set(STEAMWORKS_DLLS "${STEAMWORKS_DLL}")
	else()
		set(STEAMWORKS_DLLS "")
	endif()

	add_imported_lib(Steamworks::Steamworks "${STEAMWORKS_INCLUDE_DIR}" "${STEAMWORKS_LIBRARIES}" "${STEAMWORKS_DLLS}")
endif()

mark_as_advanced(STEAMWORKS_INCLUDE_DIR STEAMWORKS_LIBRARY STEAMWORKS_DLL)