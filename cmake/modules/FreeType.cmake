include(FetchContent)

if(PkgConfig_FOUND)
	pkg_check_modules(FREETYPE QUIET freetype2)
endif()

if(FREETYPE_FOUND)
	add_library(FreeType::FreeType INTERFACE IMPORTED)
	set_target_properties(FreeType::FreeType PROPERTIES
		INTERFACE_INCLUDE_DIRECTORIES "${FREETYPE_INCLUDE_DIRS}"
		INTERFACE_LINK_LIBRARIES "${FREETYPE_LIBRARIES}"
	)
else()
	set(CMAKE_POSITION_INDEPENDENT_CODE ON)
	FetchContent_Declare(
		freetype
		URL https://gitlab.freedesktop.org/freetype/freetype/-/archive/VER-2-14-3/freetype-VER-2-14-3.tar.gz
		DOWNLOAD_EXTRACT_TIMESTAMP TRUE
	)
	FetchContent_MakeAvailable(freetype)

	set_target_properties(freetype PROPERTIES FOLDER "3rd Party")

	if(TARGET freetype)
		add_library(FreeType::FreeType ALIAS freetype)
	elseif(NOT TARGET FreeType::FreeType)
		add_library(FreeType::FreeType INTERFACE IMPORTED)
		set_target_properties(FreeType::FreeType PROPERTIES
			INTERFACE_INCLUDE_DIRECTORIES "${freetype_SOURCE_DIR}/include"
			INTERFACE_LINK_LIBRARIES "freetype"
		)
	endif()
endif()
