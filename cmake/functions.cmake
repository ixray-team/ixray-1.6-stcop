if(NOT TARGET copy_all_dlls)
    add_custom_target(copy_all_dlls ALL)
    set_target_properties(copy_all_dlls PROPERTIES FOLDER "Pre-Build")
endif()

function(add_imported_lib name include_dir lib_path dll_paths)
    add_library(${name} INTERFACE IMPORTED)
    set_target_properties(${name} PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${include_dir}"
        INTERFACE_LINK_LIBRARIES "${lib_path}"
    )

    foreach(dll_file IN LISTS dll_paths)
        if(EXISTS "${dll_file}")
            set(target_dir "${IXRAY_OUTPUT}")
            add_custom_command(TARGET copy_all_dlls PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E make_directory "${target_dir}"
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                        "${dll_file}" "${target_dir}/"
            )
        endif()
    endforeach()

    add_dependencies(${name} copy_all_dlls)
endfunction()

function(target_copy_dependency TARGET_NAME FILE_PATH)
    if(WIN32)
        add_custom_command(TARGET ${TARGET_NAME} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    ${FILE_PATH}
                    ${IXRAY_OUTPUT}
        )
    else()
        get_filename_component(FILE_NAME ${FILE_PATH} NAME)
    
        add_custom_command(TARGET ${TARGET_NAME} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E echo "Copying ${FILE_NAME}"
            COMMAND ${CMAKE_COMMAND} -E copy_if_different 
                ${FILE_PATH} 
                $<TARGET_FILE_DIR:${TARGET_NAME}>/${FILE_NAME}
            COMMENT "Copying ${FILE_NAME} to output directory"
        )
    endif()
endfunction()

function(download_single_header url filename)
    set(full_path "${EXTERNAL_INCLUDE_DIR}${filename}")
    
    if(NOT EXISTS "${full_path}")
        message(STATUS "Downloading ${filename}...")
        
        file(DOWNLOAD 
            "${url}" 
            "${full_path}"
            TLS_VERIFY ON
            STATUS download_status
            TIMEOUT 60
        )
        
        list(GET download_status 0 status_code)
        if(NOT status_code EQUAL 0)
            message(WARNING "Failed to download ${filename}")
        else()
            message(STATUS "Successfully downloaded ${filename}")
        endif()
    endif()
endfunction()

find_package(PkgConfig QUIET)

# Общая функция для скачивания и распаковки SDK
function(download_and_extract_sdk url zip_file out_dir)
	if(NOT EXISTS "${zip_file}")
		message(STATUS "Downloading ${url} ...")
		file(DOWNLOAD
			"${url}"
			"${zip_file}"
			SHOW_PROGRESS
			TLS_VERIFY ON
			TIMEOUT 120
			INACTIVITY_TIMEOUT 30
			STATUS sdk_download_status
		)
		list(GET sdk_download_status 0 sdk_download_code)
		if(NOT sdk_download_code EQUAL 0)
			file(REMOVE "${zip_file}")
			list(GET sdk_download_status 1 sdk_download_error)
			message(FATAL_ERROR "Failed to download SDK ${url}: ${sdk_download_error}")
		endif()

		file(MAKE_DIRECTORY "${out_dir}")

		execute_process(
			COMMAND ${CMAKE_COMMAND} -E tar -xzf "${zip_file}"
			WORKING_DIRECTORY "${out_dir}"
			RESULT_VARIABLE sdk_extract_result
		)
		if(NOT sdk_extract_result EQUAL 0)
			message(FATAL_ERROR "Failed to extract ${zip_file}")
		endif()
	endif()
endfunction()

# Папка с зависимостями
set(DEP_DIR ${CMAKE_BINARY_DIR}/dep)

function(ixray_split_compile target)
	get_target_property(type ${target} TYPE)
	if(CMAKE_GENERATOR MATCHES "^Ninja" OR type STREQUAL "EXECUTABLE")
		return()
	endif()
	set(args ${ARGN})
	set(objs ${target}_obj)
	set(parts)
	while(args)
		list(POP_FRONT args part regex)
		list(APPEND objs ${part})
		string(APPEND parts " ${part} [[${regex}]]")
	endwhile()
	# DEFER expands variables when the call runs, so the names are baked in through EVAL.
	cmake_language(EVAL CODE "
		cmake_language(DEFER CALL _ixray_split_compile_sources ${target}${parts})
		cmake_language(DEFER DIRECTORY [[${CMAKE_SOURCE_DIR}]] CALL _ixray_split_compile_usage ${target} ${objs})")
endfunction()

function(ixray_split_compile_for_msvc target)
	if(CMAKE_GENERATOR MATCHES "^Visual Studio" AND CMAKE_CXX_COMPILER_ID STREQUAL "MSVC" AND NOT IXRAY_CROSS_COMPILATION)
		ixray_split_compile(${target} ${ARGN})
	endif()
endfunction()

function(_ixray_split_compile_sources target)
	set(link_inputs "\\.(rc|def|manifest|ico|bmp)$")
	get_target_property(sources ${target} SOURCES)
	set(kept ${sources})
	list(FILTER kept INCLUDE REGEX "${link_inputs}")
	list(FILTER sources EXCLUDE REGEX "${link_inputs}")
	get_target_property(deps ${target} MANUALLY_ADDED_DEPENDENCIES)

	set(args ${ARGN} ${target}_obj ".")
	set(objects)
	while(args)
		list(POP_FRONT args obj regex)
		set(part ${sources})
		list(FILTER part INCLUDE REGEX "${regex}")
		list(FILTER sources EXCLUDE REGEX "${regex}")

		add_library(${obj} OBJECT ${part})
		foreach(prop INCLUDE_DIRECTORIES COMPILE_DEFINITIONS COMPILE_OPTIONS COMPILE_FLAGS PRECOMPILE_HEADERS DISABLE_PRECOMPILE_HEADERS UNITY_BUILD UNITY_BUILD_BATCH_SIZE)
			get_target_property(value ${target} ${prop})
			if(NOT value MATCHES "-NOTFOUND$")
				set_property(TARGET ${obj} PROPERTY ${prop} "${value}")
			endif()
		endforeach()
		target_compile_definitions(${obj} PRIVATE ${target}_EXPORTS)
		set_target_properties(${obj} PROPERTIES POSITION_INDEPENDENT_CODE ON)
		foreach(dep IN LISTS deps)
			if(TARGET ${dep})
				get_target_property(type ${dep} TYPE)
				if(type STREQUAL "UTILITY")
					add_dependencies(${obj} ${dep})
				endif()
			endif()
		endforeach()
		list(APPEND objects $<TARGET_OBJECTS:${obj}>)
	endwhile()

	# The sources live only in the object libraries, so the IDE takes their IntelliSense
	# context from the project that actually compiles them.
	set_property(TARGET ${target} PROPERTY SOURCES ${kept} ${objects})
	set_target_properties(${target} PROPERTIES LINKER_LANGUAGE CXX DISABLE_PRECOMPILE_HEADERS ON UNITY_BUILD OFF COMPILE_FLAGS "")
endfunction()

# Runs after every directory is processed: $<TARGET_PROPERTY> carries the usage requirements
# of the whole link closure without adding the build-order dependency that linking would.
function(_ixray_split_compile_usage target)
	get_target_property(folder ${target} FOLDER)
	if(folder)
		foreach(obj IN LISTS ARGN)
			set_target_properties(${obj} PROPERTIES FOLDER "${folder}")
		endforeach()
	endif()
	set_target_properties(${target}_obj PROPERTIES PROJECT_LABEL ${target})
	set_target_properties(${target} PROPERTIES PROJECT_LABEL ${target}_link FOLDER "${PREDEFINED_TARGETS_FOLDER}/Link")

	get_target_property(queue ${target} LINK_LIBRARIES)
	if(NOT queue)
		return()
	endif()
	get_target_property(no_system_from_imported ${target} NO_SYSTEM_FROM_IMPORTED)
	set(seen)
	while(NOT "${queue}" STREQUAL "")
		list(POP_FRONT queue dep)
		if(NOT TARGET "${dep}" OR "${dep}" IN_LIST seen)
			continue()
		endif()
		list(APPEND seen "${dep}")
		get_target_property(next ${dep} INTERFACE_LINK_LIBRARIES)
		if(next)
			list(APPEND queue ${next})
		endif()
		get_target_property(system_target ${dep} SYSTEM)
		get_target_property(imported_target ${dep} IMPORTED)
		# Code generators of a dependency (XREFLECT) emit its public headers, so they must run
		# before the objects compile even though the dependency itself is not waited for.
		set(generators)
		if(NOT imported_target)
			get_target_property(dep_manual ${dep} MANUALLY_ADDED_DEPENDENCIES)
			foreach(gen IN LISTS dep_manual)
				if(TARGET ${gen})
					get_target_property(gen_type ${gen} TYPE)
					if(gen_type STREQUAL "UTILITY")
						list(APPEND generators ${gen})
					endif()
				endif()
			endforeach()
		endif()
		foreach(obj IN LISTS ARGN)
			if(generators)
				add_dependencies(${obj} ${generators})
			endif()
			if(system_target AND NOT (imported_target AND no_system_from_imported))
				target_include_directories(${obj} SYSTEM PRIVATE $<TARGET_PROPERTY:${dep},INTERFACE_INCLUDE_DIRECTORIES>)
			else()
				target_include_directories(${obj} PRIVATE $<TARGET_PROPERTY:${dep},INTERFACE_INCLUDE_DIRECTORIES>)
			endif()
			target_include_directories(${obj} SYSTEM PRIVATE $<TARGET_PROPERTY:${dep},INTERFACE_SYSTEM_INCLUDE_DIRECTORIES>)
			target_compile_definitions(${obj} PRIVATE $<TARGET_PROPERTY:${dep},INTERFACE_COMPILE_DEFINITIONS>)
			target_compile_options(${obj} PRIVATE $<TARGET_PROPERTY:${dep},INTERFACE_COMPILE_OPTIONS>)
		endforeach()
	endwhile()
endfunction()
