# FindOptiX.cmake - Find NVIDIA OptiX SDK 9+
# Ищет заголовочные файлы include

# Сначала проверяем переменную окружения
find_path(OptiX_INCLUDE_DIR
  NAMES optix.h
  PATHS
    ENV OPTIX_ROOT
    ENV OPTIX_ROOT/include
  NO_DEFAULT_PATH
)

if(NOT OptiX_INCLUDE_DIR)
    file(GLOB all_paths
        "C:/ProgramData/NVIDIA Corporation/OptiX SDK *"
        "C:/Program Files/NVIDIA GPU Computing Toolkit/OptiX SDK *"
        "C:/Program Files/NVIDIA Corporation/OptiX SDK *"
    )
    
    set(OptiX_filtered_paths)
    foreach(path IN LISTS all_paths)
        get_filename_component(dir_name "${path}" NAME)
        if(dir_name MATCHES "OptiX SDK 9\\.[0-9]+\\.[0-9]+")
            list(APPEND OptiX_filtered_paths "${path}")
        endif()
    endforeach()
    
    if(OptiX_filtered_paths)
        set(version_path_pairs)
        foreach(path IN LISTS OptiX_filtered_paths)
            get_filename_component(dir_name "${path}" NAME)
            if(dir_name MATCHES "OptiX SDK (9\\.[0-9]+\\.[0-9]+)")
                list(APPEND version_path_pairs "${CMAKE_MATCH_1}|${path}")
            endif()
        endforeach()
        
        list(SORT version_path_pairs)
        list(REVERSE version_path_pairs)
        
        foreach(pair IN LISTS version_path_pairs)
            string(REPLACE "|" ";" pair_list "${pair}")
            list(GET pair_list 1 path)
            
            if(EXISTS "${path}/include/optix.h")
                set(OptiX_INCLUDE_DIR "${path}/include")
                break()
            endif()
        endforeach()
    endif()
endif()

if(OptiX_INCLUDE_DIR)
  set(OptiX_FOUND TRUE)
else()
  set(OptiX_FOUND FALSE)
endif()

if(OptiX_FOUND)
  message(STATUS "Found OptiX include dir: ${OptiX_INCLUDE_DIR}")
else()
  message(WARNING "Could NOT find OptiX include directory")
endif()

set(OptiX_INCLUDE_DIRS ${OptiX_INCLUDE_DIR} CACHE PATH "OptiX include directory")

mark_as_advanced(OptiX_INCLUDE_DIR OptiX_DLL)