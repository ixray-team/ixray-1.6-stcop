# HACK for Vorbis

if(TARGET Ogg::ogg)
    set(Ogg_FOUND TRUE CACHE INTERNAL "")
else()
    message(FATAL_ERROR "FindOgg.cmake: target Ogg::ogg not found! "
                       "Make sure Ogg is loaded via modules or FetchContent before calling find_package(Ogg).")
endif()