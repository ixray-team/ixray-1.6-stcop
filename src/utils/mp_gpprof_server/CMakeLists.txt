project(mp_gpprof_server)
set(MPGF_SRC_FILES)

# Files import
file(GLOB_RECURSE MPGF_SOURCE_ALL_FILES
    "atlas_stalkercoppc_v1.c"
    "entry_point.cpp"
    "gamespy_sake.cpp"
    "libraries/fcgi-2.4.0/libfcgi/fcgiapp.c"
    "libraries/fcgi-2.4.0/libfcgi/fcgio.cpp"
    "libraries/fcgi-2.4.0/libfcgi/fcgi_stdio.c"
    "libraries/fcgi-2.4.0/libfcgi/os_win32.c"
    "libraries/gamespy/common/gsAvailable.c"
    "libraries/gamespy/common/gsCore.c"
    "libraries/gamespy/common/gsCrypt.c"
    "libraries/gamespy/common/gsLargeInt.c"
    "libraries/gamespy/common/gsRC4.c"
    "libraries/gamespy/common/gsSHA1.c"
    "libraries/gamespy/common/gsSoap.c"
    "libraries/gamespy/common/gsSSL.c"
    "libraries/gamespy/common/gsStringUtil.c"
    "libraries/gamespy/common/gsUdpEngine.c"
    "libraries/gamespy/common/gsXML.c"
    "libraries/gamespy/darray.c"
    "libraries/gamespy/ghttp/ghttpBuffer.c"
    "libraries/gamespy/ghttp/ghttpCallbacks.c"
    "libraries/gamespy/ghttp/ghttpCommon.c"
    "libraries/gamespy/ghttp/ghttpConnection.c"
    "libraries/gamespy/ghttp/ghttpEncryption.c"
    "libraries/gamespy/ghttp/ghttpMain.c"
    "libraries/gamespy/ghttp/ghttpPost.c"
    "libraries/gamespy/ghttp/ghttpProcess.c"
    "libraries/gamespy/GP/gp.c"
    "libraries/gamespy/GP/gpi.c"
    "libraries/gamespy/GP/gpiBuddy.c"
    "libraries/gamespy/GP/gpiBuffer.c"
    "libraries/gamespy/GP/gpiCallback.c"
    "libraries/gamespy/GP/gpiConnect.c"
    "libraries/gamespy/GP/gpiInfo.c"
    "libraries/gamespy/GP/gpiKeys.c"
    "libraries/gamespy/GP/gpiOperation.c"
    "libraries/gamespy/GP/gpiPeer.c"
    "libraries/gamespy/GP/gpiProfile.c"
    "libraries/gamespy/GP/gpiPS3.c"
    "libraries/gamespy/GP/gpiSearch.c"
    "libraries/gamespy/GP/gpiTransfer.c"
    "libraries/gamespy/GP/gpiUnique.c"
    "libraries/gamespy/GP/gpiUtility.c"
    "libraries/gamespy/gt2/gt2Auth.c"
    "libraries/gamespy/gt2/gt2Buffer.c"
    "libraries/gamespy/gt2/gt2Callback.c"
    "libraries/gamespy/gt2/gt2Connection.c"
    "libraries/gamespy/gt2/gt2Encode.c"
    "libraries/gamespy/gt2/gt2Filter.c"
    "libraries/gamespy/gt2/gt2Main.c"
    "libraries/gamespy/gt2/gt2Message.c"
    "libraries/gamespy/gt2/gt2Socket.c"
    "libraries/gamespy/gt2/gt2Utility.c"
    "libraries/gamespy/hashtable.c"
    "libraries/gamespy/md5c.c"
    "libraries/gamespy/nonport.c"
    "libraries/gamespy/sake/sakeMain.c"
    "libraries/gamespy/sake/sakeRequest.c"
    "libraries/gamespy/sake/sakeRequestMisc.c"
    "libraries/gamespy/sake/sakeRequestModify.c"
    "libraries/gamespy/sake/sakeRequestRead.c"
    "libraries/pthreads-w32-2-8-0-release/pthread.c"
    "profiles_cache.cpp"
    "profile_data_types.cpp"
    "profile_request.cpp"
    "requests_processor.cpp"
    "sake_worker.cpp"
    "threads.cpp"
)

# Source groups
source_group("kernel" FILES ${MPGF_SOURCE_ALL_FILES})

# Apply list
list(APPEND MPGF_SRC_FILES ${MPGF_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER MPGF_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

# mp_gpprof_server project
add_executable(mp_gpprof_server ${MPGF_SRC_FILES})
target_include_directories(mp_gpprof_server PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(mp_gpprof_server PRIVATE "libraries/fcgi-2.4.0")
target_include_directories(mp_gpprof_server PRIVATE "libraries/fcgi-2.4.0/include")
target_include_directories(mp_gpprof_server PRIVATE "libraries/gamespy")
target_include_directories(mp_gpprof_server PRIVATE "libraries/pthreads-w32-2-8-0-release")

# Project defines
target_compile_definitions(mp_gpprof_server PRIVATE XRAY_DISABLE_GAMESPY_WARNINGS)
target_compile_definitions(mp_gpprof_server PRIVATE PTW32_STATIC_LIB)
target_compile_definitions(mp_gpprof_server PRIVATE _WINDOWS)

add_compile_options(/fp:fast)
target_compile_definitions(mp_gpprof_server PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(mp_gpprof_server PUBLIC xrCore)
target_link_libraries(mp_gpprof_server PUBLIC "ws2_32.lib")
