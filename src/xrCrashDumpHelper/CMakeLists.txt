project(xrCrashDumpHelper)

add_executable(xrCrashDumpHelper WIN32 "xrCrashDumpHelper.cpp")

target_link_libraries(xrCrashDumpHelper PRIVATE WindowsSDK::DBGHELP)

set_target_properties(xrCrashDumpHelper PROPERTIES ${IXRAY_FOLDER_ENGINE})
