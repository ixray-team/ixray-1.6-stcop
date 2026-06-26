# Discord SDK
if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(DISCORD_PLATFORM_EX x86_64)
else()
    set(DISCORD_PLATFORM_EX x86)
endif()

set(DISCORD_SDK_DIR ${CMAKE_BINARY_DIR}/dep/discord_gamesdk)
set(DISCORD_LIB   ${DISCORD_SDK_DIR}/lib/${DISCORD_PLATFORM_EX}/discord_game_sdk.dll.lib)
set(DISCORD_DLL   ${DISCORD_SDK_DIR}/lib/${DISCORD_PLATFORM_EX}/discord_game_sdk.dll)

add_imported_lib(
    DiscordSDK::discord
    ""                # include dirs (SDK не требует)
    "${DISCORD_LIB}"
    "${DISCORD_DLL}"
)