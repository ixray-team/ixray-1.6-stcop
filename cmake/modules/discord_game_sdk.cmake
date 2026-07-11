# Discord SDK
if(NOT "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
    set(DISCORD_PLATFORM_EX x86)
else()
    set(DISCORD_PLATFORM_EX x86_64)
endif()

set(DISCORD_GAME_SDK_ZIP ${DEP_DIR}/discord_gamesdk_3.2.1.zip)
set(DISCORD_GAME_SDK_URL https://github.com/ixray-team/ixray-packages/releases/download/d2023.12.8/Discord.GameSdk.3.2.1.zip)
set(DISCORD_GAME_SDK_DIR ${DEP_DIR}/discord_gamesdk)
download_and_extract_sdk(${DISCORD_GAME_SDK_URL} ${DISCORD_GAME_SDK_ZIP} ${DISCORD_GAME_SDK_DIR})
set(DISCORD_GAME_SDK ${DISCORD_GAME_SDK_DIR}/)

set(DISCORD_SDK_DIR ${CMAKE_BINARY_DIR}/dep/discord_gamesdk)
set(DISCORD_LIB   ${DISCORD_SDK_DIR}/lib/${DISCORD_PLATFORM_EX}/discord_game_sdk.dll.lib)
set(DISCORD_DLL   ${DISCORD_SDK_DIR}/lib/${DISCORD_PLATFORM_EX}/discord_game_sdk.dll)

add_imported_lib(
    DiscordSDK::discord
    ""                # include dirs (SDK не требует)
    "${DISCORD_LIB}"
    "${DISCORD_DLL}"
)