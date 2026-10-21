#pragma once
#if defined(FFX_EXTENDED_ENTITYES_LOGIC_BUILD)
#define FFX_GAME_HOST_API __declspec(dllimport)
#define FFX_GAME_HOST_DATA_IMPORT __declspec(dllimport)
#else
#define FFX_GAME_HOST_DATA_IMPORT
#if defined(XRGAME_EXPORTS)
#define FFX_GAME_HOST_API __declspec(dllexport)
#else
#define FFX_GAME_HOST_API
#endif
#endif
