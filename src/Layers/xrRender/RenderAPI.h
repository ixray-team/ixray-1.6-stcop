#pragma once

// Exports renderer internals from a renderer DLL to the modules that link against it (editors).
// Game renderers loaded with LoadLibrary define neither macro.
#if defined(RENDER_API_EXPORTS)
#	define RENDER_API __declspec(dllexport)
#elif defined(RENDER_API_IMPORTS)
#	define RENDER_API __declspec(dllimport)
#else
#	define RENDER_API
#endif
