#pragma once

#ifdef xrRenderInterface_EXPORTS
#	define RHI_API __declspec(dllexport)
#else
#	define RHI_API __declspec(dllimport)
#endif