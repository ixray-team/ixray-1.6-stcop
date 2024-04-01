#pragma once

#ifdef XRPHYSICS_EXPORTS
#	define XRPHYSICS_API __declspec(dllexport)
#else
#	define XRPHYSICS_API __declspec(dllimport)
#endif

