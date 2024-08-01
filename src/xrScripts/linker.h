#pragma once

#ifdef	XR_SCRIPTS_EXPORTS
#	define SCRIPTS_API __declspec(dllexport)
#else
#	define SCRIPTS_API __declspec(dllimport)
#endif