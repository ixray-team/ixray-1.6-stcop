#pragma once
#include "../xrEngine/stdafx.h"

#ifdef	XR_SCRIPTS_EXPORTS
#	define SCRIPTS_API __declspec(dllexport)
#else
#	define SCRIPTS_API __declspec(dllimport)
#endif

#ifdef XRSE_FACTORY_EXPORTS
#undef SCRIPTS_API
#define SCRIPTS_API
#endif