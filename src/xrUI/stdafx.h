#pragma once
#include "../xrEngine/stdafx.h"

#ifdef XR_UI_EXPORTS
#	define UI_API __declspec(dllexport)
#include "Widgets/UIWindow.h"
#include "Widgets/UIStatic.h"
#else
#	define UI_API __declspec(dllimport)
#endif
