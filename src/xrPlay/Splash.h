#pragma once

#ifdef _EDITOR
#include <../Editors/xrECore/resource.h>
#define SPLASH_API ECORE_API
#else
#define SPLASH_API
#endif // _EDITOR


namespace splash
{
#ifdef _EDITOR
	SPLASH_API void SetBackground(int);
#endif
	SPLASH_API void Show();
	SPLASH_API void Close();
	SPLASH_API void SetProgressStatus(int prog, const char* status);
}