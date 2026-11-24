#pragma once

namespace CrashWindow
{
	// Shows a blocking SDL3 crash dialog displaying the crash summary and callstacks.
	void Show(const char* Title, const char* Details);
}
