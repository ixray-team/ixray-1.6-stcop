#include "Application.h"

#if defined(IXR_APPLE_SERIES)
#include <limits.h>
#include <mach-o/dyld.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

// Finder/launchd start the process in "/" while the engine expects its data
// folder (fsgame.ltx) as the working directory; jump next to the executable.
static void PrepareExecutableFolderLaunch()
{
	char path[PATH_MAX];
	uint32_t size = sizeof(path);
	if (_NSGetExecutablePath(path, &size) == 0)
	{
		char* dir = strrchr(path, '/');
		if (dir != nullptr)
		{
			*dir = '\0';
			// Jump next to the executable only when the game files are there. Inside a
			// macOS bundle they live in Contents/Resources and the launcher has already
			// set that directory, so chdir would only break the lookup of fsgame.ltx.
			string_path fsgame;
			xr_sprintf(fsgame, "%s/fsgame.ltx", path);
			if (std::filesystem::exists(fsgame))
			{
				chdir(path);
			}
		}
	}
}
#endif

int ENTRY_FUNCTION(ENTRY_ARGS)
{
#if defined(IXR_APPLE_SERIES)
	PrepareExecutableFolderLaunch();
#endif
	CApplication App(ENTRY_ARGS_PUSH);
	return App.Run();
}
