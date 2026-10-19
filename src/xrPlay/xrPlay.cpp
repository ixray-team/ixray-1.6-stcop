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
// The Vulkan loader also needs the bundled MoltenVK ICD, which nothing
// configures for a double-click launch.
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
			chdir(path);

			char icd[PATH_MAX];
			snprintf(icd, sizeof(icd), "%s/MoltenVK_icd.json", path);
			setenv("VK_ICD_FILENAMES", icd, 0);
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
