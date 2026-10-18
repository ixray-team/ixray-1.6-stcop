#include "../../../xrCore/xrCore.h"
#include "../xrLC/xrLC_Light.h"
#include "../xrLC/DetailLight/global_calculation_data.h"
#include "../Common/cl_log.h"
 
void xrLight_Details();
 
#include "../UI/CompilersUI.h"
extern CompilersMode gCompilerMode;

void StartupDO()
{
	for (auto& [Name, Selected] : gCompilerMode.Files)
	{
		if (!Selected)
			continue;

		char name[256];
		strcpy(name, Name.data());
		// Load project

		string256 temp;
		xr_sprintf(temp, "%s - Detail Compiler", name);

		SDL_SetWindowTitle(g_AppInfo.Window, temp);

		FS.get_path("$level$")->_set(name);

		Phase("Loading level...");
		gl_data.xrLoad(gCompilerMode.SkipTHM);

		Phase("Lighting nodes...");
		xrLight_Details();

		gl_data.slots_data.Free();
	}
}