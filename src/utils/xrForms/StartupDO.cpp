#include "../../xrCore/xrCore.h"
#include "../xrLC_Light/xrLC_Light.h"
#include "../xrLC_Light/global_calculation_data.h"
#include "cl_log.h"

extern volatile BOOL bClose;

static const char* h_str =
"The following keys are supported / required:\n"
"-? or -h	== this help\n"
"-f<NAME>	== compile level in gamedata\\levels\\<NAME>\\\n"
"-o			== modify build options\n"
"-use_intel == включить Embree"
"\n"
"NOTE: The last key is required for any functionality\n";

void Help(const char*);

void xrLight_Details();
 
#include "CompilersUI.h"
extern CompilersMode gCompilerMode;

void StartupDO()
{
	bClose = FALSE;

	for (auto& [Name, Selected] : gCompilerMode.Files)
	{
		if (!Selected)
			continue;

		char name[256];
		strcpy(name, Name.data());
		// Load project

		extern HWND logWindow;
		string256 temp;
		xr_sprintf(temp, "%s - Detail Compiler", name);
		SetWindowTextA(logWindow, temp);

		FS.get_path("$level$")->_set(name);

		Phase("Loading level...");
		gl_data.xrLoad();

		gl_data.use_intel = gCompilerMode.Embree;

		Phase("Lighting nodes...");
		xrLight_Details();

		gl_data.slots_data.Free();
	}
}