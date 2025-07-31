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
 
void StartupDO(LPSTR lpCmdLine) {
	bClose = FALSE;

	char cmd[512], name[256];

	bool bNet = false;
	xr_strcpy(cmd, lpCmdLine);
	_strlwr(cmd);

	if (strstr(cmd, "-?") || strstr(cmd, "-h")) 
	{
		Help(h_str); 
		return; 
	}

	if (strstr(cmd, "-f") == 0) 
	{
		Help(h_str); 
		return; 
	}
 
	// Load project
	name[0] = 0;
	sscanf(strstr(cmd, "-f") + 2, "%s", name);

	extern  HWND logWindow;
	string256			temp;
	xr_sprintf(temp, "%s - Detail Compiler", name);
	SetWindowTextA(logWindow, temp);

	FS.get_path("$level$")->_set(name);

	Phase("Loading level...");
	// gl_data.slots_data.Load();
	gl_data.xrLoad();

	gl_data.use_intel = strstr(cmd, "-use_intel");

	Phase("Lighting nodes...");
	xrLight_Details();

	gl_data.slots_data.Free();
}
