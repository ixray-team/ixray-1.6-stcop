#include "../../xrCore/xrCore.h"
#include "../xrlc_light/xrlc_light.h"
#include "../xrlc_light/global_calculation_data.h"
#include "cl_log.h"

extern volatile BOOL bClose;

static const char* h_str =
"The following keys are supported / required:\n"
"-? or -h	== this help\n"
"-f<NAME>	== compile level in gamedata\\levels\\<NAME>\\\n"
"-o			== modify build options\n"
"\n"
"NOTE: The last key is required for any functionality\n";

void Help(const char*);

void xrLight();
namespace lc_net { void xrNetDOLight(); }

void StartupDO(LPSTR lpCmdLine)
{
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

	if (strstr(cmd, "-net"))
		bNet = true;

	// Load project
	name[0] = 0;
	sscanf(strstr(cmd, "-f") + 2, "%s", name);

	extern  HWND logWindow;
	string256			temp;
	xr_sprintf(temp, "%s - Detail Compiler", name);
	SetWindowTextA(logWindow, temp);


	FS.get_path("$level$")->_set(name);

	Phase("Loading level...");
	gl_data.xrLoad();

	Phase("Lighting nodes...");

	if (bNet)
		lc_net::xrNetDOLight();
	else
		xrLight();

	gl_data.slots_data.Free();
}