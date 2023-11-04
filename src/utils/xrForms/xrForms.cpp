#include "../../xrCore/xrCore.h"
#include "cl_log.h"

#pragma warning(disable:4995)
#include <timeapi.h>
#include <commctrl.h>
#pragma warning(default:4995)

extern HWND logWindow;

void StartupAI(LPSTR lpCmdLine);
void StartupLC(LPSTR lpCmdLine);
void StartupDO(LPSTR lpCmdLine);

void InitialFactory();
void DestroyFactory();

void Help(const char* h_str)
{
	MessageBoxA(0, h_str, "Command line options", MB_OK | MB_ICONINFORMATION);
}

void Startup(LPSTR lpCmdLine)
{
	u32 dwStartupTime = timeGetTime();

	if (strstr(lpCmdLine, "-lc"))
	{
		Phase("xrLC Startup");
		StartupLC(lpCmdLine);
	}

	if (strstr(lpCmdLine, "-ai"))
	{
		Phase("xrAI Startup");
		InitialFactory();
		StartupAI(lpCmdLine);
		DestroyFactory();
	}

	if (strstr(lpCmdLine, "-do"))
	{
		Phase("xrDO Startup");
		StartupDO(lpCmdLine);
	}

	// Show statistic
	string256 stats;
	extern std::string make_time(u32 sec);
	u32 dwEndTime = timeGetTime();
	xr_sprintf(stats, "Time elapsed: %s", make_time((dwEndTime - dwStartupTime) / 1000).c_str());

	if (!strstr(lpCmdLine, "-silent"))
	{
		MessageBoxA(logWindow, stats, "Congratulation!", MB_OK | MB_ICONINFORMATION);
	}

	extern volatile BOOL bClose;

	// Close log
	bClose = TRUE;
	FlushLog();
	Sleep(200);
}

int APIENTRY WinMain
(
	HINSTANCE hInstance,
	HINSTANCE hPrevInstance,
	LPSTR     lpCmdLine,
	int       nCmdShow
)
{
	// Initialize debugging
	Debug._initialize(false);
	Core._initialize("IX-Ray Compilers");

	// Give a LOG-thread a chance to startup
	InitCommonControls();
	Sleep(150);
	thread_spawn(logThread, "log-update", 1024 * 1024, 0);

	while (!logWindow)
		Sleep(100);

	Startup(lpCmdLine);

	return 0;
}
