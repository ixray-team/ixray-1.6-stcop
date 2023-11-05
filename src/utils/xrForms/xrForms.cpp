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

struct CompilersMode
{
	bool AI = false;
	bool DO = false;
	bool LC = false;

	bool Silent = false;
};

CompilersMode gCompilerMode;

void Startup(LPSTR lpCmdLine)
{
	u32 dwStartupTime = timeGetTime();

	u32 dwTimeLC = 0;
	if (gCompilerMode.LC)
	{
		dwTimeLC = timeGetTime();
		Phase("xrLC Startup");
		StartupLC(lpCmdLine);

		dwTimeLC = (timeGetTime() - dwTimeLC) / 1000;
	}

	u32 dwTimeAI = 0;
	if (gCompilerMode.AI)
	{
		dwTimeAI = timeGetTime();
		Phase("xrAI Startup");
		InitialFactory();
		StartupAI(lpCmdLine);
		DestroyFactory();
		dwTimeAI = (timeGetTime() - dwTimeAI) / 1000;
	}

	u32 dwTimeDO = 0;
	if (gCompilerMode.DO)
	{
		dwTimeDO = timeGetTime();
		Phase("xrDO Startup");
		StartupDO(lpCmdLine);
		dwTimeDO = (timeGetTime() - dwTimeDO) / 1000;
	}

	// Show statistic
	string256 stats;
	extern std::string make_time(u32 sec);
	u32 dwEndTime = timeGetTime();

	xr_sprintf
	(
		stats, 
		"Time elapsed: %s \r\n xrLC: %s\r\n xrAI: %s\r\n xrDO: %s", 
		make_time((dwEndTime - dwStartupTime) / 1000).c_str(), 
		make_time(dwTimeLC).c_str(),
		make_time(dwTimeAI).c_str(), 
		make_time(dwTimeDO).c_str()
	);

	if (!gCompilerMode.Silent)
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

	// Read modes
	bool SupportAll = strstr(lpCmdLine, "-all");
	gCompilerMode.AI = SupportAll || strstr(lpCmdLine, "-ai");
	gCompilerMode.LC = SupportAll || strstr(lpCmdLine, "-lc");
	gCompilerMode.DO = SupportAll || strstr(lpCmdLine, "-do");

	gCompilerMode.Silent = strstr(lpCmdLine, "-silent");

	// Give a LOG-thread a chance to startup
	InitCommonControls();
	Sleep(150);
	thread_spawn(logThread, "log-update", 1024 * 1024, 0);

	while (!logWindow)
		Sleep(100);

	Startup(lpCmdLine);

	return 0;
}
