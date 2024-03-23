#include "../../xrCore/xrCore.h"
#include "cl_log.h"
#include "resource.h"

#include "xrForms.h"

#pragma warning(disable:4995)
//#include <timeapi.h>
//#include <commctrl.h>

#include <chrono>

#ifdef _WINDOWS_
#include <wx/msw/msgdlg.h>
#else
#include <wx/gtk2/msgdlg.h>
#endif

#pragma warning(default:4995)

//extern HWND logWindow;

void StartupAI(LPSTR lpCmdLine);
void StartupLC(LPSTR lpCmdLine);
void StartupDO(LPSTR lpCmdLine);

void InitialFactory();
void DestroyFactory();

void Help(const char* h_str)
{
	//MessageBoxA(0, h_str, "Command line options", MB_OK | MB_ICONINFORMATION);
	wxMessageDialog* help = new wxMessageDialog(NULL, 
		wxT("Command line options", h_str, wxOK | wxICON_INFORMATION));

	help->ShowModal();
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
	// Make time cross platform TODO: test
	//u32 dwStartupTime = timeGetTime();
	auto start = std::chrono::high_resolution_clock::now();
	auto finish = std::chrono::high_resolution_clock::now();

	std::chrono::duration<double, std::milli> elapsed = finish - start;
	double dwStartupTime = elapsed.count();

	//u32 dwTimeLC = 0;
	double dwTimeLC = .0;
	if (gCompilerMode.LC)
	{
		//dwTimeLC = timeGetTime();
		start = std::chrono::high_resolution_clock::now();
		Phase("xrLC Startup");
		StartupLC(lpCmdLine);

		//dwTimeLC = (timeGetTime() - dwTimeLC) / 1000;
		finish = std::chrono::high_resolution_clock::now();
	}

	//u32 dwTimeAI = 0;
	double dwTimeAI = .0;
	if (gCompilerMode.AI)
	{
		//dwTimeAI = timeGetTime();
		start = std::chrono::high_resolution_clock::now();
		Phase("xrAI Startup");
		InitialFactory();
		StartupAI(lpCmdLine);
		DestroyFactory();

		finish = std::chrono::high_resolution_clock::now();
		elapsed = finish - start;
		dwTimeAI = elapsed.count();

		//dwTimeAI = (timeGetTime() - dwTimeAI) / 1000;
	}

	//u32 dwTimeDO = 0;
	double dwTimeDO = .0;
	if (gCompilerMode.DO)
	{
		//dwTimeDO = timeGetTime();
		start = std::chrono::high_resolution_clock::now();

		Phase("xrDO Startup");
		StartupDO(lpCmdLine);

		finish = std::chrono::high_resolution_clock::now();
		elapsed = finish - start;
		dwTimeDO = elapsed.count();

		//dwTimeDO = (timeGetTime() - dwTimeDO) / 1000;
	}

	start = std::chrono::high_resolution_clock::now();
	finish = std::chrono::high_resolution_clock::now();
	elapsed = finish - start;
	double dwEndTime = elapsed.count();

	// Show statistics
	string256 stats;
	extern std::string make_time(double sec);
	//u32 dwEndTime = timeGetTime();

	xr_sprintf
	(
		stats, 
		"Time elapsed: %s \r\n xrLC: %s\r\n xrAI: %s\r\n xrDO: %s", 
		//make_time((dwEndTime - dwStartupTime) / 1000).c_str(), 
		make_time(dwEndTime - dwStartupTime).c_str(), 
		make_time(dwTimeLC).c_str(),
		make_time(dwTimeAI).c_str(), 
		make_time(dwTimeDO).c_str()
	);

	if (!gCompilerMode.Silent)
	{
		//MessageBoxA(logWindow, stats, "Congratulations!", MB_OK | MB_ICONINFORMATION);

		wxMessageDialog* logWindow = new wxMessageDialog(NULL, 
			wxT("Congratulations!", stats, wxOK | wxICON_INFORMATION));
		logWindow->ShowModal();
	}

	extern volatile BOOL bClose;

	// Close log
	bClose = TRUE;
	xrLogger::FlushLog();
	Sleep(200);
}

bool Main::OnInit()
{
	// Initialize debugging
	Debug._initialize(false);
	Core._initialize("IX-Ray Compilers");

	// Have to get arguments from the application array in wxWidgets
	std::string args;
	for (int i=0; i<(wxTheApp->argc); ++i) 
	{
		args += wxTheApp->argv[i];
    }
	LPSTR lpCmdLine = const_cast<char*>(args.c_str());

	// Read modes
	bool SupportAll = strstr(lpCmdLine, "-all");
	gCompilerMode.AI = SupportAll || strstr(lpCmdLine, "-ai");
	gCompilerMode.LC = SupportAll || strstr(lpCmdLine, "-lc");
	gCompilerMode.DO = SupportAll || strstr(lpCmdLine, "-do");

	gCompilerMode.Silent = strstr(lpCmdLine, "-silent");

	// Give a LOG-thread a chance to startup
	//InitCommonControls();

	//Sleep(150);

	thread_spawn(logThread, "log-update", 1024 * 1024, 0);

	/*
	while (!logWindow)
		Sleep(100);
		*/

	Startup(lpCmdLine);

	return true;
}

/*
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
*/

