#include "resource.h"
#include "../../xrCore/xrCore.h"
#include <time.h>
#include <mmsystem.h>
#include <CommCtrl.h>
#include "cl_log.h"

i_lc_log* lc_log = 0;
//************************* Log-thread data


xr_vector<xr_string> myLogVector;
void MyLogCallback(const char* string) {
	xrCriticalSectionGuard LogGuard(&csLog);
	myLogVector.push_back(string);
}

xr_vector<xr_string>& GetLogVector()
{
	return myLogVector;
}

volatile BOOL				bClose				= FALSE;

static char					status[1024] = "";
static float				progress			= 0.0f;
static u32					phase_start_time	= 0;
static u32					phase_total_time	= 0;

//static HWND hwPhaseTime	= 0;

xr_vector<IterationData> iterationData;
IterationData* ActiveIteration = nullptr;

xr_string make_time(u32 sec)
{
	char buf[64];
	xr_sprintf(buf, "%2.0d:%2.0d:%2.0d", sec / 3600, (sec % 3600) / 60, sec % 60);
	int len = int(xr_strlen(buf));
	for (int i = 0; i < len; i++) if (buf[i] == ' ') buf[i] = '0';
	return buf;
}

void __cdecl Status(const char* format, ...) 
{
	csLog.Enter();
	va_list				mark;
	va_start(mark, format);
	vsprintf(status, format, mark);
	
	Msg("    | %s", status);
	csLog.Leave();
}

void StatusNoMsg(const char* format, ...)
{
	csLog.Enter();
	va_list				mark;
	va_start(mark, format);
	vsprintf(status, format, mark);
	
 	csLog.Leave();
}

void Progress(const float F) {
	progress = F;

	if (ActiveIteration->phases.size() > 0)
		ActiveIteration->phases[ActiveIteration->phases.size() - 1].PhasePersent = F;
}
float GetProgress()
{
	return progress;
}

u32& GetPhaseStartTime()
{
	return phase_start_time;
}

xr_vector<IterationData>& GetIterationData()
{
	return iterationData;
}

IterationData* GetActiveIteration()
{
	return ActiveIteration;
}
void SetActiveIteration(IterationData* i)
{
	if (auto* p = (ActiveIteration ? &ActiveIteration->phases : nullptr); 
		p && p->size() > 0 && (*p)[p->size() - 1].status != Complited)
		(*p)[p->size() - 1].status = Complited;

	 ActiveIteration = i;
}

void Phase(const char* phase_name) {
	csLog.Enter();

	phase_total_time = timeGetTime() - phase_start_time;

	// Start _new phase
	if (ActiveIteration->phases.size() > 0)
		ActiveIteration->phases[ActiveIteration->phases.size() - 1].status = Complited;

	ActiveIteration->phases.push_back({ phase_name });

	phase_start_time = timeGetTime();
	Progress(0);

	Msg("\n* New phase started: %s", phase_name);
	csLog.Leave();
}

// TODO: windows specific stuff, dunno about Linux
HWND logWindow=0;
void logThread(void* dummy) {
	extern void Startup(LPSTR lpCmdLine);
	
	xrLogger::AddLogCallback(MyLogCallback);

	string128 cmd;
	Startup(cmd);
}

void clLog(LPCSTR msg) {
	csLog.Enter();
	Log(msg);
	csLog.Leave();
}

void __cdecl clMsg(const char* format, ...) {
	va_list		mark;
	char buf[4 * 256];
	va_start(mark, format);
	vsprintf(buf, format, mark);


	string1024		_out_;
	xr_strconcat(_out_, "    |    | ", buf);
	clLog(_out_);
}

class client_log_impl : public i_lc_log {
	virtual void clMsg(LPCSTR msg) override { ::clMsg(msg); }
	virtual void clLog(LPCSTR msg) override { ::clLog(msg); }
	virtual void Status(LPCSTR msg) override { ::Status(msg); }
	virtual	void Progress(const float F) override { ::Progress(F); }
	virtual	void Phase(LPCSTR phase_name) override { ::Phase(phase_name); }
public:
	client_log_impl() { lc_log = this; }
} client_log_impl;
