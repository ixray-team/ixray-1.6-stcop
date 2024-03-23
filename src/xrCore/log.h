#pragma once
#include "DateTime.hpp"
#include <atomic>
#define VPUSH(a)	((a).x), ((a).y), ((a).z)

void 	XRCORE_API		Msg	(const char* format, ...);
// Old shit
void 	XRCORE_API		Log			(const char* msg);
void 	XRCORE_API		Log			(const char* msg, const Fvector& dop);
void 	XRCORE_API		Log			(const char* msg, const Fmatrix& dop);

class XRCORE_API xrLogger
{
public:
	using LogCallback = void(*)	(const char* string);

	void Msg(LPCSTR Msg, va_list argList);
	void SimpleMessage(LPCSTR Message, u32 MessageSize = 0);

	static void OpenLogFile();
	static const string_path& GetLogPath();
	static void EnableFastDebugLog();
	static void InitLog();
	static void FlushLog();
	static void CloseLog();

	static void AddLogCallback(LogCallback logCb);
	static void RemoveLogCallback(LogCallback logCb);

	xrLogger();
	~xrLogger();

	void LogThreadEntry();

private:
	void InternalCloseLog();
	volatile bool bIsAlive;
	HANDLE hLogThread;

	void InternalOpenLogFile();

	string_path logFileName;
	volatile IWriter* logFile;

	struct LogRecord
	{
		LogRecord() {}
		LogRecord(LPCSTR Msg, u32 sizeMsg);
		xr_string Message;
		Time time;
	};

	xrCriticalSection logDataGuard;
	bool bFastDebugLog;

	std::atomic_bool bFlushRequested;

	//LogCallback onLogMsg;
	xr_list<LogCallback> logCallbackList;
public:
	static xr_queue <LogRecord> logData;
};