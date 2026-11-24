#pragma once
#include "Kernel/DateTime.hpp"

#define VPUSH(a) ((a).x), ((a).y), ((a).z)

void XRCORE_API Msg(const char* format, ...);
// Old shit
void XRCORE_API Log(const char* msg);
void XRCORE_API Log(const char* msg, const Fvector& dop);
void XRCORE_API Log(const char* msg, const Fmatrix& dop);

#include "ChZGlobalLogger.h"

class XRCORE_API xrLogger
{
public:
	using LogCallback = void(*)	(const char* string);

	void Msg(const char* Msg, va_list argList);
	void SimpleMessage(const char* Message, u32 MessageSize = 0);

	static void OpenLogFile();
	static const string_path& GetLogPath();
	static void EnableFastDebugLog();
	static void InitLog();
	static void FlushLog();
	static void EmergencyFlush();
	static void WriteCrashLine(const char* Line);
	static void CloseLog();

	static void AddLogCallback(LogCallback logCb);
	static void RemoveLogCallback(LogCallback logCb);

	xrLogger();
	~xrLogger();

	void LogThreadEntry();

private:
	void InternalCloseLog();
	void InternalFlushLog();
	void InternalEmergencyFlush();
	void InternalWriteCrashText(const char* Text, u32 Size, bool bAppendNewLine);
	void InternalOpenLogFile();

	string_path logFileName;
	IWriter* logFile;

	struct LogRecord
	{
		LogRecord() = default;
		LogRecord(const char* Msg, u32 sizeMsg);
		xr_string Message;
		Time time;
	};

	void WriteRecord(const LogRecord& Record, bool bRunCallbacks);

	xrCriticalSection logDataGuard;
	xrCriticalSection logFileGuard;
	xrCriticalSection logCallbackGuard;
	bool bFastDebugLog;
	bool bLogFileOpened;

	std::atomic_bool bIsAlive = true;
	std::atomic_bool bFlushRequested;
	std::atomic_bool bCrashMode;

	//LogCallback onLogMsg;
	xr_list<LogCallback> logCallbackList;
public:
	static xr_queue <LogRecord>* logData;
};