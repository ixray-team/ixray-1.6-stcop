#include "stdafx.h"

#include <time.h>
#include "log.h"

static xrLogger* theLogger = nullptr;
static ThreadID theLogThread = 0;
XRCORE_API xr_queue <xrLogger::LogRecord>* xrLogger::logData;

void Log(const char* s)
{
	if (IsDebuggerPresent())
	{
		OutputDebugStringA(s);
		OutputDebugStringA("\n");
	}
	if (theLogger)
	{
		theLogger->SimpleMessage(s);
	}
}

void Msg(const char *format, ...)
{
	va_list		mark;
	va_start	(mark, format );
	if (theLogger)
	{
		theLogger->Msg(format, mark);
	}
    va_end		(mark);
}

void Log(const char* msg, const Fvector& dop)
{
	u32 buffer_size = (xr_strlen(msg) + 2 + 3 * (64 + 1) + 1) * sizeof(char);
	char* buf = (char*)_alloca(buffer_size);

	xr_sprintf(buf, buffer_size, "%s (%f,%f,%f)", msg, VPUSH(dop));
	Log(buf);
}

void Log(const char* msg, const Fmatrix& dop)
{
	u32 buffer_size = (xr_strlen(msg) + 2 + 4 * (4 * (64 + 1) + 1) + 1) * sizeof(char);
	char* buf = (char*)_alloca(buffer_size);

	xr_sprintf(buf, buffer_size, "%s:\n%f,%f,%f,%f\n%f,%f,%f,%f\n%f,%f,%f,%f\n%f,%f,%f,%f\n", msg, dop.i.x, dop.i.y, dop.i.z, dop._14_, dop.j.x, dop.j.y, dop.j.z, dop._24_, dop.k.x, dop.k.y, dop.k.z, dop._34_, dop.c.x, dop.c.y, dop.c.z, dop._44_);
	Log(buf);
}

void xrLogger::Msg(const char* Msg, va_list argList)
{
	string4096	formattedMessage;
	int MsgSize = _vsnprintf(formattedMessage, sizeof(formattedMessage) - 1, Msg, argList);

	if (MsgSize < 0)
		return;

	formattedMessage[MsgSize] = 0;

	if (IsDebuggerPresent() && bFastDebugLog)
	{
		OutputDebugStringA(formattedMessage);
		OutputDebugStringA("\n");
	}

	SimpleMessage(formattedMessage, MsgSize);
}

static bool TryLockWithTimeout(xrCriticalSection& Lock, u32 TimeoutMs)
{
	for (u32 Elapsed = 0; Elapsed <= TimeoutMs; ++Elapsed)
	{
		if (Lock.TryEnter())
		{
			return true;
		}
		if (Elapsed < TimeoutMs)
		{
			Sleep(1);
		}
	}
	return false;
}

void xrLogger::SimpleMessage(const char* Message, u32 MessageSize /*= 0*/)
{
	if (!Message)
	{
		return;
	}
	switch (MessageSize)
	{
	case (u32(-1)): return;
	case 0:			MessageSize = (u32)xr_strlen(Message); break;
	default:		break;
	}
	if (bCrashMode.load(std::memory_order_relaxed))
	{
		InternalWriteCrashText(Message, MessageSize, true);
		return;
	}
	if (!logData)
	{
		return;
	}
	xrCriticalSectionGuard guard(&logDataGuard);
	logData->emplace(LogRecord(Message, MessageSize));
}

void xrLogger::OpenLogFile()
{
	if (theLogger && !theLogger->bLogFileOpened)
	{
		theLogger->InternalOpenLogFile();
		theLogger->bLogFileOpened = true;
	}
}

const string_path& xrLogger::GetLogPath()
{
	static string_path emptyPath = "";
	return theLogger ? theLogger->logFileName : emptyPath;
}

void xrLogger::EnableFastDebugLog()
{
	if (theLogger)
	{
		theLogger->bFastDebugLog = true;
	}
}

void LogThreadEntryStartup(void* nullParam)
{
	PROF_THREAD("Logger Thread");
	if (theLogger)
	{
		theLogger->LogThreadEntry();
	}
}

void xrLogger::InitLog()
{
	if (theLogger == nullptr)
	{
		theLogger = new xrLogger;
		xrLogger::logData = new xr_queue <xrLogger::LogRecord>;
		theLogThread = thread_spawn(LogThreadEntryStartup, "X-Ray Log Thread", 0, nullptr);
	}
}

void xrLogger::FlushLog()
{
	if (theLogger)
	{
		theLogger->InternalFlushLog();
	}
}

void xrLogger::EmergencyFlush()
{
	if (theLogger)
	{
		theLogger->InternalEmergencyFlush();
	}
}

void xrLogger::WriteCrashLine(const char* Line)
{
	if (!Line)
	{
		return;
	}
	if (IsDebuggerPresent())
	{
		OutputDebugStringA(Line);
		OutputDebugStringA("\n");
	}
	if (theLogger)
	{
		theLogger->bCrashMode.store(true, std::memory_order_release);
		theLogger->InternalWriteCrashText(Line, (u32)xr_strlen(Line), true);
	}
}

void xrLogger::CloseLog()
{
	if (theLogger == nullptr)
		return;

	theLogger->bIsAlive = false;

	if (theLogThread)
	{
		Platform::JoinThread(theLogThread);
		theLogThread = 0;
	}

	theLogger->InternalCloseLog();

	xr_delete(logData);
	xr_delete(theLogger);
}

void xrLogger::AddLogCallback(LogCallback logCb)
{
	if (logCb == nullptr || theLogger == nullptr)
		return;

	xrCriticalSectionGuard guard(&theLogger->logCallbackGuard);
	theLogger->logCallbackList.push_back(logCb);
}

void xrLogger::RemoveLogCallback(LogCallback logCb)
{
	if (logCb == nullptr || theLogger == nullptr)
		return;

	xrCriticalSectionGuard guard(&theLogger->logCallbackGuard);
	theLogger->logCallbackList.remove(logCb);
}

void xrLogger::WriteRecord(const LogRecord& Record, bool bRunCallbacks)
{
	if (Record.Message.empty())
	{
		return;
	}

	const bool bIsDebug = IsDebuggerPresent();
	xr_vector<xr_string> LogLines = Record.Message.Split('\n');

	PROF_EVENT("Log: Apply Messages");
	for (const xr_string& Line : LogLines)
	{
		u32 LineSize = (u32)Line.size();
		if (LineSize > 0 && Line[LineSize - 1] == '\r')
		{
			--LineSize;
		}

		if (bIsDebug && !bFastDebugLog)
		{
			OutputDebugStringA(Line.c_str());
			OutputDebugStringA("\n");
		}

		{
			xrCriticalSectionGuard FileGuard(&logFileGuard);
			if (logFile != nullptr)
			{
				if (LineSize > 0)
				{
					logFile->w(Line.c_str(), LineSize);
				}
				logFile->w("\r\n", 2);
			}
		}

		if (bRunCallbacks)
		{
			xrCriticalSectionGuard CallbackGuard(&logCallbackGuard);
			for (const LogCallback& FnCallback : logCallbackList)
			{
				FnCallback(Line.c_str());
			}
		}
	}
}

void xrLogger::InternalWriteCrashText(const char* Text, u32 Size, bool bAppendNewLine)
{
	if (!TryLockWithTimeout(logFileGuard, 100))
	{
		return;
	}
	if (logFile != nullptr)
	{
		if (Text != nullptr && Size > 0)
		{
			logFile->w(Text, Size);
		}
		if (bAppendNewLine)
		{
			logFile->w("\r\n", 2);
		}
		logFile->flush();
	}
	logFileGuard.Leave();
}

void xrLogger::InternalFlushLog()
{
	if (bCrashMode.load(std::memory_order_acquire))
	{
		InternalEmergencyFlush();
		return;
	}

	PROF_EVENT("Log Flush");
	for (;;)
	{
		LogRecord TheRecord;
		{
			xrCriticalSectionGuard Guard(&logDataGuard);
			if (!logData || logData->empty())
			{
				break;
			}
			TheRecord = std::move(logData->front());
			logData->pop();
		}
		WriteRecord(TheRecord, true);
	}

	{
		xrCriticalSectionGuard FileGuard(&logFileGuard);
		if (logFile != nullptr)
		{
			logFile->flush();
		}
	}
	bFlushRequested = false;
}

void xrLogger::InternalEmergencyFlush()
{
	bCrashMode.store(true, std::memory_order_release);

	// Best-effort drain of any pending queued records without blocking forever on a frozen thread.
	if (logData != nullptr && TryLockWithTimeout(logDataGuard, 100))
	{
		while (!logData->empty())
		{
			const LogRecord& Record = logData->front();
			if (!Record.Message.empty())
			{
				InternalWriteCrashText(Record.Message.c_str(), (u32)Record.Message.size(), true);
			}
			logData->pop();
		}
		logDataGuard.Leave();
	}

	if (TryLockWithTimeout(logFileGuard, 100))
	{
		if (logFile != nullptr)
		{
			logFile->flush();
		}
		logFileGuard.Leave();
	}
	bFlushRequested = false;
}

void xrLogger::InternalCloseLog()
{
	InternalFlushLog();

	IWriter* tempCopy = nullptr;
	{
		xrCriticalSectionGuard FileGuard(&logFileGuard);
		tempCopy = logFile;
		logFile = nullptr;
	}

	if (tempCopy != nullptr)
	{
		tempCopy->flush();
		FS.w_close(tempCopy);
	}
}

xrLogger::xrLogger()
	: logFile(nullptr), bFastDebugLog(true), bLogFileOpened(false), bFlushRequested(false), bCrashMode(false)
{}

xrLogger::~xrLogger()
{
	InternalCloseLog();
}

void xrLogger::InternalOpenLogFile()
{
	string256 CurrentDate;
	string256 CurrentTime;
	
	Time time;
	xr_strconcat(CurrentDate, time.GetYearString().c_str(), ".", time.GetMonthString().c_str(), ".", time.GetDayString().c_str());
	xr_strconcat(CurrentTime, time.GetHoursString().c_str(), ".", time.GetMinutesString().c_str(), ".", time.GetSecondsString().c_str());

	xr_strconcat(logFileName, Core.ApplicationName, "-", CurrentDate, "-" , CurrentTime, "-", Core.UserName, ".log");
	if (FS.path_exist("$logs$"))
	{
		FS.update_path(logFileName, "$logs$", logFileName);
	}
	IWriter* OpenedFile = FS.w_open_ex(logFileName);
	CHECK_OR_EXIT(OpenedFile, "Can't create log file");
	xrCriticalSectionGuard FileGuard(&logFileGuard);
	logFile = OpenedFile;
}

void xrLogger::LogThreadEntry()
{
	while (bIsAlive || (logData && !logData->empty()))
	{
		if (bCrashMode.load(std::memory_order_acquire))
		{
			if (!bIsAlive)
			{
				break;
			}
			Sleep(13);
			continue;
		}

		PROF_EVENT("Log Frame");

		bool bWroteAny = false;
		for (;;)
		{
			if (bCrashMode.load(std::memory_order_acquire))
			{
				break;
			}

			LogRecord theRecord;
			{
				xrCriticalSectionGuard guard(&logDataGuard);
				if (logData && !logData->empty())
				{
					theRecord = std::move(logData->front());
					logData->pop();
				}
				else
				{
					break;
				}
			}

			if (!theRecord.Message.empty())
			{
				WriteRecord(theRecord, true);
				bWroteAny = true;
			}
		}

		if ((bWroteAny || bFlushRequested.load(std::memory_order_relaxed)) && !bCrashMode.load(std::memory_order_acquire))
		{
			PROF_EVENT("Log Flush");
			xrCriticalSectionGuard FileGuard(&logFileGuard);
			if (logFile != nullptr)
			{
				logFile->flush();
			}
			bFlushRequested = false;
		}

		if (bIsAlive)
		{
			Sleep(13); // work at 60 FPS roughly
		}
		else
		{
			xrCriticalSectionGuard guard(&logDataGuard);
			if (!logData || logData->empty())
			{
				break;
			}
		}
	}
}

xrLogger::LogRecord::LogRecord(const char* Msg, u32 sizeMsg)
	: Message(Msg, sizeMsg)
{}
