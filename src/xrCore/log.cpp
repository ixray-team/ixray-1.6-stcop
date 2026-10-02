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

void xrLogger::SimpleMessage(const char* Message, u32 MessageSize /*= 0*/)
{
	switch (MessageSize)
	{
	case (u32(-1)): return;
	case 0:			MessageSize = xr_strlen(Message); break;
	default:		break;
	}
	if (!logData)
		return;
	xrCriticalSectionGuard guard(&logDataGuard);
	logData->emplace(LogRecord(Message, MessageSize));
}

void xrLogger::OpenLogFile()
{
	static bool isLogOpened = false;
	if (!isLogOpened && theLogger) {
		theLogger->InternalOpenLogFile();
		isLogOpened = true;
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
		theLogger->bFlushRequested = true;
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

void xrLogger::InternalCloseLog()
{
	FlushLog();

	IWriter* tempCopy = (IWriter*)logFile;
	logFile = nullptr;

	if (tempCopy != nullptr)
	{
		tempCopy->flush();
		FS.w_close(tempCopy);
	}
}

xrLogger::xrLogger()
	: logFile(nullptr), bFastDebugLog(true), bFlushRequested(false)
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
	logFile = FS.w_open_ex(logFileName);
	CHECK_OR_EXIT(logFile, "Can't create log file");
}

void xrLogger::LogThreadEntry()
{
	bool isDebug = IsDebuggerPresent();

	auto FlushLogIfRequestedLambda = [this]()
	{
		if (bFlushRequested)
		{
			PROF_EVENT("Log Flush")
			if (logFile != nullptr)
			{
				IWriter* mutableWritter = (IWriter*)logFile;
				mutableWritter->flush();
				::Msg("* Log file has been flushed successfully!");
			}
			bFlushRequested = false;
		}
	};

	while (bIsAlive || (logData && !logData->empty()))
	{
		PROF_EVENT("Log Frame");

		bool bHaveMore = false;
		LogRecord theRecord;

		{
			xrCriticalSectionGuard guard(&logDataGuard);
			if (logData && !logData->empty())
			{
				theRecord = logData->front();
				logData->pop();
				bHaveMore = !logData->empty();
			}
			else if (!bIsAlive)
			{
				break;
			}
		}

		if (!theRecord.Message.empty())
		{
			xr_vector<xr_string> LogLines = theRecord.Message.Split('\n');

			string256 TimeOfDay = {};

			PROF_EVENT("Log: Apply Messages")
			int TimeOfDaySize = 0;
			for (const xr_string& line : LogLines)
			{
				string4096 finalLine;
				xr_strconcat(finalLine, TimeOfDay, line.c_str());

				int FinalSize = TimeOfDaySize + (int)line.size();
				// line is ready, ready up everything

				// Output to MSVC debug output
				if (isDebug && !bFastDebugLog)
				{
					OutputDebugStringA(finalLine);
					OutputDebugStringA("\n");
				}

				if (logFile != nullptr)
				{
					IWriter* mutableWritter = (IWriter*)logFile;
					// write to file
					mutableWritter->w(finalLine, FinalSize);
					mutableWritter->w("\r\n", 2);
				}

				xrCriticalSectionGuard guard(&logCallbackGuard);
				for (const LogCallback& FnCallback : logCallbackList)
				{
					FnCallback(finalLine);
				}
			}
		}

		FlushLogIfRequestedLambda();

		if (bIsAlive && !bHaveMore)
		{
			Sleep(13); // work at 60 FPS roughly
		}
	}

	FlushLogIfRequestedLambda();
}

xrLogger::LogRecord::LogRecord(const char* Msg, u32 sizeMsg)
	: Message(Msg, sizeMsg)
{}
