#include "stdafx.h"
#include "EventManager.h"
#include "xrDebug.h"

#include "Kernel/EngineExternal.h"
#include "Kernel/os_clipboard.h"
#include "CrashDumpHelper.h"
#include "CrashWindow.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <exception>

#ifdef IXR_WINDOWS
#	include "StackTrace/StackTrace.h"
#else
#	include <csignal>
#	if defined(IXR_LINUX) || defined(IXR_MACOS)
#		include <execinfo.h>
#	endif
#endif

#pragma warning(push)
#pragma warning(disable:4995)
#if defined(_MSC_VER) && !defined(IXR_ARM64) && !defined(__clang__)
#	include <direct.h>
#	include <dxerr.h>
#endif
#pragma warning(pop)

extern bool shared_str_initialized;
XRCORE_API xrDebug Debug;

#ifndef DEBUG
#	define USE_OWN_MINI_DUMP
#endif // DEBUG

static bool	error_after_dialog = false;

namespace
{
	void AppendBufferText(char*& Cursor, const char* Base, size_t TotalSize, const char* Text)
	{
		if (Cursor == nullptr || Base == nullptr || Text == nullptr || TotalSize == 0)
		{
			return;
		}
		const size_t Used = static_cast<size_t>(Cursor - Base);
		if (Used + 1 >= TotalSize)
		{
			return;
		}
		const size_t Remaining = TotalSize - Used;
		const int Added = std::snprintf(Cursor, Remaining, "%s", Text);
		if (Added > 0)
		{
			const size_t Step = static_cast<size_t>(Added) < Remaining ? static_cast<size_t>(Added) : (Remaining - 1);
			Cursor += Step;
		}
	}

	struct StackCaptureState
	{
		char* Buffer;
		const char* Base;
		size_t TotalSize;
		const char* Endline;
		int FrameIndex;
		bool bUseCrashWrite;
	};

	void EmitStackFrameLine(const char* Line, void* UserData)
	{
		auto* State = static_cast<StackCaptureState*>(UserData);
		if (Line == nullptr || State == nullptr)
		{
			return;
		}

		// Skip internal debug/stack-capture frames
		if (std::strstr(Line, "xrDebug::gather_info") != nullptr
			|| std::strstr(Line, "xrDebug::backend") != nullptr
			|| std::strstr(Line, "xrDebug::fail") != nullptr
			|| std::strstr(Line, "xrDebug::fatal") != nullptr
			|| std::strstr(Line, "CaptureCurrentStackTrace") != nullptr
			|| std::strstr(Line, "LogStackTrace") != nullptr)
		{
			return;
		}

		++State->FrameIndex;

		char Formatted[1280] = {};
		std::snprintf(Formatted, sizeof(Formatted), "Frame %d: %s", State->FrameIndex, Line);

		if (State->bUseCrashWrite)
		{
			xrLogger::WriteCrashLine(Formatted);
		}
		else
		{
			Msg("%s", Formatted);
		}

		if (State->Buffer != nullptr && State->Base != nullptr && State->TotalSize > 0)
		{
			AppendBufferText(State->Buffer, State->Base, State->TotalSize, Formatted);
			AppendBufferText(State->Buffer, State->Base, State->TotalSize, State->Endline ? State->Endline : "\r\n");
		}
	}

	void CaptureCurrentStackTrace(char*& Buffer, const char* Base, size_t TotalSize, const char* Endline, bool bUseCrashWrite)
	{
		int FrameIndex = 0;

#if USE_CXX_STACKTRACE
		const auto Stack = std::stacktrace::current();
		for (const std::stacktrace_entry& Entry : Stack)
		{
			const std::string SourceFile = Entry.source_file();
			if (SourceFile.contains("vctools\\crt\\vcstartup")
				|| SourceFile.contains("VC\\Tools\\MSVC"))
			{
				continue;
			}

			const std::string Description = Entry.description();
			if (Description.contains("xrDebug::gather_info")
				|| Description.contains("xrDebug::backend")
				|| Description.contains("xrDebug::fail")
				|| Description.contains("xrDebug::fatal")
				|| Description.contains("CaptureCurrentStackTrace")
				|| Description.contains("LogStackTrace"))
			{
				continue;
			}

			++FrameIndex;
			char Line[1280] = {};
			if (!SourceFile.empty())
			{
				std::snprintf(Line, sizeof(Line), "Frame %d: %s - %s:%u", FrameIndex, Description.c_str(), SourceFile.c_str(), static_cast<unsigned int>(Entry.source_line()));
			}
			else if (!Description.empty())
			{
				std::snprintf(Line, sizeof(Line), "Frame %d: %s", FrameIndex, Description.c_str());
			}
			else
			{
				std::snprintf(Line, sizeof(Line), "Frame %d: <unknown>", FrameIndex);
			}

			if (bUseCrashWrite)
			{
				xrLogger::WriteCrashLine(Line);
			}
			else
			{
				Msg("%s", Line);
			}

			if (Buffer != nullptr && Base != nullptr && TotalSize > 0)
			{
				AppendBufferText(Buffer, Base, TotalSize, Line);
				AppendBufferText(Buffer, Base, TotalSize, Endline ? Endline : "\r\n");
			}
		}
#endif

#ifdef IXR_WINDOWS
		if (FrameIndex == 0)
		{
			CONTEXT Context = {};
			Context.ContextFlags = CONTEXT_FULL;
			RtlCaptureContext(&Context);

			StackCaptureState State = { Buffer, Base, TotalSize, Endline, FrameIndex, bUseCrashWrite };
			StackTrace::WalkStackTrace(&Context, 128, &EmitStackFrameLine, &State);
			Buffer = State.Buffer;
			FrameIndex = State.FrameIndex;
		}
#elif defined(IXR_LINUX) || defined(IXR_MACOS)
		if (FrameIndex == 0)
		{
			void* Frames[128] = {};
			const int Count = backtrace(Frames, 128);
			char** Symbols = backtrace_symbols(Frames, Count);
			if (Symbols != nullptr)
			{
				StackCaptureState State = { Buffer, Base, TotalSize, Endline, FrameIndex, bUseCrashWrite };
				for (int i = 0; i < Count; ++i)
				{
					EmitStackFrameLine(Symbols[i], &State);
				}
				Buffer = State.Buffer;
				free(Symbols);
			}
		}
#endif
	}
} // namespace

void LogStackTrace(const char* header)
{
	if (header != nullptr && header[0] != '\0')
	{
		Msg("%s", header);
	}
	Msg("stack trace:\n");
	char* NullBuffer = nullptr;
	CaptureCurrentStackTrace(NullBuffer, nullptr, 0, "\n", false);
	xrLogger::FlushLog();
}

void xrDebug::gather_info(const char* expression, const char* description, const char* argument0, const char* argument1, const char* file, int line, const char* function, char* assertion_info, u32 const assertion_info_size)
{
	char* buffer_base = assertion_info;
	char* buffer = assertion_info;
	int assertion_size	= (int)assertion_info_size;
	const char*				endline = "\n";
	const char*				prefix = "[error]";
	bool				extended_description = (description && !argument0 && strchr(description,'\n'));
	for (int i=0; i<2; ++i) {
		if (!i)
			buffer		+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%sFATAL ERROR%s%s",endline,endline,endline);
		buffer			+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%sExpression    : %s%s",prefix,expression,endline);
		buffer			+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%sFunction      : %s%s",prefix,function,endline);
		buffer			+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%sFile          : %s%s",prefix,file,endline);
		buffer			+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%sLine          : %d%s",prefix,line,endline);
		
		if (extended_description) {
			buffer		+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%s%s%s",endline,description,endline);
			if (argument0) {
				if (argument1) {
					buffer	+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%s%s",argument0,endline);
					buffer	+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%s%s",argument1,endline);
				}
				else
					buffer	+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%s%s",argument0,endline);
			}
		}
		else {
			buffer		+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%sDescription   : %s%s",prefix,description,endline);
			if (argument0) {
				if (argument1) {
					buffer	+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%sArgument 0    : %s%s",prefix,argument0,endline);
					buffer	+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%sArgument 1    : %s%s",prefix,argument1,endline);
				}
				else
					buffer	+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%sArguments     : %s%s",prefix,argument0,endline);
			}
		}

		buffer			+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"%s",endline);
		if (!i) {
			Msg			("%s",assertion_info);
			xrLogger::FlushLog();
			buffer		= assertion_info;
			endline		= "\r\n";
			prefix		= "";
		}
	}

	if (SendErrorCallback)
	{
		SendErrorCallback(assertion_info);
	}

	if (!strstr(GetCommandLineA(), "-no_call_stack_assert"))
	{
		Msg("stack trace:\n");
		AppendBufferText(buffer, buffer_base, assertion_info_size, "stack trace:");
		AppendBufferText(buffer, buffer_base, assertion_info_size, endline);
		AppendBufferText(buffer, buffer_base, assertion_info_size, endline);

		CaptureCurrentStackTrace(buffer, buffer_base, assertion_info_size, endline, false);

		xrLogger::FlushLog();

		if (!ignore_error_window && !IsDebuggerPresent())
		{
			os_clipboard::copy_to_clipboard(assertion_info);
		}
	}
}

void xrDebug::do_exit(const std::string& message)
{
	extern XRCORE_API bool ignore_error_window;
	xrLogger::EmergencyFlush();

	if (!SilentErrorMode)
	{
		if (!ignore_error_window)
		{
			CrashWindow::Show("IX-Ray process has crashed", message.c_str());
		}
	}

	xrLogger::EmergencyFlush();
	exit(1);
}

void xrDebug::backend	(const char *expression, const char *description, const char *argument0, const char *argument1, const char *file, int line, const char *function, bool &ignore_always)
{
	PROF_EVENT("xrDebug::backend");
	static xrCriticalSection CS;

	CS.Enter			();

	error_after_dialog	= true;

	string4096			assertion_info;

	gather_info			(expression, description, argument0, argument1, file, line, function, assertion_info, sizeof(assertion_info) );

	const char*				endline = "\r\n";
	char* buffer = assertion_info + xr_strlen(assertion_info);
	buffer				+= xr_sprintf(buffer,sizeof(assertion_info) - u32(buffer - &assertion_info[0]),"%sPress CANCEL to abort execution%s",endline,endline);
	buffer				+= xr_sprintf(buffer,sizeof(assertion_info) - u32(buffer - &assertion_info[0]),"Press TRY AGAIN to continue execution%s",endline);
	buffer				+= xr_sprintf(buffer,sizeof(assertion_info) - u32(buffer - &assertion_info[0]),"Press CONTINUE to continue execution and ignore all the errors of this type%s%s",endline,endline);

	if (g_pEventManager == nullptr || g_pEventManager->IsEventThread())
	{
		show_dialog(assertion_info, ignore_always);
	}
	else
	{
		static std::string LastError = "";
		LastError = assertion_info;

		g_pEventManager->Event.Defer("KERNEL:assert", (size_t)&LastError, (size_t)&ignore_always);

		if (IsDebuggerPresent() && (!SilentErrorMode || Core.ParamsData.test(ECoreParams::no_silent_errors)))
		{
			DebugBreak();
		}
	}

	CS.Leave();
}

void xrDebug::show_dialog(const std::string& message, bool& ignore_always)
{
	if (SilentErrorMode)
	{
		return;
	}

	if (!ignore_error_window && handler)
		handler();

	if (!ignore_error_window && get_on_dialog())
		get_on_dialog()	(true);

	xrLogger::FlushLog();

	int buttonid = -1;

	const SDL_MessageBoxButtonData buttons[] = 
	{
		{ 0, 0, "Cancel" },
		{ 0, 1, "Try again" },
		{ SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT, 2, "Continue" },
#ifdef DEBUG
			{0, 3, "Trigger breakpoint and try again"},
			{0, 4, "Trigger breakpoint and continue"},
#endif
	};

	auto utf8_message = Platform::ANSI_TO_UTF8(Platform::UTF8_to_CP1251(message.c_str()));

	const SDL_MessageBoxData messageboxdata = 
	{
		SDL_MESSAGEBOX_ERROR | SDL_MESSAGEBOX_BUTTONS_LEFT_TO_RIGHT,		/* .flags */
		nullptr,					/* .window */
		"Fatal Error",				/* .title */
		utf8_message.c_str(),			/* .message */
		std::size(buttons),			/* .numbuttons */
		buttons,					/* .buttons */
		nullptr						/* .colorScheme */
	};
	extern XRCORE_API bool ignore_error_window;
	int ret = ignore_error_window ? 0 : SDL_ShowMessageBox(&messageboxdata, &buttonid);

	if (buttonid == 1)
	{
		// Return to main menu
		error_after_dialog = false;
	}
	else if (buttonid == 2)
	{
		error_after_dialog = false;
		ignore_always = true;
	}
#ifdef DEBUG
	else if (buttonid == 3)
	{
		// Return to main menu
		error_after_dialog = false;
		if (IsDebuggerPresent())
		{
			__debugbreak();
		}
	}
	else if (buttonid == 4)
	{
		error_after_dialog = false;
		ignore_always = true;
		if (IsDebuggerPresent())
		{
			__debugbreak();
		}
	}
#endif
	else
	{
		if(!ignore_error_window)
		{
			if (IsDebuggerPresent())
			{
				__debugbreak();
			}
			xrLogger::EmergencyFlush();
			// TODO: Maybe not correct
			exit(-1);
		}
	}
	if (!ignore_error_window && get_on_dialog())
		get_on_dialog()	(false);
}

const char* xrDebug::error2string(long code)
{
	static char desc_storage[1024] = {};
#ifdef IXR_WINDOWS
	FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM, nullptr, code, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), desc_storage, 0, nullptr);
#endif
	return desc_storage;
}

const char* xrDebug::dxerror2string(long code)
{
	static string512 Err = {};
	memset(Err, 0, sizeof(Err));
#if defined(_MSC_VER) && !defined(IXR_ARM64) && !defined(__clang__)
	DXGetErrorDescriptionA(code, Err, sizeof(Err));
#elif defined(IXR_WINDOWS)
	return error2string(code);
#endif
	return Err;
}

void xrDebug::error(long hr, const char* expr, const char* file, int line, const char* function, bool& ignore_always)
{
	backend(error2string(hr), expr, nullptr, nullptr, file, line, function, ignore_always);
}

void xrDebug::error(long hr, const char* expr, const char* e2, const char* file, int line, const char* function, bool& ignore_always)
{
	backend(error2string(hr), expr, e2, nullptr, file, line, function, ignore_always);
}

void xrDebug::fail(const char* e1, const char* file, int line, const char* function, bool& ignore_always)
{
	backend("assertion failed", e1, nullptr, nullptr, file, line, function, ignore_always);
}

void xrDebug::fail(const char* e1, const std::string& e2, const char* file, int line, const char* function, bool& ignore_always)
{
	backend(e1, e2.c_str(), nullptr, nullptr, file, line, function, ignore_always);
}

void xrDebug::fail(const char* e1, const char* e2, const char* file, int line, const char* function, bool& ignore_always)
{
	backend(e1, e2, nullptr, nullptr, file, line, function, ignore_always);
}

void xrDebug::fail(const char* e1, const char* e2, const char* e3, const char* file, int line, const char* function, bool& ignore_always)
{
	backend(e1, e2, e3, nullptr, file, line, function, ignore_always);
}

void xrDebug::fail(const char* e1, const char* e2, const char* e3, const char* e4, const char* file, int line, const char* function, bool& ignore_always)
{
	backend(e1, e2, e3, e4, file, line, function, ignore_always);
}

void xrDebug::error_dx(long hr, const char* expr, const char* file, int line, const char* function, bool& ignore_always)
{
	backend(dxerror2string(hr), expr, nullptr, nullptr, file, line, function, ignore_always);
}

void __cdecl xrDebug::fatal(const char *file, int line, const char *function, const char* F,...)
{
	string1024	buffer;

	va_list		p;
	va_start	(p,F);
	vsprintf	(buffer,F,p);
	va_end		(p);

	bool		ignore_always = true;

	backend		("fatal error","<no expression>",buffer,nullptr,file,line,function,ignore_always);
}

typedef void (*full_memory_stats_callback_type) ( );
XRCORE_API full_memory_stats_callback_type g_full_memory_stats_callback = nullptr;

int out_of_memory_handler	(size_t size)
{
	if ( g_full_memory_stats_callback )
		g_full_memory_stats_callback	( );
	else {
		Memory.mem_compact	();

#ifdef IXR_WINDOWS
		u32 process_heap = mem_usage_impl((void*)_get_heap_handle(), nullptr, nullptr);
#else
		u32 process_heap = mem_usage_impl(0, 0, 0);
#endif // IXR_WINDOWS

		int					eco_strings		= (int)g_pStringContainer->stat_economy			();
		int					eco_smem		= (int)g_pSharedMemoryContainer->stat_economy	();
		Msg					("* [x-ray]: process heap[%d K]", process_heap / 1024);
		Msg					("* [x-ray]: economy: strings[%d K], smem[%d K]",eco_strings/1024,eco_smem);
	}

	Debug.fatal				(DEBUG_INFO,"Out of memory. Memory request: %d K",size/1024);
	return					1;
}

XRCORE_API string_path g_bug_report_file;

#if defined(IXR_WINDOWS)
typedef long WINAPI UnhandledExceptionFilterType(struct _EXCEPTION_POINTERS *pExceptionInfo);
typedef long (  *PFNCHFILTFN ) ( EXCEPTION_POINTERS * pExPtrs ) ;
extern "C" bool  SetCrashHandlerFilter ( PFNCHFILTFN pFn );

static UnhandledExceptionFilterType	*previous_filter = nullptr;

#ifdef USE_OWN_MINI_DUMP
typedef bool (WINAPI *MINIDUMPWRITEDUMP)(HANDLE hProcess, DWORD dwPid, HANDLE hFile, MINIDUMP_TYPE DumpType,
										 CONST PMINIDUMP_EXCEPTION_INFORMATION ExceptionParam,
										 CONST PMINIDUMP_USER_STREAM_INFORMATION UserStreamParam,
										 CONST PMINIDUMP_CALLBACK_INFORMATION CallbackParam
										 );

// TODO: windows specific stuff, Linux would require debugging tools and APIs like `libunwind`, `libbfd`, and `gdb`...
void save_mini_dump			(_EXCEPTION_POINTERS *pExceptionInfo)
{
	const char* szResult = nullptr;
	string_path	szDumpPath;
	string_path	szScratch;
	string64	t_stemp;

	timestamp	(t_stemp);
	xr_strcpy		( szDumpPath, Core.ApplicationName);
	xr_strcat		( szDumpPath, "_"					);
	xr_strcat		( szDumpPath, Core.UserName			);
	xr_strcat		( szDumpPath, "_"					);
	xr_strcat		( szDumpPath, t_stemp				);
	xr_strcat		( szDumpPath, ".mdmp"				);

	__try {
		if (FS.path_exist("$logs$"))
			FS.update_path	(szDumpPath,"$logs$",szDumpPath);
	}
	__except( EXCEPTION_EXECUTE_HANDLER ) {
		string_path	temp;
		xr_strcpy		(temp,szDumpPath);
		xr_strcpy		(szDumpPath,"logs/");
		xr_strcat		(szDumpPath,temp);
	}

	// create the file
	HANDLE hFile = ::CreateFileA( szDumpPath, GENERIC_WRITE, FILE_SHARE_WRITE, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr );
	if (INVALID_HANDLE_VALUE==hFile)	
	{
		// try to place into current directory
		const char* filename = strrchr(szDumpPath, '\\');
		const char* slash = strrchr(szDumpPath, '/');
		if (slash && (!filename || slash > filename))
			filename = slash;
		if (filename)
			MoveMemory(szDumpPath, filename + 1, strlen(filename + 1) + 1);
		hFile		= ::CreateFileA( szDumpPath, GENERIC_WRITE, FILE_SHARE_WRITE, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr );
	}
	if (hFile != INVALID_HANDLE_VALUE)
	{
		_MINIDUMP_EXCEPTION_INFORMATION ExInfo;
			
		ExInfo.ThreadId = ::GetCurrentThreadId();
		ExInfo.ExceptionPointers = pExceptionInfo;
		ExInfo.ClientPointers = false;

		// The full dump takes seconds while the other threads run on: the process may end before it is written.
		// A small dump with the stacks goes first, next to it.
		const bool IsFullDump = strstr(Core.Params, "-full_dump") != nullptr;
		if (IsFullDump)
		{
			string_path SmallDumpPath;
			xr_strcpy(SmallDumpPath, szDumpPath);
			if (char* Extension = strrchr(SmallDumpPath, '.'))
				*Extension = 0;
			xr_strcat(SmallDumpPath, "_small.mdmp");
			HANDLE hSmallFile = ::CreateFileA(SmallDumpPath, GENERIC_WRITE, FILE_SHARE_WRITE, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
			if (hSmallFile != INVALID_HANDLE_VALUE)
			{
				MiniDumpWriteDump(GetCurrentProcess(), GetCurrentProcessId(), hSmallFile, MINIDUMP_TYPE(MiniDumpNormal | MiniDumpWithThreadInfo), &ExInfo, nullptr, nullptr);
				::CloseHandle(hSmallFile);
			}
		}

		// write the dump
		MINIDUMP_TYPE dump_flags = IsFullDump ? MINIDUMP_TYPE(MiniDumpNormal | MiniDumpWithFullMemory | MiniDumpIgnoreInaccessibleMemory | MiniDumpWithDataSegs | MiniDumpWithThreadInfo | MiniDumpWithFullMemoryInfo) : MINIDUMP_TYPE(MiniDumpNormal | MiniDumpWithThreadInfo);

		bool bOK = MiniDumpWriteDump( GetCurrentProcess(), GetCurrentProcessId(), hFile, dump_flags, &ExInfo, nullptr, nullptr );
		if (bOK)
		{
			xr_sprintf( szScratch, "Saved dump file to '%s'", szDumpPath );
			szResult = szScratch;
//			retval = EXCEPTION_EXECUTE_HANDLER;
		}
		else
		{
			xr_sprintf( szScratch, "Failed to save dump file to '%s' (error %d)", szDumpPath, GetLastError() );
			szResult = szScratch;
		}
		::CloseHandle(hFile);
	}
	else
	{
		xr_sprintf( szScratch, "Failed to create dump file '%s' (error %d)", szDumpPath, GetLastError() );
		szResult = szScratch;
	}
}
#endif // USE_OWN_MINI_DUMP

void format_message	(LPSTR buffer, const u32 &buffer_size)
{
	LPVOID		message = nullptr;
	DWORD		error_code = GetLastError(); 

	if (!error_code) {
		*buffer	= 0;
		return;
	}

	const DWORD Length = FormatMessageA(
		FORMAT_MESSAGE_ALLOCATE_BUFFER | 
		FORMAT_MESSAGE_FROM_SYSTEM |
		FORMAT_MESSAGE_IGNORE_INSERTS,
		nullptr,
		error_code,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPSTR)&message,
		0,
		nullptr
	);

	if (Length == 0 || message == nullptr)
	{
		xr_sprintf(buffer, buffer_size, "[error][%8lu]    : <unknown system error>", error_code);
		return;
	}

	xr_sprintf	(buffer,buffer_size,"[error][%8lu]    : %s",error_code,(const char*)message);
	LocalFree	(message);
}

#include <errorrep.h>

static bool EnabledStackTrace = true;

static const char* ExceptionCodeToString(u32 Code)
{
#define EXCEPTION_CODE_CASE(Name) case EXCEPTION_##Name: return "EXCEPTION_" #Name;
	switch (Code)
	{
	EXCEPTION_CODE_CASE(ACCESS_VIOLATION)
	EXCEPTION_CODE_CASE(ARRAY_BOUNDS_EXCEEDED)
	EXCEPTION_CODE_CASE(BREAKPOINT)
	EXCEPTION_CODE_CASE(DATATYPE_MISALIGNMENT)
	EXCEPTION_CODE_CASE(FLT_DENORMAL_OPERAND)
	EXCEPTION_CODE_CASE(FLT_DIVIDE_BY_ZERO)
	EXCEPTION_CODE_CASE(FLT_INEXACT_RESULT)
	EXCEPTION_CODE_CASE(FLT_INVALID_OPERATION)
	EXCEPTION_CODE_CASE(FLT_OVERFLOW)
	EXCEPTION_CODE_CASE(FLT_STACK_CHECK)
	EXCEPTION_CODE_CASE(FLT_UNDERFLOW)
	EXCEPTION_CODE_CASE(ILLEGAL_INSTRUCTION)
	EXCEPTION_CODE_CASE(IN_PAGE_ERROR)
	EXCEPTION_CODE_CASE(INT_DIVIDE_BY_ZERO)
	EXCEPTION_CODE_CASE(INT_OVERFLOW)
	EXCEPTION_CODE_CASE(INVALID_DISPOSITION)
	EXCEPTION_CODE_CASE(NONCONTINUABLE_EXCEPTION)
	EXCEPTION_CODE_CASE(PRIV_INSTRUCTION)
	EXCEPTION_CODE_CASE(SINGLE_STEP)
	EXCEPTION_CODE_CASE(STACK_OVERFLOW)
	default: return "UNKNOWN_EXCEPTION";
	}
#undef EXCEPTION_CODE_CASE
}

struct CrashStackContext
{
	char* Cursor;
	const char* Base;
	size_t TotalSize;
};

static void OnCrashStackFrame(const char* Line, void* UserData)
{
	xrLogger::WriteCrashLine(Line);

	auto* Ctx = static_cast<CrashStackContext*>(UserData);
	if (Ctx != nullptr && Ctx->Cursor != nullptr)
	{
		AppendBufferText(Ctx->Cursor, Ctx->Base, Ctx->TotalSize, Line);
		AppendBufferText(Ctx->Cursor, Ctx->Base, Ctx->TotalSize, "\r\n");
	}
}

static void WalkCrashStackProtected(PCONTEXT ContextRecord, CrashStackContext* Ctx)
{
	__try
	{
		StackTrace::WalkAllThreads(ContextRecord, 128, &OnCrashStackFrame, Ctx);
	}
	__except (EXCEPTION_EXECUTE_HANDLER)
	{
		xrLogger::WriteCrashLine("! Exception while capturing stack trace");
	}
}

static void ShowCrashWindowProtected(const char* Title, const char* Details)
{
	__try
	{
		CrashWindow::Show(Title, Details);
	}
	__except (EXCEPTION_EXECUTE_HANDLER)
	{
		xrLogger::WriteCrashLine("! Exception inside CrashWindow::Show");
	}
}

static void InvokeOnDialogProtected(on_dialog* Callback, bool Before)
{
	if (Callback == nullptr)
	{
		return;
	}
	__try
	{
		Callback(Before);
	}
	__except (EXCEPTION_EXECUTE_HANDLER)
	{
		xrLogger::WriteCrashLine("! Exception inside on_dialog callback");
	}
}

static void UpdateClipboardOnCrashProtected(const char* Text)
{
	if (Text == nullptr || Text[0] == '\0')
	{
		return;
	}
	__try
	{
		os_clipboard::copy_to_clipboard(Text);
	}
	__except (EXCEPTION_EXECUTE_HANDLER)
	{
		xrLogger::WriteCrashLine("! Exception while updating clipboard on crash");
	}
}

void ProcessStackTrace(_EXCEPTION_POINTERS *pExceptionInfo, char* OutCrashText, size_t OutCrashTextSize)
{
	string256 error_message = {};
	format_message(error_message, sizeof(error_message));

	char* CrashCursor = OutCrashText;
	if (OutCrashText != nullptr && OutCrashTextSize > 0)
	{
		OutCrashText[0] = '\0';
	}

	xrLogger::WriteCrashLine("");
	xrLogger::WriteCrashLine("FATAL ERROR");
	if (pExceptionInfo != nullptr && pExceptionInfo->ExceptionRecord != nullptr)
	{
		const EXCEPTION_RECORD* Rec = pExceptionInfo->ExceptionRecord;
		char Header[512] = {};
		if (Rec->ExceptionCode == EXCEPTION_ACCESS_VIOLATION && Rec->NumberParameters >= 2)
		{
			const char* AccessOp = Rec->ExceptionInformation[0] == 0 ? "reading" : (Rec->ExceptionInformation[0] == 1 ? "writing" : "executing");
			std::snprintf(Header, sizeof(Header), "[error] Unhandled exception: 0x%08lx (%s) at 0x%016llx (%s address 0x%016llx)",
				Rec->ExceptionCode, ExceptionCodeToString(Rec->ExceptionCode),
				static_cast<unsigned long long>(reinterpret_cast<uintptr_t>(Rec->ExceptionAddress)),
				AccessOp, static_cast<unsigned long long>(Rec->ExceptionInformation[1]));
		}
		else
		{
			std::snprintf(Header, sizeof(Header), "[error] Unhandled exception: 0x%08lx (%s) at 0x%016llx",
				Rec->ExceptionCode, ExceptionCodeToString(Rec->ExceptionCode),
				static_cast<unsigned long long>(reinterpret_cast<uintptr_t>(Rec->ExceptionAddress)));
		}
		xrLogger::WriteCrashLine(Header);
		if (OutCrashText != nullptr)
		{
			AppendBufferText(CrashCursor, OutCrashText, OutCrashTextSize, Header);
			AppendBufferText(CrashCursor, OutCrashText, OutCrashTextSize, "\r\n\r\n");
		}
	}

	if (EnabledStackTrace && pExceptionInfo != nullptr && pExceptionInfo->ContextRecord != nullptr)
	{
		xrLogger::WriteCrashLine("----------------------------------------------");
		xrLogger::WriteCrashLine("stack trace:");

		static char StackBuffer[65536] = {};
		StackBuffer[0] = '\0';
		CrashStackContext Ctx = { StackBuffer, StackBuffer, sizeof(StackBuffer) };
		WalkCrashStackProtected(pExceptionInfo->ContextRecord, &Ctx);

		xrLogger::WriteCrashLine("----------------------------------------------");

		if (OutCrashText != nullptr && StackBuffer[0] != '\0')
		{
			AppendBufferText(CrashCursor, OutCrashText, OutCrashTextSize, StackBuffer);
		}

		if (*error_message)
		{
			xrLogger::WriteCrashLine(error_message);
			if (OutCrashText != nullptr)
			{
				AppendBufferText(CrashCursor, OutCrashText, OutCrashTextSize, "\r\n");
				AppendBufferText(CrashCursor, OutCrashText, OutCrashTextSize, error_message);
			}
		}

		if (EngineExternal()[EEngineExternalSystem::CustomMessageInClipboardOnCrash])
		{
			xr_stack_string256 ClipboardMessage = "Please, provide a full log in bugreport";
			auto GetFunc = ClipboardMessageCallback::instance().GetFunc();
			if (GetFunc)
			{
				ClipboardMessage = GetFunc();
			}
			UpdateClipboardOnCrashProtected(ClipboardMessage.c_str());
		}
		else if (StackBuffer[0] != '\0')
		{
			if (*error_message)
			{
				AppendBufferText(Ctx.Cursor, Ctx.Base, Ctx.TotalSize, "\r\n");
				AppendBufferText(Ctx.Cursor, Ctx.Base, Ctx.TotalSize, error_message);
			}
			static char MarkdownBuffer[65568] = {};
			char* MdCursor = MarkdownBuffer;
			MarkdownBuffer[0] = '\0';
			AppendBufferText(MdCursor, MarkdownBuffer, sizeof(MarkdownBuffer), "```\r\n");
			AppendBufferText(MdCursor, MarkdownBuffer, sizeof(MarkdownBuffer), StackBuffer);
			if (MdCursor > MarkdownBuffer && *(MdCursor - 1) != '\n')
			{
				AppendBufferText(MdCursor, MarkdownBuffer, sizeof(MarkdownBuffer), "\r\n");
			}
			AppendBufferText(MdCursor, MarkdownBuffer, sizeof(MarkdownBuffer), "```");
			UpdateClipboardOnCrashProtected(MarkdownBuffer);
		}
	}

	xrLogger::EmergencyFlush();
}

LONG WINAPI UnhandledFilter	(_EXCEPTION_POINTERS *pExceptionInfo)
{
	// One thread reports the crash. Another thread failing meanwhile (a corrupted heap often fails on several)
	// waits here instead of ending the process before the dump is written.
	static std::atomic<DWORD> CrashingThread = 0;
	DWORD ExpectedThread = 0;
	const DWORD CurrentThread = ::GetCurrentThreadId();
	if (!CrashingThread.compare_exchange_strong(ExpectedThread, CurrentThread) && ExpectedThread != CurrentThread)
	{
		::Sleep(INFINITE);
	}

	// Flush all buffered log lines immediately before minidump or symbol loading.
	xrLogger::EmergencyFlush();

#ifdef USE_OWN_MINI_DUMP
	// Before the stack trace: the symbol engine takes seconds and allocates from a heap that may be broken.
	// The helper process writes the dumps without this process allocating; without it the dump is written here.
	if (!CrashDumpHelper::Write(pExceptionInfo))
	{
		save_mini_dump	(pExceptionInfo);
	}
#endif // USE_OWN_MINI_DUMP

	static char CrashText[65536] = {};
	ProcessStackTrace(pExceptionInfo, CrashText, sizeof(CrashText));

	InvokeOnDialogProtected(Debug.get_on_dialog(), true);

	extern XRCORE_API bool ignore_error_window;
	if (!ignore_error_window && !Debug.SilentErrorMode)
	{
		ShowCrashWindowProtected("IX-Ray process has crashed", CrashText[0] != '\0' ? CrashText : "Fatal error occurred.");
	}
	else
	{
		char SkipMsg[128] = {};
		std::snprintf(SkipMsg, sizeof(SkipMsg), "! Crash window suppressed (ignore_error_window=%d, SilentErrorMode=%d)",
			ignore_error_window ? 1 : 0, Debug.SilentErrorMode ? 1 : 0);
		xrLogger::WriteCrashLine(SkipMsg);
	}

	ReportFault(pExceptionInfo, 0);

	InvokeOnDialogProtected(Debug.get_on_dialog(), false);

	xrLogger::EmergencyFlush();

	return EXCEPTION_EXECUTE_HANDLER;
}
#else
static void PosixCrashSignalHandler(int SignalNumber)
{
	static std::atomic_bool bHandlingCrash = false;
	if (bHandlingCrash.exchange(true))
	{
		std::signal(SignalNumber, SIG_DFL);
		std::raise(SignalNumber);
		return;
	}

	xrLogger::EmergencyFlush();

	char Header[128] = {};
	std::snprintf(Header, sizeof(Header), "FATAL ERROR: Unhandled signal %d", SignalNumber);
	xrLogger::WriteCrashLine("");
	xrLogger::WriteCrashLine(Header);
	xrLogger::WriteCrashLine("----------------------------------------------");
	xrLogger::WriteCrashLine("stack trace:");

	char* NullBuffer = nullptr;
	CaptureCurrentStackTrace(NullBuffer, nullptr, 0, "\n", true);

	xrLogger::WriteCrashLine("----------------------------------------------");
	xrLogger::EmergencyFlush();

	std::signal(SignalNumber, SIG_DFL);
	std::raise(SignalNumber);
}
#endif

//////////////////////////////////////////////////////////////////////
void _terminate()
{
	string4096 assertion_info = {};

	Debug.gather_info(
		"<no expression>",
		"Unexpected application termination",
		nullptr,
		nullptr,
#ifdef ANONYMOUS_BUILD
		"",
		0,
#else
		__FILE__,
		__LINE__,
#endif
		__FUNCTION__,
		assertion_info
	);

	xrLogger::EmergencyFlush();

	if (strstr(GetCommandLineA(), "-silent_error_mode"))
	{
		exit(-1);
	}

	const char* endline = "\r\n";
	char* buffer = assertion_info + xr_strlen(assertion_info);
	AppendBufferText(buffer, assertion_info, sizeof(assertion_info), "Press OK to abort execution");
	AppendBufferText(buffer, assertion_info, sizeof(assertion_info), endline);

	extern XRCORE_API bool ignore_error_window;
	if (!ignore_error_window && !Debug.SilentErrorMode)
	{
		CrashWindow::Show("IX-Ray process has crashed", assertion_info);
	}

	xrLogger::EmergencyFlush();
	exit(-1);
}

#ifdef IXR_WINDOWS
IC void handler_base(const char* reason_string)
{
	bool skip;
	Debug.backend("Error handler is invoked!", reason_string, nullptr, nullptr, DEBUG_INFO, skip);
}

void invalid_parameter_handler(const wchar_t* expression, const wchar_t* function, const wchar_t* file, unsigned int line, uintptr_t reserved)
{
	string4096	expression_,
		function_,
		file_;

	size_t converted_chars = 0;

	if (expression)
		wcstombs_s(&converted_chars, expression_, sizeof(expression_), expression, (wcslen(expression) + 1) * 2 * sizeof(char));
	else
		xr_strcpy(expression_, "");

	if (function)
		wcstombs_s(&converted_chars, function_, sizeof(function_), function, (wcslen(function) + 1) * 2 * sizeof(char));
	else
		xr_strcpy(function_, __FUNCTION__);

	if (file)
		wcstombs_s(&converted_chars, file_, sizeof(file_), file, (wcslen(file) + 1) * 2 * sizeof(char));
	else
	{
		line = __LINE__;
		xr_strcpy(file_, __FILE__);
	}

	bool skip;
	Debug.backend("Error handler is invoked!", expression_, nullptr, nullptr, file_, line, function_, skip);
}
#endif

void __cdecl debug_on_thread_spawn(void)
{
	std::set_terminate(&_terminate);
#ifdef IXR_WINDOWS
	SetUnhandledExceptionFilter(UnhandledFilter);

	auto abort_handler = [](int signal) { handler_base("Application is aborting"); };
	auto floating_point_handler = [](int signal) { handler_base("Floating point error"); };
	auto pure_call_handler = []() { handler_base("Pure virtual function call"); };
	auto illegal_instruction_handler = [](int signal) { handler_base("Illegal instruction"); };

	signal(SIGABRT, abort_handler);
	signal(SIGFPE, floating_point_handler);
	signal(SIGILL, illegal_instruction_handler);

	_set_invalid_parameter_handler(&invalid_parameter_handler);

	_set_new_mode(1);
	_set_new_handler(&out_of_memory_handler);

	_set_purecall_handler(pure_call_handler);
#endif
}

void xrDebug::_initialize(bool dedicated)
{
	PROF_EVENT("xrDebug::_initialize");
	if (dedicated)
	{
		SilentErrorMode = true;
	}
	*g_bug_report_file = 0;
	std::set_terminate(&_terminate);
#ifdef IXR_WINDOWS
	previous_filter = ::SetUnhandledExceptionFilter(UnhandledFilter);	// exception handler to all "unhandled" exceptions
#else
	std::signal(SIGSEGV, &PosixCrashSignalHandler);
	std::signal(SIGABRT, &PosixCrashSignalHandler);
	std::signal(SIGFPE, &PosixCrashSignalHandler);
	std::signal(SIGILL, &PosixCrashSignalHandler);
	std::signal(SIGBUS, &PosixCrashSignalHandler);
#endif
}

ClipboardMessageCallback& ClipboardMessageCallback::instance()
{
	static ClipboardMessageCallback s_instance;
	return s_instance;
}
