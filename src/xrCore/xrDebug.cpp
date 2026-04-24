#include "stdafx.h"
#include "EventManager.h"
#include "xrDebug.h"

#include "EngineExternal.h"
#include "os_clipboard.h"

#pragma warning(push)
#pragma warning(disable:4995)
#if defined(_MSC_VER) && !defined(IXR_ARM64) && !defined(__clang__)
#	include <direct.h>
#	include <dxerr.h>
#endif
#pragma warning(pop)

extern bool shared_str_initialized;
XRCORE_API xrDebug Debug;

#define DEBUG_INVOKE	__debugbreak();
#define USE_OWN_ERROR_MESSAGE_WINDOW

#ifndef DEBUG
#	define USE_OWN_MINI_DUMP
#endif // DEBUG


static bool	error_after_dialog = false;

void xrDebug::gather_info		(const char *expression, const char *description, const char *argument0, const char *argument1, const char *file, int line, const char *function, LPSTR assertion_info, u32 const assertion_info_size)
{
	LPSTR				buffer_base = assertion_info;
	LPSTR				buffer = assertion_info;
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
			if (shared_str_initialized) {
				Msg		("%s",assertion_info);
				xrLogger::FlushLog();
			}
			buffer		= assertion_info;
			endline		= "\r\n";
			prefix		= "";
		}
	}

	if (SendErrorCallback)
	{
		SendErrorCallback(assertion_info);
	}

	if (!ignore_error_window && !IsDebuggerPresent() && !strstr(GetCommandLineA(), "-no_call_stack_assert")) {
		if (shared_str_initialized)
			Msg			("stack trace:\n");

#ifdef USE_OWN_ERROR_MESSAGE_WINDOW
		buffer			+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"stack trace:%s%s",endline,endline);
#endif // USE_OWN_ERROR_MESSAGE_WINDOW

#if USE_CXX_STACKTRACE
		int frame_i = 0;
		const auto& stack = std::stacktrace::current();
		for (const std::stacktrace_entry& entry : stack)
		{
			const std::string& source_file = entry.source_file();
			if (source_file.empty())
				continue;

			if (source_file.contains("vctools\\crt\\vcstartup")
				|| source_file.contains("VC\\Tools\\MSVC"))
				continue;

			const std::string& description = entry.description();
			if (description.contains("xrDebug::gather_info+")
				|| description.contains("xrDebug::backend+")
				|| description.contains("xrDebug::fail+"))
				continue;

#ifdef USE_OWN_ERROR_MESSAGE_WINDOW
			buffer += xr_sprintf(buffer, assertion_size - u32(buffer - buffer_base), "Frame %d: %s - %s:%d%s", ++frame_i, description.c_str(), source_file.c_str(), entry.source_line(), endline);
#endif // USE_OWN_ERROR_MESSAGE_WINDOW

			Msg("Frame %d: %s - %s:%d\n", frame_i, description.c_str(), source_file.c_str(), entry.source_line());
		}
#endif

		if (shared_str_initialized)
			xrLogger::FlushLog	();

		os_clipboard::copy_to_clipboard	(assertion_info);
	}
}

void xrDebug::do_exit	(const std::string &message)
{
	extern XRCORE_API bool ignore_error_window;
	xrLogger::FlushLog			();
	if (!SilentErrorMode)
	{
		
		if(!ignore_error_window)
			SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "Error", message.c_str(), nullptr);
	}

#ifdef IXR_WINDOWS
	if (!ignore_error_window)
	TerminateProcess	(GetCurrentProcess(),1);
#else
	kill(getpid(), SIGKILL);
#endif
}

void xrDebug::backend	(const char *expression, const char *description, const char *argument0, const char *argument1, const char *file, int line, const char *function, bool &ignore_always)
{
	PROF_EVENT("xrDebug::backend");
	static xrCriticalSection CS;

	CS.Enter			();

	error_after_dialog	= true;

	string4096			assertion_info;

	gather_info			(expression, description, argument0, argument1, file, line, function, assertion_info, sizeof(assertion_info) );

#ifdef USE_OWN_ERROR_MESSAGE_WINDOW
	const char*				endline = "\r\n";
	LPSTR				buffer = assertion_info + xr_strlen(assertion_info);
	buffer				+= xr_sprintf(buffer,sizeof(assertion_info) - u32(buffer - &assertion_info[0]),"%sPress CANCEL to abort execution%s",endline,endline);
	buffer				+= xr_sprintf(buffer,sizeof(assertion_info) - u32(buffer - &assertion_info[0]),"Press TRY AGAIN to continue execution%s",endline);
	buffer				+= xr_sprintf(buffer,sizeof(assertion_info) - u32(buffer - &assertion_info[0]),"Press CONTINUE to continue execution and ignore all the errors of this type%s%s",endline,endline);
#endif // USE_OWN_ERROR_MESSAGE_WINDOW

	if ( g_pEventManager == nullptr || g_pEventManager->IsEventThread())
	{
		show_dialog(assertion_info, ignore_always);
	}
	else
	{
		static std::string LastError = "";
		LastError = assertion_info;

		g_pEventManager->Event.Defer("KERNEL:assert", (size_t)&LastError, (size_t)&ignore_always);

		if (IsDebuggerPresent() && !SilentErrorMode)
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
			DEBUG_INVOKE;
		}
	}
	else if (buttonid == 4)
	{
		error_after_dialog = false;
		ignore_always = true;
		if (IsDebuggerPresent())
		{
			DEBUG_INVOKE;
		}
	}
#endif
	else
	{
		if(!ignore_error_window)
		{
			if (IsDebuggerPresent())
			{
				DEBUG_INVOKE;
			}
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
		MoveMemory	(szDumpPath,szDumpPath+5,strlen(szDumpPath));
		hFile		= ::CreateFileA( szDumpPath, GENERIC_WRITE, FILE_SHARE_WRITE, nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr );
	}
	if (hFile != INVALID_HANDLE_VALUE)
	{
		_MINIDUMP_EXCEPTION_INFORMATION ExInfo;
			
		ExInfo.ThreadId = ::GetCurrentThreadId();
		ExInfo.ExceptionPointers = pExceptionInfo;
		ExInfo.ClientPointers = false;

		// write the dump
		MINIDUMP_TYPE dump_flags = MINIDUMP_TYPE(MiniDumpNormal | MiniDumpFilterMemory | MiniDumpScanMemory | MiniDumpWithDataSegs | MiniDumpWithThreadInfo | MiniDumpWithFullMemoryInfo);

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
	LPVOID		message;
	DWORD		error_code = GetLastError(); 

	if (!error_code) {
		*buffer	= 0;
		return;
	}

	FormatMessageA(
		FORMAT_MESSAGE_ALLOCATE_BUFFER | 
		FORMAT_MESSAGE_FROM_SYSTEM,
		nullptr,
		error_code,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPSTR)&message,
		0,
		nullptr
	);

	xr_sprintf	(buffer,buffer_size,"[error][%8d]    : %s",error_code,message);
	LocalFree	(message);
}

#include <errorrep.h>

#include "StackTrace/StackTrace.h"
static bool EnabledStackTrace = true;

void ProcessStackTrace(_EXCEPTION_POINTERS *pExceptionInfo)
{
	
	string256				error_message;
	format_message			(error_message,sizeof(error_message));

	if (EnabledStackTrace)
	{
		CONTEXT save = *pExceptionInfo->ContextRecord;

		using namespace StackTrace;
		std::vector<std::string> stackTrace = BuildStackTrace(pExceptionInfo->ContextRecord, 1024);
		*pExceptionInfo->ContextRecord = save;
		Msg("\n----------------------------------------------");
		Msg("stack trace:\n");

		string4096			buffer;

		for (size_t i = 0; i < stackTrace.size(); i++)
		{
			Msg(stackTrace[i].c_str());
			xr_sprintf(buffer, sizeof(buffer), "%s\r\n", stackTrace[i].c_str());
		}

		Msg("----------------------------------------------\n\n");

		if (*error_message)
		{
			//if (shared_str_initialized)
			Msg("\n%s", error_message);
			if (EngineExternal()[EEngineExternalSystem::CustomMessageInClipboardOnCrash])
			{
				xr_stack_string256 ClipboardMessage = "Please, provide a full log in bugreport";
				auto GetFunc = ClipboardMessageCallback::instance().GetFunc();
				if (GetFunc)
				{
					ClipboardMessage = GetFunc();
				}
				os_clipboard::update_clipboard(ClipboardMessage.c_str());
			} else
			{

				xr_strcat(error_message, sizeof(error_message), "\r\n");
				os_clipboard::update_clipboard(buffer);
			}
		}
	}	

	xrLogger::FlushLog();
}

LONG WINAPI UnhandledFilter	(_EXCEPTION_POINTERS *pExceptionInfo)
{
	ProcessStackTrace(pExceptionInfo);

#ifdef USE_OWN_MINI_DUMP
	save_mini_dump		(pExceptionInfo);
#endif // USE_OWN_MINI_DUMP

	if (!error_after_dialog) {
		if (Debug.get_on_dialog())
			Debug.get_on_dialog()	(true);

		//SDL_ShowWindow(g_AppInfo.Window);
		//SDL_MinimizeWindow(g_AppInfo.Window);
		extern XRCORE_API bool ignore_error_window;
		if(!ignore_error_window)
			SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "Fatal error", "Fatal error occured\n\nPress OK to abort program execution", nullptr);
	}

	ReportFault(pExceptionInfo, 0);

#ifdef USE_OWN_ERROR_MESSAGE_WINDOW
	if (Debug.get_on_dialog())
		Debug.get_on_dialog()		(false);
#endif // USE_OWN_ERROR_MESSAGE_WINDOW

	return EXCEPTION_EXECUTE_HANDLER;
}
#endif

//////////////////////////////////////////////////////////////////////
void _terminate()
{
	if (strstr(GetCommandLineA(), "-silent_error_mode"))
		exit(-1);

	string4096				assertion_info;

	Debug.gather_info(
		//gather_info				(
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

	const char* endline = "\r\n";
	LPSTR buffer = assertion_info + xr_strlen(assertion_info);
	buffer += xr_sprintf(buffer, xr_strlen(assertion_info), "Press OK to abort execution%s", endline);

	SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "Fatal Error", assertion_info, nullptr);
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
#ifdef IXR_WINDOWS
	previous_filter = ::SetUnhandledExceptionFilter(UnhandledFilter);	// exception handler to all "unhandled" exceptions
#endif
}

ClipboardMessageCallback& ClipboardMessageCallback::instance()
{
	static ClipboardMessageCallback s_instance;
	return s_instance;
}