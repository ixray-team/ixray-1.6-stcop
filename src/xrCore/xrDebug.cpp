#include "stdafx.h"
#pragma hdrstop

#include "xrdebug.h"
#include "os_clipboard.h"

#pragma warning(push)
#pragma warning(disable:4995)
#include <malloc.h>
#include <direct.h>
#pragma warning(pop)

extern bool shared_str_initialized;

#ifdef __BORLANDC__
    #	include "d3d9.h"
    #	include "d3dx9.h"
    #	include "D3DX_Wrapper.h"
    #	pragma comment(lib,"EToolsB.lib")
    #	define DEBUG_INVOKE	DebugBreak()
        static BOOL			bException	= TRUE;
#else
    #	define DEBUG_INVOKE	__debugbreak();
        static BOOL			bException	= FALSE;

	#	define USE_OWN_ERROR_MESSAGE_WINDOW
#endif

#include <dbghelp.h>						// MiniDump flags

#include <new.h>							// for _set_new_mode
#include <signal.h>							// for signals

#ifndef DEBUG
#	define USE_OWN_MINI_DUMP
#endif // DEBUG

XRCORE_API	xrDebug		Debug;

static bool	error_after_dialog = false;

HWND get_current_wnd()
{
	HWND hWnd = GetActiveWindow();
	if (hWnd == nullptr)
		hWnd = GetForegroundWindow();
	return hWnd;
}

void xrDebug::gather_info		(const char *expression, const char *description, const char *argument0, const char *argument1, const char *file, int line, const char *function, LPSTR assertion_info, u32 const assertion_info_size)
{
	LPSTR				buffer_base = assertion_info;
	LPSTR				buffer = assertion_info;
	int assertion_size	= (int)assertion_info_size;
	LPCSTR				endline = "\n";
	LPCSTR				prefix = "[error]";
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
				FlushLog();
			}
			buffer		= assertion_info;
			endline		= "\r\n";
			prefix		= "";
		}
	}

	if (!IsDebuggerPresent() && !strstr(GetCommandLineA(), "-no_call_stack_assert")) {
		if (shared_str_initialized)
			Msg			("stack trace:\n");

#ifdef USE_OWN_ERROR_MESSAGE_WINDOW
		buffer			+= xr_sprintf(buffer,assertion_size - u32(buffer - buffer_base),"stack trace:%s%s",endline,endline);
#endif // USE_OWN_ERROR_MESSAGE_WINDOW
		
		if (shared_str_initialized)
			FlushLog	();

		os_clipboard::copy_to_clipboard	(assertion_info);
	}
}

void xrDebug::do_exit	(const std::string &message)
{
	FlushLog			();
	ShowWindow(get_current_wnd(), SW_MINIMIZE);
	MessageBoxA			(NULL,message.c_str(),"Error",MB_OK|MB_ICONERROR|MB_SYSTEMMODAL);
	TerminateProcess	(GetCurrentProcess(),1);
}

void xrDebug::backend	(const char *expression, const char *description, const char *argument0, const char *argument1, const char *file, int line, const char *function, bool &ignore_always)
{
	static xrCriticalSection CS
#ifdef PROFILE_CRITICAL_SECTIONS
	(MUTEX_PROFILE_ID(xrDebug::backend))
#endif // PROFILE_CRITICAL_SECTIONS
	;

	CS.Enter			();

	error_after_dialog	= true;

	string4096			assertion_info;

	gather_info			(expression, description, argument0, argument1, file, line, function, assertion_info, sizeof(assertion_info) );

#ifdef USE_OWN_ERROR_MESSAGE_WINDOW
	LPCSTR				endline = "\r\n";
	LPSTR				buffer = assertion_info + xr_strlen(assertion_info);
	buffer				+= xr_sprintf(buffer,sizeof(assertion_info) - u32(buffer - &assertion_info[0]),"%sPress CANCEL to abort execution%s",endline,endline);
	buffer				+= xr_sprintf(buffer,sizeof(assertion_info) - u32(buffer - &assertion_info[0]),"Press TRY AGAIN to continue execution%s",endline);
	buffer				+= xr_sprintf(buffer,sizeof(assertion_info) - u32(buffer - &assertion_info[0]),"Press CONTINUE to continue execution and ignore all the errors of this type%s%s",endline,endline);
#endif // USE_OWN_ERROR_MESSAGE_WINDOW

	if (handler)
		handler			();

	if (get_on_dialog())
		get_on_dialog()	(true);

	FlushLog			();

#ifdef XRCORE_STATIC
	MessageBoxA			(NULL,assertion_info,"X-Ray error",MB_OK|MB_ICONERROR|MB_SYSTEMMODAL);
#else

	ShowWindow(get_current_wnd(), SW_MINIMIZE);

	int result = MessageBoxA(
		NULL, assertion_info, "Fatal Error",
		MB_CANCELTRYCONTINUE | MB_ICONERROR | MB_DEFBUTTON3 | MB_SYSTEMMODAL | MB_DEFAULT_DESKTOP_ONLY);

		switch (result) {
			case IDCANCEL : {
				if (IsDebuggerPresent()) {
					DEBUG_INVOKE;
				}
				// TODO: Maybe not correct
				exit(-1);
				break;
			}
			case IDTRYAGAIN : {
				error_after_dialog	= false;
				break;
			}
			case IDCONTINUE : {
				error_after_dialog	= false;
				ignore_always	= true;
				ShowWindow(get_current_wnd(), SW_SHOWNORMAL);
				break;
			}
			default: {
				Msg("! xrDebug::backend default reached");
				break;
			}
		}
#endif

	if (get_on_dialog())
		get_on_dialog()	(false);

	CS.Leave			();
}

LPCSTR xrDebug::error2string(long code) {
	static char desc_storage[1024] = {};
	FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM, nullptr, code, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), desc_storage, 0, nullptr);
	return desc_storage;
}

void xrDebug::error		(long hr, const char* expr, const char *file, int line, const char *function, bool &ignore_always)
{
	backend		(error2string(hr),expr,0,0,file,line,function,ignore_always);
}

void xrDebug::error		(long hr, const char* expr, const char* e2, const char *file, int line, const char *function, bool &ignore_always)
{
	backend		(error2string(hr),expr,e2,0,file,line,function,ignore_always);
}

void xrDebug::fail		(const char *e1, const char *file, int line, const char *function, bool &ignore_always)
{
	backend		("assertion failed",e1,0,0,file,line,function,ignore_always);
}

void xrDebug::fail		(const char *e1, const std::string &e2, const char *file, int line, const char *function, bool &ignore_always)
{
	backend		(e1,e2.c_str(),0,0,file,line,function,ignore_always);
}

void xrDebug::fail		(const char *e1, const char *e2, const char *file, int line, const char *function, bool &ignore_always)
{
	backend		(e1,e2,0,0,file,line,function,ignore_always);
}

void xrDebug::fail		(const char *e1, const char *e2, const char *e3, const char *file, int line, const char *function, bool &ignore_always)
{
	backend		(e1,e2,e3,0,file,line,function,ignore_always);
}

void xrDebug::fail		(const char *e1, const char *e2, const char *e3, const char *e4, const char *file, int line, const char *function, bool &ignore_always)
{
	backend		(e1,e2,e3,e4,file,line,function,ignore_always);
}

void __cdecl xrDebug::fatal(const char *file, int line, const char *function, const char* F,...)
{
	string1024	buffer;

	va_list		p;
	va_start	(p,F);
	vsprintf	(buffer,F,p);
	va_end		(p);

	bool		ignore_always = true;

	backend		("fatal error","<no expression>",buffer,0,file,line,function,ignore_always);
}

typedef void (*full_memory_stats_callback_type) ( );
XRCORE_API full_memory_stats_callback_type g_full_memory_stats_callback = 0;

int out_of_memory_handler	(size_t size)
{
	if ( g_full_memory_stats_callback )
		g_full_memory_stats_callback	( );
	else {
		Memory.mem_compact	();

		u32					process_heap	= mem_usage_impl(nullptr, nullptr);
		int					eco_strings		= (int)g_pStringContainer->stat_economy			();
		int					eco_smem		= (int)g_pSharedMemoryContainer->stat_economy	();
		Msg					("* [x-ray]: process heap[%d K]", process_heap / 1024);
		Msg					("* [x-ray]: economy: strings[%d K], smem[%d K]",eco_strings/1024,eco_smem);
	}

	Debug.fatal				(DEBUG_INFO,"Out of memory. Memory request: %d K",size/1024);
	return					1;
}

extern LPCSTR log_name();

XRCORE_API string_path g_bug_report_file;

#if 1
typedef LONG WINAPI UnhandledExceptionFilterType(struct _EXCEPTION_POINTERS *pExceptionInfo);
typedef LONG ( __stdcall *PFNCHFILTFN ) ( EXCEPTION_POINTERS * pExPtrs ) ;
extern "C" BOOL __stdcall SetCrashHandlerFilter ( PFNCHFILTFN pFn );

static UnhandledExceptionFilterType	*previous_filter = 0;

#ifdef USE_OWN_MINI_DUMP
typedef BOOL (WINAPI *MINIDUMPWRITEDUMP)(HANDLE hProcess, DWORD dwPid, HANDLE hFile, MINIDUMP_TYPE DumpType,
										 CONST PMINIDUMP_EXCEPTION_INFORMATION ExceptionParam,
										 CONST PMINIDUMP_USER_STREAM_INFORMATION UserStreamParam,
										 CONST PMINIDUMP_CALLBACK_INFORMATION CallbackParam
										 );

void save_mini_dump			(_EXCEPTION_POINTERS *pExceptionInfo)
{
	// firstly see if dbghelp.dll is around and has the function we need
	// look next to the EXE first, as the one in System32 might be old 
	// (e.g. Windows 2000)
	HMODULE hDll	= NULL;
	string_path		szDbgHelpPath;

	if (GetModuleFileNameA( NULL, szDbgHelpPath, _MAX_PATH ))
	{
		char *pSlash = strchr( szDbgHelpPath, '\\' );
		if (pSlash)
		{
			xr_strcpy	(pSlash+1, sizeof(szDbgHelpPath)-(pSlash - szDbgHelpPath), "DBGHELP.DLL" );
			hDll = ::LoadLibraryA( szDbgHelpPath );
		}
	}

	if (hDll==NULL)
	{
		// load any version we can
		hDll = ::LoadLibraryA( "DBGHELP.DLL" );
	}

	const char* szResult = NULL;

	if (hDll)
	{
		MINIDUMPWRITEDUMP pDump = (MINIDUMPWRITEDUMP)::GetProcAddress( hDll, "MiniDumpWriteDump" );
		if (pDump)
		{
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
			HANDLE hFile = ::CreateFileA( szDumpPath, GENERIC_WRITE, FILE_SHARE_WRITE, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
			if (INVALID_HANDLE_VALUE==hFile)	
			{
				// try to place into current directory
				MoveMemory	(szDumpPath,szDumpPath+5,strlen(szDumpPath));
				hFile		= ::CreateFileA( szDumpPath, GENERIC_WRITE, FILE_SHARE_WRITE, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
			}
			if (hFile!=INVALID_HANDLE_VALUE)
			{
				_MINIDUMP_EXCEPTION_INFORMATION ExInfo;

				ExInfo.ThreadId				= ::GetCurrentThreadId();
				ExInfo.ExceptionPointers	= pExceptionInfo;
				ExInfo.ClientPointers		= NULL;

				// write the dump
				MINIDUMP_TYPE	dump_flags	= MINIDUMP_TYPE(MiniDumpNormal | MiniDumpFilterMemory | MiniDumpScanMemory );

				BOOL bOK = pDump( GetCurrentProcess(), GetCurrentProcessId(), hFile, dump_flags, &ExInfo, NULL, NULL );
				if (bOK)
				{
					xr_sprintf( szScratch, "Saved dump file to '%s'", szDumpPath );
					szResult = szScratch;
//					retval = EXCEPTION_EXECUTE_HANDLER;
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
		else
		{
			szResult = "DBGHELP.DLL too old";
		}
	}
	else
	{
		szResult = "DBGHELP.DLL not found";
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
        NULL,
        error_code,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        (LPSTR)&message,
        0,
		NULL
	);

	xr_sprintf	(buffer,buffer_size,"[error][%8d]    : %s",error_code,message);
    LocalFree	(message);
}

#ifndef _EDITOR
    #include <errorrep.h>
    #pragma comment( lib, "faultrep.lib" )
#endif

LONG WINAPI BuildStackTrace(PEXCEPTION_POINTERS pExceptionInfo)
{
	HANDLE process = GetCurrentProcess();
	SymInitialize(process, NULL, TRUE);

	// StackWalk64() may modify context record passed to it, so we will
	// use a copy.
	CONTEXT context_record = *pExceptionInfo->ContextRecord;
	// Initialize stack walking.
	STACKFRAME64 stack_frame;
	memset(&stack_frame, 0, sizeof(stack_frame));

#if defined(_WIN64)
	int machine_type = IMAGE_FILE_MACHINE_AMD64;
	stack_frame.AddrPC.Offset = context_record.Rip;
	stack_frame.AddrFrame.Offset = context_record.Rbp;
	stack_frame.AddrStack.Offset = context_record.Rsp;
#else
	int machine_type = IMAGE_FILE_MACHINE_I386;
	stack_frame.AddrPC.Offset = context_record.Eip;
	stack_frame.AddrFrame.Offset = context_record.Ebp;
	stack_frame.AddrStack.Offset = context_record.Esp;
#endif
	stack_frame.AddrPC.Mode = AddrModeFlat;
	stack_frame.AddrFrame.Mode = AddrModeFlat;
	stack_frame.AddrStack.Mode = AddrModeFlat;

	char buffer[sizeof(SYMBOL_INFO) + MAX_SYM_NAME * sizeof(TCHAR)];
	const auto pSymbol = reinterpret_cast<PSYMBOL_INFO>(buffer);

	pSymbol->SizeOfStruct = sizeof(SYMBOL_INFO);
	pSymbol->MaxNameLen = MAX_SYM_NAME;

	Msg("%s", "Stack Trace : \n");

	while (StackWalk64(machine_type, GetCurrentProcess(), GetCurrentThread(), &stack_frame, &context_record, NULL, &SymFunctionTableAccess64, &SymGetModuleBase64, NULL))
	{
		DWORD64 displacement = 0;

		std::string Buffer = "";

		if (SymFromAddr(process, (DWORD64)stack_frame.AddrPC.Offset, &displacement, pSymbol))
		{
			IMAGEHLP_MODULE64 moduleInfo;
			FillMemory(&moduleInfo, sizeof(moduleInfo), 0);

			moduleInfo.SizeOfStruct = sizeof(moduleInfo);

			if (::SymGetModuleInfo64(process, pSymbol->ModBase, &moduleInfo))
				Buffer += moduleInfo.ModuleName;

			char buf[_MAX_U64TOSTR_BASE2_COUNT];
			_itoa_s((int)displacement, buf, _countof(buf), 16);

			Buffer += moduleInfo.ModuleName;
			Buffer += "!";
			Buffer += pSymbol->Name;
			Buffer += displacement;
			Buffer += "\r\n";
		}

		Msg("%s", Buffer.c_str());
	}

	return EXCEPTION_CONTINUE_SEARCH;
}

LONG WINAPI UnhandledFilter	(_EXCEPTION_POINTERS *pExceptionInfo)
{
	string256				error_message;
	format_message			(error_message,sizeof(error_message));

	BuildStackTrace(pExceptionInfo);

	if (shared_str_initialized)
		FlushLog			();

#ifdef USE_OWN_MINI_DUMP
	save_mini_dump		(pExceptionInfo);
#endif // USE_OWN_MINI_DUMP

	if (!error_after_dialog) {
		if (Debug.get_on_dialog())
			Debug.get_on_dialog()	(true);

		ShowWindow(get_current_wnd(), SW_MINIMIZE);
		MessageBoxA			(NULL,"Fatal error occured\n\nPress OK to abort program execution","Fatal error",MB_OK|MB_ICONERROR|MB_SYSTEMMODAL);
	}

#ifndef _EDITOR
	ReportFault				( pExceptionInfo, 0 );
#endif

#ifdef USE_OWN_ERROR_MESSAGE_WINDOW
	if (Debug.get_on_dialog())
		Debug.get_on_dialog()		(false);
#endif // USE_OWN_ERROR_MESSAGE_WINDOW

	return EXCEPTION_EXECUTE_HANDLER;
}
#endif

//////////////////////////////////////////////////////////////////////
#ifdef M_BORLAND
	namespace std{
		extern new_handler _RTLENTRY _EXPFUNC set_new_handler( new_handler new_p );
	};

	static void __cdecl def_new_handler() 
    {
		FATAL		("Out of memory.");
    }

    void	xrDebug::_initialize		(const bool &dedicated)
    {
		handler							= 0;
		m_on_dialog						= 0;
        std::set_new_handler			(def_new_handler);	// exception-handler for 'out of memory' condition
//		::SetUnhandledExceptionFilter	(UnhandledFilter);	// exception handler to all "unhandled" exceptions
    }
#else
    typedef int		(__cdecl * _PNH)( size_t );

	void _terminate		()
	{
		if (strstr(GetCommandLineA(),"-silent_error_mode"))
			exit				(-1);

		string4096				assertion_info;
		
		Debug.gather_info			(
		//gather_info				(
			"<no expression>",
			"Unexpected application termination",
			0,
			0,
	#ifdef ANONYMOUS_BUILD
			"",
			0,
	#else
			__FILE__,
			__LINE__,
	#endif
	#ifndef _EDITOR
			__FUNCTION__,
	#else // _EDITOR
			"",
	#endif // _EDITOR
			assertion_info
		);
		
		LPCSTR					endline = "\r\n";
		LPSTR					buffer = assertion_info + xr_strlen(assertion_info);
		buffer					+= xr_sprintf(buffer, xr_strlen(assertion_info), "Press OK to abort execution%s", endline);

		ShowWindow(get_current_wnd(), SW_MINIMIZE);

		MessageBoxA				(
			/*GetTopWindow(NULL)*/ nullptr,
			assertion_info,
			"Fatal Error",
			MB_OK|MB_ICONERROR|MB_SYSTEMMODAL
		);
		
		exit					(-1);
	}
	
	void __cdecl debug_on_thread_spawn(void)
	{
		SetUnhandledExceptionFilter(UnhandledFilter);
	}

    void	xrDebug::_initialize		(const bool &dedicated)
    {
		static bool is_dedicated		= dedicated;

		*g_bug_report_file				= 0;

		previous_filter					= ::SetUnhandledExceptionFilter(UnhandledFilter);	// exception handler to all "unhandled" exceptions

#if 0
		struct foo {static void	recurs	(const u32 &count)
		{
			if (!count)
				return;

			_alloca			(4096);
			recurs			(count - 1);
		}};
		foo::recurs			(u32(-1));
		std::terminate		();
#endif // 0
	}
#endif
