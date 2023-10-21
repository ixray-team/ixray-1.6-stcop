#include "stdafx.h"
#pragma hdrstop

#include <time.h>
#include "resource.h"
#include "log.h"
#ifdef _EDITOR
	#include "malloc.h"
#endif

extern BOOL					LogExecCB		= TRUE;
static string_path			logFName		= "engine.log";
static string_path			log_file_name	= "engine.log";
static BOOL 				no_log			= TRUE;
#ifdef PROFILE_CRITICAL_SECTIONS
	static xrCriticalSection	logCS(MUTEX_PROFILE_ID(log));
#else // PROFILE_CRITICAL_SECTIONS
	static xrCriticalSection	logCS;
#endif // PROFILE_CRITICAL_SECTIONS
xr_vector<shared_str>*		LogFile			= NULL;
static LogCallback			LogCB			= 0;

void FlushLog			()
{
	if (!no_log){
		logCS.Enter			();
		IWriter *f			= FS.w_open(logFName);
        if (f) {
            for (u32 it=0; it<LogFile->size(); it++)	{
				LPCSTR		s	= *((*LogFile)[it]);
				f->w_string	(s?s:"");
			}
            FS.w_close		(f);
        }
		logCS.Leave			();
    }
}

void AddOne(const char *split) 
{
	if(!LogFile)		
						return;

	logCS.Enter			();

	if (IsDebuggerPresent()) {
		OutputDebugStringA(split);
		OutputDebugStringA("\n");
	}

//	DUMP_PHASE;
	{
		shared_str			temp = shared_str(split);
//		DUMP_PHASE;
		LogFile->push_back	(temp);
	}

	//exec CallBack
	if (LogExecCB&&LogCB)LogCB(split);

	logCS.Leave				();
}

XRCORE_API void CorrectLog(const char *s)
{
	int		i,j;

	u32			length = xr_strlen( s );
#ifndef _EDITOR    
	PSTR split  = (PSTR)_alloca( (length + 1) * sizeof(char) );
#else
	PSTR split  = (PSTR)alloca( (length + 1) * sizeof(char) );
#endif
	for (i=0,j=0; s[i]!=0; i++) {
		if (s[i]=='\n') {
			split[j]=0;	// end of line
			if (split[0]==0) { split[0]=' '; split[1]=0; }
			AddOne(split);
			j=0;
		} else {
			split[j++]=s[i];
		}
	}
	split[j]=0;
	AddOne(split);
}

void LogWinErr(const char *msg, long err_code)
{
	EngineLog("{}: {}", msg, Debug.error2string(err_code));
}

LogCallback SetLogCB	(LogCallback cb)
{
	LogCallback	result	= LogCB;
	LogCB				= cb;
	return				(result);
}

LPCSTR log_name			()
{
	return				(log_file_name);
}

void InitLog()
{
	R_ASSERT			(LogFile==NULL);
	LogFile				= xr_new< xr_vector<shared_str> >();
	LogFile->reserve	(1000);
}

void CreateLog			(BOOL nl)
{
    no_log				= nl;
	strconcat			(sizeof(log_file_name),log_file_name,Core.ApplicationName,"_",Core.UserName,".log");
	if (FS.path_exist("$logs$"))
		FS.update_path	(logFName,"$logs$",log_file_name);
	if (!no_log){
        IWriter *f		= FS.w_open	(logFName);
        if (f==NULL){
        	MessageBoxA	(NULL,"Can't create log file.","Error",MB_ICONERROR);
        	abort();
        }
        FS.w_close		(f);
    }
}

void CloseLog(void)
{
	FlushLog		();
 	LogFile->clear	();
	xr_delete		(LogFile);
}
