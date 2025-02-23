#pragma once
#include <SDL3/SDL.h>
#include "profiler.h"
#include "Platform/Platform.h"

// Our headers
#ifdef XRCORE_STATIC
#	define XRCORE_API
#else
#	ifdef XRCORE_EXPORTS
#		define XRCORE_API __declspec(dllexport)
#	else
#		define XRCORE_API __declspec(dllimport)
#	endif
#endif

#define IC inline

#define _inline			inline
#define __inline		inline
#define ICF				__forceinline			// !!! this should be used only in critical places found by PROFILER
#define ICN		    	__declspec (noinline)

#ifndef DEBUG
	#pragma inline_depth	( 254 )
	#pragma inline_recursion( on )
#endif

#pragma intrinsic(abs, fabs, fmod, sin, cos, tan, asin, acos, atan, sqrt, exp, log, log10, strcat)

#include <filesystem>
#include <time.h>
// work-around dumb borland compiler

// Warnings
#pragma warning (disable : 4251 )		// object needs DLL interface
#pragma warning (disable : 4201 )		// nonstandard extension used : nameless struct/union
#pragma warning (disable : 4100 )		// unreferenced formal parameter
#pragma warning (disable : 4127 )		// conditional expression is constant
//#pragma warning (disable : 4530 )		// C++ exception handler used, but unwind semantics are not enabled
#pragma warning (disable : 4345 )
#pragma warning (disable : 4714 )		// __forceinline not inlined
#ifndef DEBUG
#pragma warning (disable : 4189 )		//  local variable is initialized but not refenced
#endif									//	frequently in release code due to large amount of VERIFY

#ifdef IXR_X64
#pragma warning (disable : 4512 )
#endif

// posix
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <fcntl.h>

// FSD
#include <fast_dynamic_cast/fast_dynamic_cast.hpp>
#define smart_cast fast_dynamic_cast

// stl
#pragma warning (push)
#pragma warning (disable:4702)
#include <typeinfo>
#include <algorithm>
#include <limits>
#include <regex>
#include <vector>
#include <queue>
#include <stack>
#include <list>
#include <set>
#include <map>
#include <unordered_map>
#include <unordered_set>
#include <bitset>
#include <chrono>
#include <string>

#pragma warning (pop)
#pragma warning (disable : 4100 )		// unreferenced formal parameter

// Engine
#include "Platform/PlatformAPI.h"
#include "xr_delegate.h"
#include "_noncopyable.h"

#include "xrDebug.h"
#include "vector.h"

#include "clsid.h"
#include "xrSyncronize.h"
#include "RingBuffer.h"
#include "xrMemory.h"
#include "xrDebug.h"

#include "_stl_extensions.h"
#include "xrsharedmem.h"
#include "shared_string.h"
#include "xrstring.h"
#include "_thread_types.h"
#include "xr_resource.h"
#include "rt_compressor.h"
#include "xr_shared.h"
#include "string_concatenations.h"
#include "xr_path.h"
#include "stack_string.h"

// stl ext
struct XRCORE_API xr_rtoken
{
    shared_str	name;
    int	   	id;
           	xr_rtoken	(LPCSTR _nm, int _id){name=_nm;id=_id;}
public:
    void	rename		(LPCSTR _nm)		{name=_nm;}
    bool	equal		(LPCSTR _nm)		{return (0==xr_strcmp(*name,_nm));}
};

#pragma pack (push,1)
struct XRCORE_API xr_shortcut
{
    enum
    {
        flShift	= 0x20,
        flCtrl	= 0x40,
        flAlt	= 0x80,
    };
    union
    {
    	struct
        {
            u8	 	key;
            Flags8	ext;
        };
        u16		hotkey;
    };
                xr_shortcut		(u8 k, BOOL a, BOOL c, BOOL s):key(k){ext.assign(u8((a?flAlt:0)|(c?flCtrl:0)|(s?flShift:0)));}
                xr_shortcut		(){ext.zero();key=0;}
    bool		similar			(const xr_shortcut& v)const{return ext.equal(v.ext)&&(key==v.key);}
};
#pragma pack (pop)

using RStringVec = xr_vector<shared_str>;
using RStringVecIt = RStringVec::iterator;

using RStringSet = xr_set<shared_str>;
using RStringSetIt = RStringSet::iterator;

using RTokenVec = xr_vector<xr_rtoken>;
using RTokenVecIt = RTokenVec::iterator;

#include "TimeUtils.h"
#include "xr_delegate.h"

#include "FS.h"
#include "log.h"
#include "xr_trims.h"
#include "xr_ini.h"
#include "appinfo.h"
#include "LocatorAPI.h"
#include "FileSystem.h"
#include "FTimer.h"
#include "intrusive_ptr.h"

#include "net_utils.h"
#include "xrParams.h"

// destructor
template <class T>
class destructor
{
	T* ptr;
public:
	destructor(T* p)	{ ptr=p;			}
	~destructor()		{ xr_delete(ptr);	}
	IC T& operator() ()
	{	return *ptr; }
};

// ********************************************** The Core definition
class XRCORE_API xrCore 
{
public:
	string64	ApplicationName;
	string_path	ApplicationPath;
	string_path	WorkingPath;
	string64	UserName;
	string64	CompName;
	string512	Params;
    Flags64     ParamsData;

public:
	void _initialize	(LPCSTR ApplicationName, xrLogger::LogCallback cb=0, BOOL init_fs=TRUE, LPCSTR fs_fname=0);
	void _destroy	    ();
};

//Borland class dll interface
#define	_BCL

//Borland global function dll interface
#define	_BGCL	

#include <DirectXMath.h>
namespace Platform
{
    XRCORE_API xr_string TCHAR_TO_ANSI_U8(const xr_special_char* C);
    XRCORE_API xr_string CP_TCHAR_TO_ANSI_U8(const xr_special_char* C);
    XRCORE_API xr_string UTF8_to_CP1251(xr_string const& utf8);
    XRCORE_API xr_string ANSI_TO_UTF8(const xr_string& ansi);
}

extern XRCORE_API xrCore Core;