//----------------------------------------------------
// file: stdafx.h
//----------------------------------------------------
#ifndef __INCDEF_STDAFX_H_
#define __INCDEF_STDAFX_H_

#pragma once

#define _WIN32_WINNT 0x0500    
#define IC inline



#pragma warning (disable:4995)
#include "Max.h"

/*
*	xray-re
*/
#include <xr_types.h>
#include <xr_file_system.h>
#include <xr_log.h>
#include <xr_string_utils.h>

/*
#undef _MIN
#undef _MAX
#define _MIN(a,b)		(a)<(b)?(a):(b)
#define _MAX(a,b)		(a)>(b)?(a):(b)
template <class T>
T min(T a, T b) { return _MIN(a,b); }
template <class T>
T max(T a, T b) { return _MAX(a,b); }

#undef _MIN
#undef _MAX

#define FLT_MAX flt_max

#ifdef FLT_MIN
#undef FLT_MIN
#endif

#define FLT_MIN flt_max
*/

#include <io.h>
#include <sys\stat.h>
#include <fcntl.h>
#include <sys\utime.h>

#include "istdplug.h"
#include "iparamb2.h"
#include "iparamm2.h"
#include "stdmat.h"
#include "UTILAPI.H"

#include <d3d9types.h>

#define ENGINE_API
#define ECORE_API

enum TMsgDlgType { mtWarning, mtError, mtInformation, mtConfirmation, mtCustom };
enum TMsgDlgBtn { mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp };
typedef TMsgDlgBtn TMsgDlgButtons[mbHelp];

/*
 *	std
 */
#include <string>
#include <vector>
using std::string;
using namespace xray_re;



#define AnsiString string
TYPEDEF_STD_VECTOR(AnsiString)
TYPEDEF_STD_VECTOR(LPSTR)

//#include "clsid.h"
//#include "Engine.h"
//#include "Properties.h"
//#include "..\..\Shared\ELog.h"

#define THROW xr_assert(0)

#define GAMEMTL_NONE		u32(-1)
#define _game_data_ "$game_data$"

#pragma warning (default:4995)

#endif /*_INCDEF_STDAFX_H_*/





