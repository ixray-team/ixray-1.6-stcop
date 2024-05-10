#pragma once

#pragma warning( 4 : 4018 )
#pragma warning( 4 : 4244 )
#pragma warning(disable:4505)

#if XRAY_EXCEPTIONS
#	define	THROW(expr)				do {if (!(expr)) {string4096	assertion_info; ::Debug.gather_info(_TRE(#expr),   0,   0,0,DEBUG_INFO,assertion_info); throw assertion_info;}} while(0)
#	define	THROW2(expr,msg0)		do {if (!(expr)) {string4096	assertion_info; ::Debug.gather_info(_TRE(#expr),msg0,   0,0,DEBUG_INFO,assertion_info); throw assertion_info;}} while(0)
#	define	THROW3(expr,msg0,msg1)	do {if (!(expr)) {string4096	assertion_info; ::Debug.gather_info(_TRE(#expr),msg0,msg1,0,DEBUG_INFO,assertion_info); throw assertion_info;}} while(0)
#else
#	define	THROW					VERIFY
#	define	THROW2					VERIFY2
#	define	THROW3					VERIFY3
#endif

#ifndef _PP_EDITOR_
#pragma warning(disable:4995)
#include "../xrEngine/stdafx.h"
#include "../xrScripts/stdafx.h"
#pragma warning(default:4995)

#include "../xrEngine/gamefont.h"
#include "../xrEngine/xr_object.h"
#include "../xrEngine/igame_level.h"
#include "../xrphysics/xrphysics.h"
#include "smart_cast.h"
#endif

#undef min
#undef max