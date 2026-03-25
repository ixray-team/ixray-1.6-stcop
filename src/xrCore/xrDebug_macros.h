#ifndef xrDebug_macrosH
#define xrDebug_macrosH
#pragma once

#define DEBUG_INFO __FILE__,__LINE__,__FUNCTION__
#define _TRE(arg)  arg

#include "xrDebug.h"

#	define CHECK_OR_EXIT(expr,message)	do {if (!(expr)) ::Debug.do_exit(message);} while (0)

#define GET_5TH_ARG(arg1, arg2, arg3, arg4, arg5, ...) arg5
#define MACRO_CHOOSER_ASSERT(...)\
	GET_5TH_ARG(__VA_ARGS__, \
	R_ASSERT4, R_ASSERT3, R_ASSERT2, R_ASSERT1, )
#define MACRO_CHOOSER_IASSERT(...)\
	GET_5TH_ARG(__VA_ARGS__,\
	I_ASSERT4, I_ASSERT3, I_ASSERT2, I_ASSERT1, )

#ifdef SHIPPING_BUILD
#	define R_ASSERT(expr)
#	define R_ASSERT1(expr)
#	define R_ASSERT2(expr,e2)
#	define R_ASSERT3(expr,e2,e3)
#	define R_ASSERT4(expr,e2,e3,e4)
#	define R_CHK(expr) expr
#	define R_CHK2(expr,e2) expr

#	define I_ASSERT(...) MACRO_CHOOSER_IASSERT(__VA_ARGS__)(__VA_ARGS__)
#	define I_ASSERT1(expr) (!!(expr))
#	define I_ASSERT2(expr,e2) (!!(expr))
#	define I_ASSERT3(expr,e2,e3) (!!(expr))
#	define I_ASSERT4(expr,e2,e3,e4) (!!(expr))
#	define I_ASSERT_M(expr, ...) (!!(expr))
#else
#	define R_ASSERT(...) MACRO_CHOOSER_ASSERT(__VA_ARGS__)(__VA_ARGS__)
#	define R_ASSERT1(expr)				do {static bool ignore_always = false; if (!ignore_always && !(expr)) ::Debug.fail(_TRE(#expr),DEBUG_INFO,ignore_always);} while(0)
#	define R_ASSERT2(expr,e2)			do {static bool ignore_always = false; if (!ignore_always && !(expr)) ::Debug.fail(_TRE(#expr),_TRE(e2),DEBUG_INFO,ignore_always);} while(0)
#	define R_ASSERT3(expr,e2,e3)		do {static bool ignore_always = false; if (!ignore_always && !(expr)) ::Debug.fail(_TRE(#expr),_TRE(e2),_TRE(e3),DEBUG_INFO,ignore_always);} while(0)
#	define R_ASSERT4(expr,e2,e3,e4)		do {static bool ignore_always = false; if (!ignore_always && !(expr)) ::Debug.fail(_TRE(#expr),_TRE(e2),_TRE(e3),_TRE(e4),DEBUG_INFO,ignore_always);} while(0)
#	define R_CHK(expr)					do {static bool ignore_always = false; HRESULT __hr = expr; if (!ignore_always && FAILED(__hr)) ::Debug.error(__hr,_TRE(#expr),DEBUG_INFO,ignore_always);} while(0)
#	define R_CHK2(expr,e2)				do {static bool ignore_always = false; HRESULT hr = expr; if (!ignore_always && FAILED(hr)) ::Debug.error(hr,_TRE(#expr),_TRE(e2),DEBUG_INFO,ignore_always);} while(0)

#	if 0 // use old I_ASSERT like R_ASSERT
#	define I_ASSERT(...) MACRO_CHOOSER_IASSERT(__VA_ARGS__)(__VA_ARGS__)
#	define I_ASSERT1(expr) ((!!(expr)) || ([&](){ \
		static bool ignore_always = false;\
		if (!ignore_always){ \
			::Debug.fail(_TRE(#expr),DEBUG_INFO,ignore_always);\
		} \
		return false;\
	}()))
#	define I_ASSERT2(expr,e2) ((!!(expr)) || ([&](){ \
		static bool ignore_always = false;\
		if (!ignore_always){ \
			::Debug.fail(_TRE(#expr),_TRE(e2),DEBUG_INFO,ignore_always);\
		} \
		return false;\
	}()))
#	define I_ASSERT3(expr,e2,e3) ((!!(expr)) || ([&](){ \
		static bool ignore_always = false;\
		if (!ignore_always){ \
			::Debug.fail(_TRE(#expr),_TRE(e2),_TRE(e3),DEBUG_INFO,ignore_always);\
		} \
		return false;\
	}()))
#	define I_ASSERT4(expr,e2,e3,e4) ((!!(expr)) || ([&](){ \
		static bool ignore_always = false;\
		if (!ignore_always){ \
			::Debug.fail(_TRE(#expr),_TRE(e2),_TRE(e3),_TRE(e4),DEBUG_INFO,ignore_always);\
		} \
		return false;\
	}()))
#else
#	define I_ASSERT(expr) ((!!(expr)) || ([&](){ \
		static bool ignore_always = false;\
		if (!ignore_always){ \
			::Debug.fail(_TRE(#expr),DEBUG_INFO,ignore_always);\
		} \
		return false;\
	}()))
#	define I_ASSERT_M(expr, message, ...) ((!!(expr)) || ([&](){ \
		static bool ignore_always = false;\
		if (!ignore_always){ \
			string1024 buff; \
			xr_sprintf(buff, message, ##__VA_ARGS__); \
			::Debug.fail(_TRE(#expr),buff,DEBUG_INFO,ignore_always);\
		} \
		return false;\
	}()))
#endif
#endif

#	define FATAL(description)			Debug.fatal(DEBUG_INFO,description)

#	ifdef VERIFY
#		undef VERIFY
#	endif // VERIFY

#	define MACRO_CHOOSER_IVERIFY(...)\
	GET_5TH_ARG(__VA_ARGS__, \
	IVERIFY4, IVERIFY3, IVERIFY2, IVERIFY1, )

#ifdef DEBUG
#	define MACRO_CHOOSER_VERIFY(...)\
		GET_5TH_ARG(__VA_ARGS__, \
		VERIFY4, VERIFY3, VERIFY2, VERIFY1, )
#	define NODEFAULT				FATAL("nodefault reached")
#	define VERIFY(...) MACRO_CHOOSER_VERIFY(__VA_ARGS__)(__VA_ARGS__)
#	define VERIFY1(expr)			do {static bool ignore_always = false; if (!ignore_always && !(expr)) ::Debug.fail(#expr,DEBUG_INFO,ignore_always);} while(0)
#	define VERIFY2(expr,e2)		do {static bool ignore_always = false; if (!ignore_always && !(expr)) ::Debug.fail(#expr,e2,DEBUG_INFO,ignore_always);} while(0)
#	define VERIFY3(expr,e2,e3)	do {static bool ignore_always = false; if (!ignore_always && !(expr)) ::Debug.fail(#expr,e2,e3,DEBUG_INFO,ignore_always);} while(0)
#	define VERIFY4(expr,e2,e3,e4)do {static bool ignore_always = false; if (!ignore_always && !(expr)) ::Debug.fail(#expr,e2,e3,e4,DEBUG_INFO,ignore_always);} while(0)
#	define CHK_DX(expr)				do {static bool ignore_always = false; HRESULT __hr = expr; if (!ignore_always && FAILED(__hr)) ::Debug.error_dx(__hr,#expr,DEBUG_INFO,ignore_always);} while(0)

#	if 0 // use old I_ASSERT like R_ASSERT
#	define IVERIFY(...) MACRO_CHOOSER_IVERIFY(__VA_ARGS__)(__VA_ARGS__)
#	define IVERIFY1(expr) ((!!(expr)) || ([&](){ \
		static bool ignore_always = false;\
		if (!ignore_always){ \
			::Debug.fail(_TRE(#expr),DEBUG_INFO,ignore_always);\
		} \
		return false;\
	}()))
#	define IVERIFY2(expr,e2) ((!!(expr)) || ([&](){ \
		static bool ignore_always = false;\
		if (!ignore_always){ \
			::Debug.fail(_TRE(#expr),_TRE(e2),DEBUG_INFO,ignore_always);\
		} \
		return false;\
	}()))
#	define IVERIFY3(expr,e2,e3) ((!!(expr)) || ([&](){ \
		static bool ignore_always = false;\
		if (!ignore_always){ \
			::Debug.fail(_TRE(#expr),_TRE(e2),_TRE(e3),DEBUG_INFO,ignore_always);\
		} \
		return false;\
	}()))
#	define IVERIFY4(expr,e2,e3,e4) ((!!(expr)) || ([&](){ \
		static bool ignore_always = false;\
		if (!ignore_always){ \
			::Debug.fail(_TRE(#expr),_TRE(e2),_TRE(e3),_TRE(e4),DEBUG_INFO,ignore_always);\
		} \
		return false;\
	}()))
#else
#	define IVERIFY(expr) ((!!(expr)) || ([&](){ \
		static bool ignore_always = false;\
		if (!ignore_always){ \
			::Debug.fail(_TRE(#expr),DEBUG_INFO,ignore_always);\
		} \
		return false;\
	}()))
#	define IVERIFY_M(expr, message, ...) ((!!(expr)) || ([&](){ \
		static bool ignore_always = false;\
		if (!ignore_always){ \
			string1024 buff; \
			xr_sprintf(buff, message, __VA_ARGS__); \
			::Debug.fail(_TRE(#expr),buff,DEBUG_INFO,ignore_always);\
		} \
		return false;\
	}()))
#endif
#else // DEBUG
#	if defined(_MSC_VER)
#		define NODEFAULT __assume(0)
#	else
#		include <cassert>
#		define NODEFAULT assert(false)
#	endif

#	define VERIFY(...)				do {} while (0)
#	define VERIFY2(expr, e2)		do {} while (0)
#	define VERIFY3(expr, e2, e3)	do {} while (0)
#	define VERIFY4(expr, e2, e3, e4)do {} while (0)
#	define CHK_DX(a) a
#	define IVERIFY(...) MACRO_CHOOSER_IVERIFY(__VA_ARGS__)(__VA_ARGS__)
#	define IVERIFY1(expr) (!!(expr))
#	define IVERIFY2(expr,e2) (!!(expr))
#	define IVERIFY3(expr,e2,e3) (!!(expr))
#	define IVERIFY4(expr,e2,e3,e4) (!!(expr))
#	define IVERIFY_M(expr, ...) (!!(expr))
#endif // DEBUG
//---------------------------------------------------------------------------------------------
// FIXMEs / TODOs / NOTE macros
//---------------------------------------------------------------------------------------------
#define _QUOTE(x) # x
#define QUOTE(x) _QUOTE(x)
#define __FILE__LINE__ __FILE__ "(" QUOTE(__LINE__) ") : "

#define NOTE( x )  message( x )
#define FILE_LINE  message( __FILE__LINE__ )

#define TODO( x )  message( __FILE__LINE__"\n"           \
	" ------------------------------------------------\n" \
	"|  TODO :   " #x "\n" \
	" -------------------------------------------------\n" )
#define FIXME( x )  message(  __FILE__LINE__"\n"           \
	" ------------------------------------------------\n" \
	"|  FIXME :  " #x "\n" \
	" -------------------------------------------------\n" )
#define todo( x )  message( __FILE__LINE__" TODO :   " #x "\n" ) 
#define fixme( x )  message( __FILE__LINE__" FIXME:   " #x "\n" ) 
#endif // xrDebug_macrosH