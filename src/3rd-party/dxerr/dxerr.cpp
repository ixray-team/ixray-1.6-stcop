//--------------------------------------------------------------------------------------
// File: DXErr.cpp
//
// DirectX Error Library
//
// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
// ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
// PARTICULAR PURPOSE.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//--------------------------------------------------------------------------------------
#include "dxerr.h"

#include <stdio.h>
#include <algorithm>

#if !defined(WINAPI_FAMILY) || (WINAPI_FAMILY == WINAPI_FAMILY_DESKTOP_APP)
#include <ddraw.h>
#include <audiodefs.h>
#include <dsound.h>

#define DIRECTINPUT_VERSION 0x800
#include <dinput.h>
#include <dinputd.h>
#endif

// D3D9 result codes are still reported by DXGetErrorString/Description; kept as local values, no d3d9.h dependency.
#ifndef _FACD3D
#define _FACD3D 0x876
#endif
#ifndef D3DERR_CANNOTPROTECTCONTENT
#define D3DERR_CANNOTPROTECTCONTENT MAKE_HRESULT(1, _FACD3D, 2173)
#endif
#ifndef D3DERR_CONFLICTINGRENDERSTATE
#define D3DERR_CONFLICTINGRENDERSTATE MAKE_HRESULT(1, _FACD3D, 2081)
#endif
#ifndef D3DERR_CONFLICTINGTEXTUREFILTER
#define D3DERR_CONFLICTINGTEXTUREFILTER MAKE_HRESULT(1, _FACD3D, 2078)
#endif
#ifndef D3DERR_CONFLICTINGTEXTUREPALETTE
#define D3DERR_CONFLICTINGTEXTUREPALETTE MAKE_HRESULT(1, _FACD3D, 2086)
#endif
#ifndef D3DERR_DEVICEHUNG
#define D3DERR_DEVICEHUNG MAKE_HRESULT(1, _FACD3D, 2164)
#endif
#ifndef D3DERR_DEVICELOST
#define D3DERR_DEVICELOST MAKE_HRESULT(1, _FACD3D, 2152)
#endif
#ifndef D3DERR_DEVICENOTRESET
#define D3DERR_DEVICENOTRESET MAKE_HRESULT(1, _FACD3D, 2153)
#endif
#ifndef D3DERR_DEVICEREMOVED
#define D3DERR_DEVICEREMOVED MAKE_HRESULT(1, _FACD3D, 2160)
#endif
#ifndef D3DERR_DRIVERINTERNALERROR
#define D3DERR_DRIVERINTERNALERROR MAKE_HRESULT(1, _FACD3D, 2087)
#endif
#ifndef D3DERR_DRIVERINVALIDCALL
#define D3DERR_DRIVERINVALIDCALL MAKE_HRESULT(1, _FACD3D, 2157)
#endif
#ifndef D3DERR_INVALIDCALL
#define D3DERR_INVALIDCALL MAKE_HRESULT(1, _FACD3D, 2156)
#endif
#ifndef D3DERR_INVALIDDEVICE
#define D3DERR_INVALIDDEVICE MAKE_HRESULT(1, _FACD3D, 2155)
#endif
#ifndef D3DERR_MOREDATA
#define D3DERR_MOREDATA MAKE_HRESULT(1, _FACD3D, 2151)
#endif
#ifndef D3DERR_NOTAVAILABLE
#define D3DERR_NOTAVAILABLE MAKE_HRESULT(1, _FACD3D, 2154)
#endif
#ifndef D3DERR_NOTFOUND
#define D3DERR_NOTFOUND MAKE_HRESULT(1, _FACD3D, 2150)
#endif
#ifndef D3DERR_OUTOFVIDEOMEMORY
#define D3DERR_OUTOFVIDEOMEMORY MAKE_HRESULT(1, _FACD3D, 380)
#endif
#ifndef D3DERR_PRESENT_STATISTICS_DISJOINT
#define D3DERR_PRESENT_STATISTICS_DISJOINT MAKE_HRESULT(1, _FACD3D, 2180)
#endif
#ifndef D3DERR_TOOMANYOPERATIONS
#define D3DERR_TOOMANYOPERATIONS MAKE_HRESULT(1, _FACD3D, 2077)
#endif
#ifndef D3DERR_UNSUPPORTEDALPHAARG
#define D3DERR_UNSUPPORTEDALPHAARG MAKE_HRESULT(1, _FACD3D, 2076)
#endif
#ifndef D3DERR_UNSUPPORTEDALPHAOPERATION
#define D3DERR_UNSUPPORTEDALPHAOPERATION MAKE_HRESULT(1, _FACD3D, 2075)
#endif
#ifndef D3DERR_UNSUPPORTEDCOLORARG
#define D3DERR_UNSUPPORTEDCOLORARG MAKE_HRESULT(1, _FACD3D, 2074)
#endif
#ifndef D3DERR_UNSUPPORTEDCOLOROPERATION
#define D3DERR_UNSUPPORTEDCOLOROPERATION MAKE_HRESULT(1, _FACD3D, 2073)
#endif
#ifndef D3DERR_UNSUPPORTEDCRYPTO
#define D3DERR_UNSUPPORTEDCRYPTO MAKE_HRESULT(1, _FACD3D, 2174)
#endif
#ifndef D3DERR_UNSUPPORTEDFACTORVALUE
#define D3DERR_UNSUPPORTEDFACTORVALUE MAKE_HRESULT(1, _FACD3D, 2079)
#endif
#ifndef D3DERR_UNSUPPORTEDOVERLAY
#define D3DERR_UNSUPPORTEDOVERLAY MAKE_HRESULT(1, _FACD3D, 2171)
#endif
#ifndef D3DERR_UNSUPPORTEDOVERLAYFORMAT
#define D3DERR_UNSUPPORTEDOVERLAYFORMAT MAKE_HRESULT(1, _FACD3D, 2172)
#endif
#ifndef D3DERR_UNSUPPORTEDTEXTUREFILTER
#define D3DERR_UNSUPPORTEDTEXTUREFILTER MAKE_HRESULT(1, _FACD3D, 2082)
#endif
#ifndef D3DERR_WASSTILLDRAWING
#define D3DERR_WASSTILLDRAWING MAKE_HRESULT(1, _FACD3D, 540)
#endif
#ifndef D3DERR_WRONGTEXTUREFORMAT
#define D3DERR_WRONGTEXTUREFORMAT MAKE_HRESULT(1, _FACD3D, 2072)
#endif
#ifndef D3DOK_NOAUTOGEN
#define D3DOK_NOAUTOGEN MAKE_HRESULT(0, _FACD3D, 2159)
#endif
#ifndef S_NOT_RESIDENT
#define S_NOT_RESIDENT MAKE_HRESULT(0, _FACD3D, 2165)
#endif
#ifndef S_RESIDENT_IN_SHARED_MEMORY
#define S_RESIDENT_IN_SHARED_MEMORY MAKE_HRESULT(0, _FACD3D, 2166)
#endif
#ifndef S_PRESENT_MODE_CHANGED
#define S_PRESENT_MODE_CHANGED MAKE_HRESULT(0, _FACD3D, 2167)
#endif
#ifndef S_PRESENT_OCCLUDED
#define S_PRESENT_OCCLUDED MAKE_HRESULT(0, _FACD3D, 2168)
#endif

#include <d3d10_1.h>
#include <d3d11_1.h>

#if !defined(WINAPI_FAMILY) || WINAPI_FAMILY != WINAPI_FAMILY_PHONE_APP
#include <wincodec.h>
#include <d2derr.h>
#include <dwrite.h>
#endif

#define XAUDIO2_E_INVALID_CALL          0x88960001
#define XAUDIO2_E_XMA_DECODER_ERROR     0x88960002
#define XAUDIO2_E_XAPO_CREATION_FAILED  0x88960003
#define XAUDIO2_E_DEVICE_INVALIDATED    0x88960004

#define XAPO_E_FORMAT_UNSUPPORTED MAKE_HRESULT(SEVERITY_ERROR, 0x897, 0x01)

#define DXUTERR_NODIRECT3D              MAKE_HRESULT(SEVERITY_ERROR, FACILITY_ITF, 0x0901)
#define DXUTERR_NOCOMPATIBLEDEVICES     MAKE_HRESULT(SEVERITY_ERROR, FACILITY_ITF, 0x0902)
#define DXUTERR_MEDIANOTFOUND           MAKE_HRESULT(SEVERITY_ERROR, FACILITY_ITF, 0x0903)
#define DXUTERR_NONZEROREFCOUNT         MAKE_HRESULT(SEVERITY_ERROR, FACILITY_ITF, 0x0904)
#define DXUTERR_CREATINGDEVICE          MAKE_HRESULT(SEVERITY_ERROR, FACILITY_ITF, 0x0905)
#define DXUTERR_RESETTINGDEVICE         MAKE_HRESULT(SEVERITY_ERROR, FACILITY_ITF, 0x0906)
#define DXUTERR_CREATINGDEVICEOBJECTS   MAKE_HRESULT(SEVERITY_ERROR, FACILITY_ITF, 0x0907)
#define DXUTERR_RESETTINGDEVICEOBJECTS  MAKE_HRESULT(SEVERITY_ERROR, FACILITY_ITF, 0x0908)
#define DXUTERR_INCORRECTVERSION        MAKE_HRESULT(SEVERITY_ERROR, FACILITY_ITF, 0x0909)
#define DXUTERR_DEVICEREMOVED           MAKE_HRESULT(SEVERITY_ERROR, FACILITY_ITF, 0x090A)

//-----------------------------------------------------------------------------
#define BUFFER_SIZE 3000

#pragma warning( disable : 6001 6221 )

//--------------------------------------------------------------------------------------
#define  CHK_ERR_W(hrchk, strOut) \
        case hrchk: \
             return L##strOut;

#define  CHK_ERRA_W(hrchk) \
        case hrchk: \
             return L#hrchk;

#define  CHK_ERR_A(hrchk, strOut) \
        case hrchk: \
             return strOut;

#define  CHK_ERRA_A(hrchk) \
        case hrchk: \
             return #hrchk;

#define HRESULT_FROM_WIN32b(x) ((HRESULT)(x) <= 0 ? ((HRESULT)(x)) : ((HRESULT) (((x) & 0x0000FFFF) | (FACILITY_WIN32 << 16) | 0x80000000)))

#define  CHK_ERR_WIN32A_W(hrchk) \
        case HRESULT_FROM_WIN32b(hrchk): \
        case hrchk: \
             return L#hrchk;

#define  CHK_ERR_WIN32_ONLY_W(hrchk, strOut) \
        case HRESULT_FROM_WIN32b(hrchk): \
             return L##strOut;

#define  CHK_ERR_WIN32A_A(hrchk) \
        case HRESULT_FROM_WIN32b(hrchk): \
        case hrchk: \
             return #hrchk;

#define  CHK_ERR_WIN32_ONLY_A(hrchk, strOut) \
        case HRESULT_FROM_WIN32b(hrchk): \
             return strOut;

//-----------------------------------------------------
const WCHAR* WINAPI DXGetErrorStringW( _In_ HRESULT hr )
{
#define CHK_ERRA CHK_ERRA_W
#define CHK_ERR CHK_ERR_W
#define CHK_ERR_WIN32A CHK_ERR_WIN32A_W
#define CHK_ERR_WIN32_ONLY CHK_ERR_WIN32_ONLY_W
#define DX_STR_WRAP(...) L##__VA_ARGS__
#include "DXGetErrorString.inl"
#undef DX_STR_WRAP
#undef CHK_ERR_WIN32A
#undef CHK_ERR_WIN32_ONLY
#undef CHK_ERRA
#undef CHK_ERR
}

const CHAR* WINAPI DXGetErrorStringA( _In_ HRESULT hr )
{
#define CHK_ERRA CHK_ERRA_A
#define CHK_ERR CHK_ERR_A
#define CHK_ERR_WIN32A CHK_ERR_WIN32A_A
#define CHK_ERR_WIN32_ONLY CHK_ERR_WIN32_ONLY_A
#define DX_STR_WRAP(s) s
#include "DXGetErrorString.inl"
#undef DX_STR_WRAP
#undef CHK_ERR_WIN32A
#undef CHK_ERR_WIN32_ONLY
#undef CHK_ERRA
#undef CHK_ERR
}

//--------------------------------------------------------------------------------------
#undef CHK_ERR
#undef CHK_ERRA
#undef HRESULT_FROM_WIN32b
#undef CHK_ERR_WIN32A
#undef CHK_ERR_WIN32_ONLY

#define  CHK_ERRA_W(hrchk) \
        case hrchk: \
             wcscpy_s( desc, count, L#hrchk );

#define  CHK_ERR_W(hrchk, strOut) \
        case hrchk: \
             wcscpy_s( desc, count, L##strOut );

#define  CHK_ERRA_A(hrchk) \
        case hrchk: \
             strcpy_s( desc, count, #hrchk );

#define  CHK_ERR_A(hrchk, strOut) \
        case hrchk: \
             strcpy_s( desc, count, strOut );

//--------------------------------------------------------------------------------------
void WINAPI DXGetErrorDescriptionW( _In_ HRESULT hr, _Out_cap_(count) WCHAR* desc, _In_ size_t count )
{
#define CHK_ERRA CHK_ERRA_W
#define CHK_ERR CHK_ERR_W
#define DX_FORMATMESSAGE FormatMessageW
#include "DXGetErrorDescription.inl"
#undef DX_FORMATMESSAGE
#undef CHK_ERRA
#undef CHK_ERR
}

void WINAPI DXGetErrorDescriptionA( _In_ HRESULT hr, _Out_cap_(count) CHAR* desc, _In_ size_t count )
{
#define CHK_ERRA CHK_ERRA_A
#define CHK_ERR CHK_ERR_A
#define DX_FORMATMESSAGE FormatMessageA
#include "DXGetErrorDescription.inl"
#undef DX_FORMATMESSAGE
#undef CHK_ERRA
#undef CHK_ERR
}

//-----------------------------------------------------------------------------
HRESULT WINAPI DXTraceW( _In_z_ const WCHAR* strFile, _In_ DWORD dwLine, _In_ HRESULT hr,
                         _In_opt_ const WCHAR* strMsg, _In_ bool bPopMsgBox )
{
#define DX_STR_WRAP(...) L##__VA_ARGS__
#define DX_CHAR WCHAR
#define DX_SPRINTF_S swprintf_s
#define DX_STRCPY_S wcscpy_s
#define DX_STRNLEN_S wcsnlen_s
#define STR_FMT_SPEC "%ls"
#define DX_MESSAGEBOX MessageBoxW
#define DX_OUTPUTDEBUGSTRING OutputDebugStringW
#define DX_GETERRORSTRING DXGetErrorStringW
#include "DXTrace.inl"
#undef DX_STR_WRAP
#undef DX_CHAR
#undef DX_SPRINTF_S
#undef DX_STRCPY_S
#undef DX_STRNLEN_S
#undef STR_FMT_SPEC
#undef DX_MESSAGEBOX
#undef DX_OUTPUTDEBUGSTRING
#undef DX_GETERRORSTRING
}

HRESULT WINAPI DXTraceA( _In_z_ const CHAR* strFile, _In_ DWORD dwLine, _In_ HRESULT hr,
                         _In_opt_ const CHAR* strMsg, _In_ bool bPopMsgBox )
{
#define DX_STR_WRAP(s) s
#define DX_CHAR CHAR
#define DX_SPRINTF_S sprintf_s
#define DX_STRCPY_S strcpy_s
#define DX_STRNLEN_S strnlen_s
#define STR_FMT_SPEC "%s"
#define DX_MESSAGEBOX MessageBoxA
#define DX_OUTPUTDEBUGSTRING OutputDebugStringA
#define DX_GETERRORSTRING DXGetErrorStringA
#include "DXTrace.inl"
#undef DX_STR_WRAP
#undef DX_CHAR
#undef DX_SPRINTF_S
#undef DX_STRCPY_S
#undef DX_STRNLEN_S
#undef STR_FMT_SPEC
#undef DX_MESSAGEBOX
#undef DX_OUTPUTDEBUGSTRING
#undef DX_GETERRORSTRING
}
