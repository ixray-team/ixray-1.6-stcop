// xrCPU_Pipe.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#pragma hdrstop

#pragma comment(lib,"xrEngine.lib")

BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
    return TRUE;
}

extern xrSkin1W			xrSkin1W_x86;
extern xrSkin1W			xrSkin1W_3DNow;
// extern xrSkin1W		xrSkin1W_SSE;
extern xrSkin2W			xrSkin2W_x86;
extern xrSkin2W			xrSkin2W_SSE;
extern xrSkin2W			xrSkin2W_3DNow;

extern xrSkin3W			xrSkin3W_x86;
extern xrSkin4W			xrSkin4W_x86;
extern xrM44_Mul		xrM44_Mul_x86;
extern xrM44_Mul		xrM44_Mul_3DNow;
extern xrM44_Mul		xrM44_Mul_SSE;
extern xrTransfer		xrTransfer_x86;
extern xrMemCopy_8b		xrMemCopy_MMXSSE3DNow;
extern xrMemCopy_8b		xrMemCopy_x86;
extern xrMemFill_32b	xrMemFill32_MMX;


extern "C" {
	__declspec(dllexport) void	__cdecl	xrBind_PSGP	(xrDispatchTable* T, DWORD dwFeatures)
	{
		// generic
		T->skin1W	= xrSkin1W_x86;
		T->skin2W	= xrSkin2W_x86;
		T->skin3W	= xrSkin3W_x86;
		T->skin4W	= xrSkin4W_x86;
		T->m44_mul	= xrM44_Mul_x86;
		T->transfer = xrTransfer_x86;
		T->memCopy	= xrMemCopy_x86;
		T->memFill	= NULL;
		T->memFill32= xrMemFill32_MMX;
		
		// SSE
		if (dwFeatures & _CPU_FEATURE_SSE) {
			T->memCopy	= xrMemCopy_MMXSSE3DNow;
		}
 
		// 3dnow!
		if (dwFeatures & _CPU_FEATURE_3DNOW) {
			T->memCopy	= xrMemCopy_MMXSSE3DNow;
		}
	}
};
