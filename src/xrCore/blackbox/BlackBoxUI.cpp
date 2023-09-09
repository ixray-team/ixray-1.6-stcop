#include "stdafx_.h"
#include "BugSlayerUtil.h"
#include <stdio.h>


#ifdef _EDITOR
#	pragma auto_inline(off)
	DWORD_PTR program_counter()
	{
		DWORD_PTR programcounter;

		// Get the return address out of the current stack frame
		__asm mov eax, [ebp + 4]
		// Put the return address into the variable we'll return
		__asm mov [programcounter], eax

		return programcounter;
	}
#	pragma auto_inline(on)
#else // _EDITOR
	extern "C" void * _ReturnAddress(void);

#   pragma intrinsic(_ReturnAddress)

#	pragma auto_inline(off)
	DWORD_PTR program_counter()
	{
		return (DWORD_PTR)_ReturnAddress();
	}
#	pragma auto_inline(on)
#endif // _EDITOR
#ifndef _EDITOR
__declspec(noinline)
#endif // _EDITOR
