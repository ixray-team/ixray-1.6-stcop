#include "stdafx.h"
#pragma hdrstop

void  xrMemFill_x86(void* dest, int value, size_t count)
{
	memset(dest, int(value), count);
}
