#pragma once

typedef void	__stdcall	pso_MemFill			(void* dest,  int value, u32 count);
typedef void	__stdcall	pso_MemFill32		(void* dest,  u32 value, u32 count);
typedef void	__stdcall	pso_MemCopy			(void* dest,  const void* src, u32 count);

#undef	ZeroMemory
#undef	CopyMemory
#undef	FillMemory
#define ZeroMemory(a,b)		Memory.mem_fill(a,0,b)
#define CopyMemory(a,b,c)	memcpy(a,b,c)			//. CopyMemory(a,b,c)
#define FillMemory(a,b,c)	Memory.mem_fill(a,c,b)
