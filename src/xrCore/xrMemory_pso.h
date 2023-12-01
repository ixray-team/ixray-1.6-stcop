#pragma once

typedef void	__stdcall	pso_MemFill			(void* dest,  int value, size_t count);
typedef void	__stdcall	pso_MemFill32		(void* dest,  size_t value, size_t count);
typedef void	__stdcall	pso_MemCopy			(void* dest,  const void* src, size_t count);

#undef	ZeroMemory
#undef	CopyMemory
#undef	FillMemory
#define ZeroMemory(a,b)		Memory.mem_fill(a,0,b)
#define CopyMemory(a,b,c)	memcpy(a,b,c)			//. CopyMemory(a,b,c)
#define FillMemory(a,b,c)	Memory.mem_fill(a,c,b)
