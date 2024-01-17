#include "stdafx.h"

u32 Platform::GetCoresCount()
{
	bool allocatedBuffer = false;
	SYSTEM_LOGICAL_PROCESSOR_INFORMATION SLPI = {};
	SYSTEM_LOGICAL_PROCESSOR_INFORMATION* ptr = &SLPI;
	DWORD addr = sizeof(SLPI);
	u32 sizeofStruct = sizeof(SLPI);
	BOOL result = GetLogicalProcessorInformation(&SLPI, &addr);
	if (!result)
	{
		u32 errCode = GetLastError();
		if (errCode == ERROR_INSUFFICIENT_BUFFER)
		{
			ptr = (SYSTEM_LOGICAL_PROCESSOR_INFORMATION*)new BYTE[addr];
			allocatedBuffer = true;
			result = GetLogicalProcessorInformation(ptr, &addr);
		}
	}

	u32 byteOffset = 0;
	u32 processorCoreCount = 0;
	u32 processorPackageCount = 0;

	const s64 origPtr = reinterpret_cast<s64>(ptr);
	while (byteOffset + sizeofStruct <= addr)
	{
		switch (ptr->Relationship)
		{
		case RelationProcessorCore:
			processorCoreCount++;

			break;
		case RelationProcessorPackage:
			// Logical processors share a physical package.
			processorPackageCount++;
			break;

		default:
			break;
		}
		byteOffset += sizeofStruct;
		ptr++;
	}

	ptr = reinterpret_cast<SYSTEM_LOGICAL_PROCESSOR_INFORMATION*>(origPtr);

	if (allocatedBuffer)
		xr_delete(ptr);

	return processorCoreCount;
}

ThreadID Platform::GetCurrentThread()
{
	return ::GetCurrentThread();
}