#include "stdafx.h"

#pragma intrinsic(_InterlockedCompareExchange)

unsigned int Platform::GetCpuCores(CpuCore* Cores, unsigned int MaxCount)
{
	DWORD Size = 0;
	GetLogicalProcessorInformationEx(RelationProcessorCore, nullptr, &Size);
	if (Size == 0)
	{
		return 0;
	}

	xr_vector<u8> Buffer(Size);
	if (!GetLogicalProcessorInformationEx(RelationProcessorCore, (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX)Buffer.data(), &Size))
	{
		return 0;
	}

	unsigned int Count = 0;
	for (DWORD Offset = 0; Offset < Size && Count < MaxCount;)
	{
		const auto* Info = (const SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*)(Buffer.data() + Offset);
		Offset += Info->Size;
		if (Info->Relationship != RelationProcessorCore || Info->Processor.GroupCount == 0 || Info->Processor.GroupMask[0].Group != 0)
		{
			continue;
		}

		Cores[Count].LogicalMask = Info->Processor.GroupMask[0].Mask;
		Cores[Count].EfficiencyClass = Info->Processor.EfficiencyClass;
		Count++;
	}
	return Count;
}

void Platform::SetCurrentThreadAffinity(unsigned long long LogicalMask)
{
	if (LogicalMask != 0)
	{
		SetThreadAffinityMask(::GetCurrentThread(), DWORD_PTR(LogicalMask));
	}
}

u32 Platform::GetCoresCount()
{
	bool allocatedBuffer = false;
	SYSTEM_LOGICAL_PROCESSOR_INFORMATION SLPI = {};
	SYSTEM_LOGICAL_PROCESSOR_INFORMATION* ptr = &SLPI;
	DWORD addr = sizeof(SLPI);
	u32 sizeofStruct = sizeof(SLPI);
	bool result = GetLogicalProcessorInformation(&SLPI, &addr);
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

XRCORE_API ThreadID Platform::GetCurrentThread()
{
	return ::GetCurrentThread();
}

size_t Platform::GetThreadId(ThreadID ID)
{
    return ::GetThreadId(ID);
}

#pragma pack(push, 8)
struct THREAD_NAME
{
	u32	dwType;
	const char*	szName;
	u32	dwThreadID;
	u32	dwFlags;
};
#pragma pack(pop)

XRCORE_API void Platform::SetThreadName(const char* name)
{
    using SetThreadDescriptionDesc = HRESULT(WINAPI*)(HANDLE, PCWSTR);
    auto Kernellib = GetModuleHandle(L"kernel32.dll");
    static SetThreadDescriptionDesc SetThreadDescriptionProc = (SetThreadDescriptionDesc)GetProcAddress(Kernellib, "SetThreadDescription");

    if (SetThreadDescriptionProc != nullptr)
    {
        SetThreadDescriptionProc(GetCurrentThread(), Platform::ANSI_TO_TCHAR(name));
    }
    else
    {
        THREAD_NAME tn;
        tn.dwType = 0x1000;
        tn.szName = name;
        tn.dwThreadID = DWORD(-1);
        tn.dwFlags = 0;

        __try
        {
            RaiseException(0x406D1388, 0, sizeof(tn) / sizeof(DWORD), (ULONG_PTR*)&tn);
        }
        __except (EXCEPTION_CONTINUE_EXECUTION)
        {
        }
    }
}

long Platform::AtomicCompareExchange(long* ptr, long expected, long desired)
{
	return _InterlockedCompareExchange(ptr, desired, expected);
}

unsigned int Platform::AtomicCompareExchangeU32(unsigned int* ptr, unsigned int expected, unsigned int desired)
{
	return (u32)_InterlockedCompareExchange((long*)ptr, (long)desired, (long)expected);
}