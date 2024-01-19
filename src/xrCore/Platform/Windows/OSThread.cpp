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

ThreadID Platform::GetCurrentThread()
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
	LPCSTR	szName;
	u32	dwThreadID;
	u32	dwFlags;
};
#pragma pack(pop)

void Platform::SetThreadName(const char* name)
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
