using ulong_t = unsigned long long;
using long_t = long long;

typedef HRESULT(WINAPI* NTQUERYSYSTEMINFORMATION)(UINT, PVOID, ULONG, PULONG);

#define SystemProcessorPerformanceInformation 8
#define MAX_CPU 8
#define MAX_HISTORY 512

#pragma once

typedef struct SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION
{
	LARGE_INTEGER	IdleTime;
	LARGE_INTEGER	KernelTime;
	LARGE_INTEGER	UserTime;
	LARGE_INTEGER	Reserved1[2];
	ULONG			Reserved2;
} SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION;

enum class CPUFeature : unsigned
{
	MMX = 1 << 0,
	MMXExt = 1 << 1,

	SSE = 1 << 2,
	SSE2 = 1 << 3,
	SSE3 = 1 << 4,
	SSSE3 = 1 << 5,
	SSE41 = 1 << 6,
	SSE4a = 1 << 7,
	SSE42 = 1 << 8,

	AVX = 1 << 9,
	AVX2 = 1 << 10,
	AVX512F = 1 << 11,
	AVX512PF = 1 << 12,
	AVX512ER = 1 << 13,
	AVX512CD = 1 << 14,

	AMD_3DNow = 1 << 15,
	AMD_3DNowExt = 1 << 16,

	MWait = 1 << 17,
	HT = 1 << 18,
	TM2 = 1 << 19,
	AES = 1 << 20,
	EST = 1 << 21,
	VMX = 1 << 22,
	AMD = 1 << 23,
	XFSR = 1 << 24
};

struct XRCORE_API processor_info
{
	processor_info();
	~processor_info();

	DWORD m_dwNumberOfProcessors = 0;
	SYSTEM_INFO sysInfo;
	NTQUERYSYSTEMINFORMATION m_pNtQuerySystemInformation;
	SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION* perfomanceInfo;
	FILETIME prevSysIdle, prevSysKernel, prevSysUser;
	DWORD m_dwTickCount[MAX_CPU];
	DWORD m_dwCount;
	LARGE_INTEGER m_idleTime[MAX_CPU];
	FLOAT m_fltCpuUsage[MAX_CPU];
	FLOAT m_fltCpuUsageHistory[MAX_CPU][512];
	UINT m_nTimerID;
	float* fUsage;

	u8 family;	// family of the processor, eg. Intel_Pentium_Pro is family 6 processor
	u8 model;	// model of processor, eg. Intel_Pentium_Pro is model 1 of family 6 processor
	u8 stepping; // Processor revision number

	bool isAmd;				// AMD flag
	bool isIntel;			// IntelCore flag
	char vendor[32];
	char modelName[64];

	unsigned features;		// processor Feature ( same as return value).

	unsigned n_cores;		// number of available physical cores
	unsigned n_threads;		// number of available logical threads

	unsigned affinity_mask; // recommended affinity mask
	// all processors available to process
	// except 2nd (and upper) logical threads
	// of the same physical core

	bool hasFeature(const CPUFeature feature) const noexcept
	{
		return (features & static_cast<unsigned>(feature));
	}

	bool GetCPULoad(double& Val);
	float* MTCPULoad();
	float CalcMPCPULoad(DWORD dwCPU);

};