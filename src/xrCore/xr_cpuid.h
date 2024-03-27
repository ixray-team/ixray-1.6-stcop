#pragma once

using ulong_t = unsigned long long;
using long_t = long long;

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

	u8 family;		// family of the processor, eg. Intel_Pentium_Pro is family 6 processor
	u8 model;		// model of processor, eg. Intel_Pentium_Pro is model 1 of family 6 processor
	u8 stepping;	// Processor revision number

	bool isAmd = false;		// AMD flag
	bool isIntel = false;	// IntelCore flag
	char vendor[32];
	char modelName[64];

	unsigned features = 0;		// processor Feature ( same as return value).

	unsigned n_cores = 0;		// number of available physical cores
	unsigned n_threads = 0;		// number of available logical threads

	bool hasFeature(const CPUFeature feature) const noexcept
	{
		return (features & static_cast<unsigned>(feature));
	}
};