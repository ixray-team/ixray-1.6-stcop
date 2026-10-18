#include "stdafx.h"

#include "CUDAContext.h"

// GPU Usage
#ifdef LCCUDA_BUILD
#include <nvml.h>
#pragma comment(lib, "nvml.lib")
 
static bool isStatisticInitilized = false;
static nvmlDevice_t device = nullptr;

void CudaUsage(unsigned int& UsageCuda, unsigned int& UsageMemory)
{
	nvmlReturn_t result;
 	if (device != nullptr)
	{
		nvmlUtilization_t util{};
		result = nvmlDeviceGetUtilizationRates(device, &util);
		if (result == NVML_SUCCESS)
		{
			UsageCuda = util.gpu;
			UsageMemory = util.memory;
		}
	}
}

static xr_vector<float> gpuUsage;
static xr_vector<float> memUsage;

xr_vector<float> get_cuda_usage()
{
	return gpuUsage;
}

xr_vector<float> get_mem_usage()
{
	return memUsage;
}

void CudaStatisticThread()
{
	nvmlReturn_t result = nvmlInit();
	if (!isStatisticInitilized)
	{
		if (result != NVML_SUCCESS)
		{
			Msg("NVML Init failed: ", nvmlErrorString(result));
			return;
		}

		unsigned int deviceCount = 0;
		nvmlDeviceGetCount(&deviceCount);

		if (deviceCount == 0)
		{
			Msg("No NVIDIA GPU found.");
			nvmlShutdown();
			return;
		}

		nvmlDeviceGetHandleByIndex(0, &device);

		char name[128];
		nvmlDeviceGetName(device, name, sizeof(name));
		Msg("Cuda Device: %s", name);
		isStatisticInitilized = true;
	}

	std::thread([]
		{
			while (true)
			{
				u32 uCuda = 0;
				u32 uMemory = 0;
				CudaUsage(uCuda, uMemory);

				{
					gpuUsage.push_back(uCuda);

					if (gpuUsage.size() > 80)
					{
						gpuUsage.erase(gpuUsage.begin());
					}

					memUsage.push_back(uMemory);
					if (memUsage.size() > 80)
					{
						memUsage.erase(memUsage.begin());
					}
				}

				Sleep(33);
			};

		}).detach();
}

void CudaStatsShutdown()
{
	if (isStatisticInitilized)
		nvmlShutdown();
}

#endif 