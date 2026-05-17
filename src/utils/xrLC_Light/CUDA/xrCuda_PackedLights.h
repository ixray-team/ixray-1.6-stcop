#pragma once
#include "xrFace.h"
#include "base_color.h"
#include "xrRaysDefines.h"

// Initialize TASKS
#define MAX_RAYS_PER_TASK   16*1024				// Общее кол-во Задач (на запуск GPU)
#define MAX_RAYS_PER_GPU	16*1024				// Кол-во задач которое может обработать GPU за 1 заход Слишком большое кол-во вызывает недогруз ГПУ

 
class CUDA_PackedLighting
{
public:
	// Unordered for maps
	size_t MakeKey(u32 U, u32 V)
	{
		return (static_cast<u64>(U) << 32) | static_cast<u64>(V);
	}

	inline u32 GetU(u64 key)
	{
		return static_cast<u32>(key >> 32);
	}

	inline u32 GetV(u64 key)
	{
		return static_cast<u32>(key & 0xFFFFFFFFull);
	}

	void InitializeGPU();		   // Optix-Cuda, PTX (Once Load)
	void InitializeGPU_Model  ();  // Loading Model
	void DestroyGPU_Model();
	
	/* Специальные релизация под разные типы освещения */
	ColorsReturnType ColorsMapType = eCommon;
	void LightPointPacked_add_task(size_t IndexTask, void* Refference, Fvector& P, Fvector& N, Face* skip);
 	void LightPointPacked_run_tasks();
  	
	// Lightpoint Base
 	xrCriticalSection									csEnter;
 	xr_concurrent_unordered_map <size_t, base_color_c>  task_colors;

	// Reseting
	void RestartALL();
  
	// Stats 
	bool	isInitializedGPU = false;
	u8	    current_flags = 0;
};

extern CUDA_PackedLighting GPUTaskinSystem;