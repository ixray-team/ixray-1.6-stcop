#pragma once
#include "xrFace.h"
#include "base_lighting.h"
#include "base_color.h"
#include "lm_layer.h"
#include "uv_tri.h"
#include "R_light.h"
#include "xrMU_Model_Reference.h"

enum LGroup : u8
{
	eSun = 1,
	eHemi = 2,
	eRGB = 3
};

// Initialize TASKS
#define MAX_RAYS_PER_TASK   48 * 1024				// Общее кол-во Задач (на запуск GPU)
#define MAX_RAYS_PER_GPU	48 * 1024				// Кол-во задач которое может обработать GPU за 1 заход Слишком большое кол-во вызывает недогруз ГПУ

// Для RTX 3060 ~+ 48 SM  Блоков по 1024 Таскера

struct RayRecvestIndex
{
	void* Owner = 0;
   	size_t  INDEX_TASK;
	

	// Task Pos, Dir, Skip
	Fvector P;
	Fvector N;
	// Face* skip;
};
  
class PackedLighting
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

	void InitializeGPU();
	
	/* Специальные релизация под разные типы освещения */


	// Implicit (or AdaptiveHT) (No Has Deflector)
	void LightPointPacked_Implicit(u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip);
	void LightPointPacked_ImplicitRun();

 	// Deflectors 
	void LightPointPackedDeflector(size_t IndexTask, CDeflector* D, Fvector& P, Fvector& N, u32 flags, Face* skip);
 	void LightPointPackedDeflectorsRun();
 
	// xrMuModels
 	void LightPointPacked_MODEL(xrMU_Reference* MU, u32 I, Fvector& P, Fvector& N, u32 flags, Face* skip);
	void LightPointPacked_MODELRun();
 
	
	/* Универсальный */
 
	// Lightpoint Base
 	xrCriticalSection									csEnter;
	xr_concurrent_vector        <RayRecvestIndex>		task_pools;
	xr_concurrent_unordered_map <size_t, base_color_c>  task_colors;

	// Basic
	void LightPointPacked(u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip);
	void LightPointPackedRun();

 
 	void RestartALL()
	{
		// start
		Recalculated = 0;
 		current_flags = 0;

		// clearing pool
		task_pools.clear();
		task_colors.clear();
	}
 
  
	// Stats 
	bool	isInitializedGPU = false;
	u8	    current_flags = 0;
 	 
	// Stats
	u32 Recalculated = 0;
};

extern PackedLighting GPUTaskinSystem;
