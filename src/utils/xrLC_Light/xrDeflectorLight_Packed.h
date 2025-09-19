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
#define MAX_RAYS_PER_TASK   1024 * 1024					// Общее кол-во Задач (на запуск GPU)
#define MAX_RAYS_PER_GPU	128  * 1024					// Кол-во задач которое может обработать GPU за 1 заход Слишком большое кол-во вызывает недогруз ГПУ

struct RayRecvestIndex
{
	void* Owner = 0;
   	size_t INDEX_TASK;

	// Task Pos, Dir, Skip
	Fvector P;
	Fvector N;
	// Face* skip;
};
 
class PackedLighting
{
	// new hash
	typedef xr_hash_map<size_t, base_color_c>		color_map;

public:
	// Result Vector
	PackedLighting()
	{	
  	};
 
	~PackedLighting() 
	{
 	};

public:
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
	
	// Implicit (or AdaptiveHT) (No Has Deflector)
	void LightPointPacked(u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip);
	void LightPointPackedRun();

	xrCriticalSection csAdd;

	// Deflectors Processing
	xr_atomic_u32 DeflectorsReady = 0;
	xr_atomic_u32 DeflectorsRecvested = 0;

	void LightPointPackedDeflector(CDeflector* D, u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip);
 	void LightPointPackedDeflectorsRun();
 
	// xrMuModels
 	void LightPointPacked_MODEL(xrMU_Reference* MU, u32 I, Fvector& P, Fvector& N, u32 flags, Face* skip);
	void LightPointPacked_MODELRun();
 
 	void RestartALL()
	{
		// start
		DeflectorsReady = 0;
		DeflectorsRecvested = 0;

		current_flags = 0;

		// Basic Tasks
  		Colors.clear();

		// task pool memory clear
		task_pools.clear();
		task_pools.shrink_to_fit();
	}
 
	// Task Index, Color. 
	// простые задчи
 	color_map			Colors;	

	// Stats 
	bool	isInitializedGPU = false;
	u8	    current_flags = 0;
 
	// tasks	
	xr_concurrent_vector<RayRecvestIndex>																task_pools;			// BASIC UV

	xrCriticalSection csEnter;
};

extern PackedLighting GPUTaskinSystem;
