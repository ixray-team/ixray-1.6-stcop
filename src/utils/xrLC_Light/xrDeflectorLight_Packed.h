#pragma once
#include "xrFace.h"
#include "base_lighting.h"
#include "base_color.h"
#include "lm_layer.h"
#include "uv_tri.h"
#include "R_light.h"

enum LGroup : u8
{
	eSun = 1,
	eHemi = 2,
	eRGB = 3
};

#pragma pack(push, 4)
// struct RayRequest
// {
// 	// Stuff Parrams
// 	R_Light* L;
// 	float    dotDirection;
// 	// Ray Request
// 	Fvector P;      // Начальная точка луча (аналог вашего `P`)
// 	Fvector D;      // Направление луча (аналог `D`)
// 	float R;        // Максимальная дистанция (аналог `R`)
// 	float result;   // Результат трассировки (расстояние или -1)
// 
// 	LGroup   LightGroup;
// 	bool	 isSunOrHemi;
// 	u16		 LightType;
// 	Face* skip;     // Полигон для игнорирования (аналог `skip`)
// };
// 
struct RayRequest
{	
 	// Ray Request
	Fvector P;			// Начальная точка луча (аналог вашего `P`)
	Fvector D;			// Направление луча (аналог `D`)
	float R;			// Максимальная дистанция (аналог `R`)
	float result;		// Результат трассировки (расстояние или -1)
	Face*	 skip;      // Полигон для игнорирования (аналог `skip`)
};
#pragma pack(pop)


// Initialize TASKS
#define MAX_RAYS_PER_TASK   1024 * 1024 * 40 // Нужно еще учесть что там будут лампочек может быть по 256 за 1 таск
 
// Recvest Class
struct RayRecvestIndex
{
	base_color_c C;
	std::pair<u32, u32> INDEX_TASK;
	u32 SampleID;

	Fvector P;
	Fvector N;
	Face* skip;
	u32 flags;
};

class PackedLighting
{
public:
	// Result Vector
	size_t TotalRaysProcessed = 0;
	xr_atomic_u32 IndexTask = 0;
	RayRecvestIndex* task_pools;

	PackedLighting()
	{
		task_pools = ( RayRecvestIndex * ) xr_malloc(MAX_RAYS_PER_TASK * sizeof(RayRecvestIndex) );
		ClearPool();
	};
 
	~PackedLighting() 
	{
		ClearPool();
	};

public:
	RayRecvestIndex& GetRays(int Index) { return task_pools[Index]; }
 
	void LightPointPacked(u32 U, u32 V, u32 SampleID, Fvector& P, Fvector& N, base_lighting& lights, u32 flags, Face* skip);
	void LightPointPackedRun();
   	
	void ClearPool()
	{ 
		TotalRaysProcessed += AllocatedRays;
 		AllocatedRays = 0; 
		IndexTask.store(0, std::memory_order_acquire);
	}

	xr_map<std::pair<u32, u32>, base_color_c> Colors;	// Task Index, Color.
 	xr_atomic_u32 AllocatedRays = 0;
 
	// Stats 
	CTimer tStats;
	u64 StatsTotalGPUCopy = 0;
	u64 StatsCopyToVec = 0;
	u64 StatsRaysAdd = 0;
};