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

// Initialize TASKS
#define MAX_RAYS_PER_TASK   1024 * 1024 * 40 // Нужно еще учесть что там будут лампочек может быть по 256 за 1 таск
 
// Recvest Class
struct RayRecvestIndex
{
	CDeflector* Owner = nullptr;
	base_color_c C;
	std::pair<u32, u32> INDEX_TASK;
  	
	// Task Pos, Dir, Skip
	Fvector P;
	Fvector N;
	Face* skip;
};

class PackedLighting
{
public:
	// Result Vector
	bool isInitializedGPU = false;
	u8	    current_flags = 0;
	size_t  TotalRaysProcessed = 0;
	u32		IndexTask = 0;


	xr_vector<RayRecvestIndex> task_pools;

	PackedLighting()
	{	
		// InitializeGPU();
		task_pools.resize(MAX_RAYS_PER_TASK);
 		ClearPool();
	};
 
	~PackedLighting() 
	{
		ClearPool();
	};

public:
	RayRecvestIndex& GetRays(int Index) { return task_pools[Index]; }
 
	void InitializeGPU();
	void LightPointPacked(u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip);
	void LightPointPackedDeflector(u32 U, u32 V, CDeflector* D, Fvector& P, Fvector& N, u32 flags, Face* skip);
 	void LightPointPackedRun();
   	
	void ClearPool()
	{ 
		TotalRaysProcessed += IndexTask;
		IndexTask = 0;
	}

	void RestartALL()
	{
		// start
		current_flags = 0;
 
		TotalRaysProcessed = 0;
		IndexTask = 0;
		Colors.clear();
 
		// Stats
		StatsTotalGPUCopy = 0;
		StatsCopyToVec = 0;
		StatsRaysAdd = 0;
	}

	xr_map<std::pair<u32, u32>, u32>		  FCountMap;
	xr_map<std::pair<u32, u32>, base_color_c> Colors;	// Task Index, Color.
  
	// Stats 
	CTimer tStats;
	u64 StatsTotalGPUCopy = 0;
	u64 StatsCopyToVec = 0;
	u64 StatsRaysAdd = 0;
};

extern PackedLighting GPUTaskinSystem;
