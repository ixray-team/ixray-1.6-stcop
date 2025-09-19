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
#define MAX_RAYS_PER_TASK   1024 * 1024 * 10 // Нужно еще учесть что там будут лампочек может быть по 256 за 1 таск
 
// Recvest Class
struct RayRecvestIndex
{
  	std::pair<u32, u32> INDEX_TASK;
	CDeflector* Owner;

	// Task Pos, Dir, Skip
	Fvector P;
	Fvector N;
	Face* skip;
};

class PackedLighting
{
	typedef xr_map<std::pair<u32, u32>, u32>		  FCountsMap;
	typedef xr_map<std::pair<u32, u32>, base_color_c> color_map;

public:
	// Result Vector
	PackedLighting()
	{	
  	};
 
	~PackedLighting() 
	{
 	};

public:
 
	void InitializeGPU();
	
	// Implicit (or AdaptiveHT) (No Has Deflector)
	void LightPointPacked(u32 U, u32 V, Fvector& P, Fvector& N, u32 flags, Face* skip);
	void LightPointPackedRun();

	// Deflectors Processing
	void LightPointPackedDeflector(u32 U, u32 V, CDeflector* D, Fvector& P, Fvector& N, u32 flags, Face* skip);
	void LightPointPackedDeflectorsRun();
 
 	void RestartALL()
	{
		// start
		current_flags = 0;
 
	 	// Basic Tasks
		task_pools.clear();
  		Colors.clear();
		FCountMap.clear();

		// Deflectors
 		DEF_FCountMap.clear();
		DEF_Colors.clear();

		// task pool memory clear
		task_pools.shrink_to_fit();
	}

 
	// Task Index, Color. 
	// простые задчи
	FCountsMap			FCountMap;
	color_map			Colors;	

	// Task Index, Color. 
	// сложные задчи СDeflector
	xr_map<CDeflector*, FCountsMap>			DEF_FCountMap;
	xr_map<CDeflector*, color_map>			DEF_Colors;
  
	// Stats 
	bool	isInitializedGPU = false;
	u8	    current_flags = 0;

	CTimer tStats;
	size_t StatsTotalGPU = 0;
	size_t StatsTraverseGPU = 0;
 	size_t StatsRaysAdd = 0;

	xrCriticalSection csRayLaunched;

	// tasks	
	concurrency::concurrent_vector<RayRecvestIndex>							 task_pools;			// BASIC UV
 };

extern PackedLighting GPUTaskinSystem;
