#pragma once
#include "xrFace.h"
#include "base_lighting.h"
#include "base_color.h"
#include "lm_layer.h"
#include "uv_tri.h"
#include "R_light.h"

enum LGroup
{
	eSun = 1,
	eHemi = 2,
	eRGB = 3
};

#pragma pack(push, 4)
struct RayInfo
{
	// Stuff Parrams
	LGroup   LGroup;
	bool     isSunOrHemi;
	u8		 LightType;

	R_Light* L;
	float    dotDirection;
};

struct RayRequest
{
	Fvector P;      // Начальная точка луча (аналог вашего `P`)
	Fvector D;      // Направление луча (аналог `D`)
	float R;        // Максимальная дистанция (аналог `R`)
	float result;   // Результат трассировки (расстояние или -1)

	Face* skip;     // Полигон для игнорирования (аналог `skip`)
};
#pragma pack(pop)


struct RayRecvestIndex
{
	base_color_c C;
	u32 INDEX_TASK;
	u32 SampleID;

	u32 begin;
	u32 end;

	xr_vector<RayInfo>			 reqInfo;
	xr_vector<RayRequest>		 reqRays;
};
typedef xr_vector<RayRecvestIndex> rays_tasked;



class PackedLighting
{
public:
	// Result Vector
	xr_vector<RayRecvestIndex> task_pools;
 	size_t AllocatedRays = 0;
 	constexpr PackedLighting() = default;
	~PackedLighting() 
	{
		ClearPool();
	};

public:
	void LightPointPacked(u32 task_id, u32 SampleID, Fvector& P, Fvector& N, base_lighting& lights, u32 flags, Face* skip);
	void LightPointPackedRun();
	void LightPointPackedApply();

	void ClearPool()
	{ 
		task_pools.clear();
		AllocatedRays = 0; 
	}
	size_t getAllocatedRays() { return AllocatedRays; }
};