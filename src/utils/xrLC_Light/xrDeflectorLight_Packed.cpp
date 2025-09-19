#include "stdafx.h"
#include "xrDeflectorLight_Packed.h"

#include <../xrForms/CompilersUI.h>
#include "../xrLC_Light/CUDA/CUDARayCast.h"
#include "light_point.h"
#include "xrLC_GlobalData.h"
#include "xrFace.h"

void PackedLighting::ProcessReadyRays()
{
	// Fast Exit
	//if (getAllocatedRays() >= MAX_RAYS_PER_TASK)
	// if (IndexTask >= MAX_RAYS_PER_TASK)
	{
		clMsg("*** Allocated Used : %u", IndexTask.load(std::memory_order_relaxed));
		LightPointPackedRun();
		// LightPointPackedApply();

		tStats.Start();
		for (auto it = 0; it < IndexTask; it++) // Последний таск ID (Тоесть size)
		{
			auto& INFO = task_pools[it];
			Colors[INFO.INDEX_TASK].add(INFO.C);
		}
		StatsTotalGPUCopy += tStats.GetElapsed_mcs();
		ClearPool();
	}
}

/*
void PackedLighting::LightPointPacked(u32 task_id, u32 SampleID, Fvector& P, Fvector& N, base_lighting& lights, u32 flags, Face* skip)
{
	ProcessReadyRays();

 	R_ASSERT(IndexTask < MAX_TASK_POOL);
   	RayRecvestIndex& task_data = task_pools[IndexTask.load(std::memory_order_relaxed)];		// MT SAFE
	IndexTask.fetch_add(1, std::memory_order_acquire); /// Загрузили сразу добовляем
 	task_data.INDEX_TASK = task_id;
 	task_data.LightsUsed = 0;

	auto FillData = [&](LGroup& group, bool& isSunOrHemi, R_Light& L, float dot,
						Fvector& Pnew, Fvector& Ldir, float Range)
	{
 		task_data.reqRays[task_data.LightsUsed].LightGroup = group;
		task_data.reqRays[task_data.LightsUsed].isSunOrHemi = isSunOrHemi;
		task_data.reqRays[task_data.LightsUsed].LightType = L.type;
		task_data.reqRays[task_data.LightsUsed].L = &L;
		task_data.reqRays[task_data.LightsUsed].dotDirection = dot;

		task_data.reqRays[task_data.LightsUsed].P = Pnew;
		task_data.reqRays[task_data.LightsUsed].D = Ldir;
		task_data.reqRays[task_data.LightsUsed].R = 1000.f;
		task_data.reqRays[task_data.LightsUsed].result = 1;
		task_data.reqRays[task_data.LightsUsed].skip = skip;
		
		task_data.LightsUsed++;
		AllocatedRays.fetch_add(1, std::memory_order_acquire);
	};
 
 	auto CaptureVec = [&](R_Light& L, LGroup group, bool isSunOrHemi)
	{
 		R_ASSERT(task_data.LightsUsed < MAX_LIGTINGS);
		Fvector Ldir;
		Fvector Pnew = P;
		Pnew.mad(N, 0.01f);

		switch (L.type)
		{
			case LT_DIRECT:
			{
				Ldir.invert(L.direction);
				float D = Ldir.dotproduct(N);
				if (D <= 0)	return;

				FillData(group, isSunOrHemi, L, D, Pnew, Ldir, 1000.0f);
			}break;

			case LT_POINT:
			case LT_SECONDARY:
			{
				float sqD = P.distance_to_sqr(L.position);
				if (sqD > L.range2)			return;

				Ldir.sub(L.position, P).normalize_safe();
				float D = Ldir.dotproduct(N);
				if (D <= 0)					return;

				if (L.type == LT_SECONDARY)
				{
					D *= -Ldir.dotproduct(L.direction);
					if (D <= 0) return;
				}

  				FillData(group, isSunOrHemi, L, D, Pnew, Ldir, _sqrt(sqD));
  			}
		}
	};

	// RGB Lights

	tStats.Start();
	if (!(flags & LP_dont_rgb))
	{
		for (R_Light& L : lights.rgb)
		{
			CaptureVec(L, LGroup::eRGB, false);
		}
	}

	// Sun Lights
	if (!(flags & LP_dont_sun))
	{
		for (R_Light& L : lights.sun)
		{
			CaptureVec(L, LGroup::eSun, true);
		}
	}

	// Hemi Lights
	if (!(flags & LP_dont_hemi))
	{
		for (R_Light& L : lights.hemi)
		{
			CaptureVec(L, LGroup::eHemi, true);
		}
	}

	StatsRaysAdd += tStats.GetElapsed_mcs();
	// Маркировка таска
};
*/

void PackedLighting::LightPointPacked(u32 task_id, u32 SampleID, Fvector& P, Fvector& N, base_lighting& lights, u32 flags, Face* skip)
{
	// ProcessReadyRays();
	tStats.Start();
	RayRecvestIndex& task_data = task_pools[IndexTask.load(std::memory_order_relaxed)];		// MT SAFE
	IndexTask.fetch_add(1, std::memory_order_acquire); /// Загрузили сразу добовляем
	task_data.INDEX_TASK = task_id;
	task_data.flags = flags;
	task_data.P = P;
	task_data.N = N;	 
	StatsRaysAdd += tStats.GetElapsed_mcs();
}


void PackedLighting::LightPointPackedRun()
{
 	// if (AllocatedRays > 0)
	{
		// Сшиваем 
		tStats.Start();
 		XRay::RayTrace::CUDA::RayTracePackNew(*this, lc_global_data()->L_static());
		StatsCopyToVec += tStats.GetElapsed_mcs();
 	}
}

/*
void PackedLighting::LightPointPackedApply()
{
 	auto processAccum = [&](  RayRequest & Reqvest, base_color_c& C)
	{
		float att = 0.0f;
		auto& Info = Reqvest;
		switch (Info.LightType)
		{
			case LT_DIRECT:
			{
				att = Info.isSunOrHemi ?
					Info.L->energy * Reqvest.result :
					Info.dotDirection * Info.L->energy * Reqvest.result;
			} break;

			case LT_POINT:
			{
				float scale = Info.dotDirection * Info.L->energy * Reqvest.result;
				if (Info.isSunOrHemi)
					att = scale / (Info.L->attenuation0 + Info.L->attenuation1 * Reqvest.R + Info.L->attenuation2 * Info.dotDirection);
				else
				{
					att = (inlc_global_data()->gl_linear())
						? scale * (1 - Reqvest.R / Info.L->range)
						: scale * (1 / (Info.L->attenuation0 +
							Info.L->attenuation1 * Reqvest.R + Info.L->attenuation2 * Info.dotDirection) - Reqvest.R * Info.L->falloff);
				}
			} break;

			case LT_SECONDARY:
			{
				att = powf(Info.dotDirection, 0.125f) * Info.L->energy * Reqvest.result * (1 - Reqvest.R / Info.L->range);
			}break;
		}

		switch (Info.LightGroup)
		{
		case eSun:
			C.sun += att;
			break;
		case eHemi:
			C.hemi += att;
			break;
		case eRGB:
			C.rgb.x += att * Info.L->diffuse.x;
			C.rgb.y += att * Info.L->diffuse.y;
			C.rgb.z += att * Info.L->diffuse.z;
			break;
		}
	};
 
 	for (auto it = 0; it < IndexTask; it++)
	{
		auto& RAY_TASK = task_pools[it];
		for (auto INDEX = 0; INDEX < RAY_TASK.LightsUsed; INDEX++)
		{
			//auto& INFO = RAY_TASK.reqRays[INDEX];
			auto& REQ  = RAY_TASK.reqRays[INDEX];

			// if (INFO.LightGroup == eHemi)
			// 	processAccum(INFO, REQ, RAY_TASK.C);
			// if (INFO.LightGroup == eRGB)
			// 	processAccum(INFO, REQ, RAY_TASK.C);
			// if (INFO.LightGroup == eSun)
			// 	processAccum(INFO, REQ, RAY_TASK.C);

			if (REQ.LightGroup == eHemi)
				processAccum(REQ, RAY_TASK.C);
			if (REQ.LightGroup == eRGB)
				processAccum(REQ, RAY_TASK.C);
			if (REQ.LightGroup == eSun)
				processAccum(REQ, RAY_TASK.C);
		}
  	}
}

*/