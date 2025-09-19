#include "stdafx.h"
#include "xrDeflectorLight_Packed.h"

#include <../xrForms/CompilersUI.h>
#include "../xrLC_Light/CUDA/CUDARayCast.h"
#include "light_point.h"
#include "xrLC_GlobalData.h"
#include "xrFace.h"
 
void PackedLighting::LightPointPacked(u32 task_id, u32 SampleID, Fvector& P, Fvector& N, base_lighting& lights, u32 flags, Face* skip)
{
	RayRecvestIndex task_data;
	auto CaptureVec = [&](R_Light& L, LGroup group, bool isSunOrHemi)
		{
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

				RayInfo info;
				info.LGroup = group;
				info.LightType = L.type;
				info.L = &L;
				info.isSunOrHemi = isSunOrHemi;
				info.dotDirection = D;
				task_data.reqInfo.push_back(std::move(info));

				RayRequest data;
				data.P = Pnew;
				data.D = Ldir;
				data.R = 1000.f;
				data.skip = skip;
				data.result = 1;
				task_data.reqRays.push_back(std::move(data));
				AllocatedRays++;

				break;
			}

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

				RayInfo info;
				info.LGroup = group;
				info.LightType = L.type;
				info.L = &L;
				info.isSunOrHemi = isSunOrHemi;
				info.dotDirection = D;
				task_data.reqInfo.push_back(std::move(info));

				RayRequest data;
				data.P = Pnew;
				data.D = Ldir;
				data.R = _sqrt(sqD);
				data.skip = skip;
				data.result = 1;
				task_data.reqRays.push_back(std::move(data));

				AllocatedRays++;
			}
			}
		};

	// RGB Lights
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

	task_data.INDEX_TASK = task_id;	// Маркировка таска
	task_pools.push_back(std::move(task_data));
};

void PackedLighting::LightPointPackedRun()
{
 	if (task_pools.size() > 0)
	{
		// Сшиваем 
		CTimer t; t.Start();
 		xr_vector<RayRequest> rays;

		size_t total_rays = 0;
		for (const auto& task : task_pools)
			total_rays += task.reqRays.size();
		rays.reserve(total_rays); // Один раз выделяем память
		 
		// clMsg("*** Lighting Process Allocate: %u ms", t.GetElapsed_ms()); t.Start();
		int INDEX = 0; 
		for (auto& RAY_TASK : task_pools)
		{
			RAY_TASK.begin = rays.size();
 			rays.insert(
				rays.end(),
				std::make_move_iterator(RAY_TASK.reqRays.begin()),
				std::make_move_iterator(RAY_TASK.reqRays.end())
			);

			RAY_TASK.reqRays.clear(); // или shrink_to_fit() если надо
 			RAY_TASK.end = rays.size();
			INDEX++;
		}

		// clMsg("*** Lighting Process Copy: %u ms", t.GetElapsed_ms()); t.Start();

 		// Ray Tracing
		XRay::RayTrace::CUDA::RayTracePack(rays);
 	
		// clMsg("*** Lighting Process GPU: %u ms", t.GetElapsed_ms()); t.Start();

		// Обратно 
		INDEX = 0;
		for (auto& RAY_TASK : task_pools)
		{
			int eStart = RAY_TASK.begin;
			int eEnd   = RAY_TASK.end;

			RAY_TASK.reqRays.insert(
				RAY_TASK.reqRays.end(),
				std::make_move_iterator(rays.begin() + eStart),
				std::make_move_iterator(rays.begin() + eEnd)
			);

			INDEX++;
		}
  		rays.clear();

		// clMsg("*** Lighting Process COPY to CPU: %u ms", t.GetElapsed_ms()); t.Start();
	}
}

void PackedLighting::LightPointPackedApply()
{
 	auto processAccum = [&](RayInfo & Info, RayRequest & Reqvest, base_color_c& C)
	{
		float att = 0.0f;
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

		switch (Info.LGroup)
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
 
	for (auto& T : task_pools)
	{
		for (auto INDEX = 0; INDEX < T.reqRays.size(); INDEX++)
		{
			auto& INFO = T.reqInfo[INDEX];
			auto& REQ = T.reqRays[INDEX];

			if (INFO.LGroup == eHemi)
				processAccum(INFO, REQ, T.C);
			if (INFO.LGroup == eRGB)
				processAccum(INFO, REQ, T.C);
			if (INFO.LGroup == eSun)
				processAccum(INFO, REQ, T.C);
		}

	///	task_pools_samples[T.SampleID].push_back(T);
	}
}

