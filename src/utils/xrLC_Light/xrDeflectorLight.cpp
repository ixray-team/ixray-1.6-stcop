#include "stdafx.h"
#include "../../xrCore/Collision/cl_intersect.h"

#include "xrDeflector.h"
#include "xrLC_GlobalData.h"
#include "light_point.h"
#include "xrFace.h"

#include "../xrForms/CompilersUI.h"
#include "src/utils/xrLC/Build.h"

void Jitter_Select(Fvector2* &Jitter, u32& Jcount)
{
	static Fvector2 Jitter1[1] = {
		{0,0}
	};
	static Fvector2 Jitter4[4] = {
		{-1,-1}, {1,-1}, {1,1}, {-1,1}
	};
	static Fvector2 Jitter9[9] = {
		{-1,-1},	{0,-1},		{1,-1}, 
		{-1,0},		{0,0},		{1,0},
		{-1,1},		{0,1},		{1,1}
	};

	switch (g_params().m_lm_jitter_samples)
	{
		case 1:
			Jcount	= 1;
			Jitter	= Jitter1;
			break;
		case 9:
			Jcount	= 9;
			Jitter	= Jitter9;
			break;
		case 4:
		default:
			Jcount	= 4;
			Jitter	= Jitter4;
			break;
	}
}

void LightPoint(EmbreeRayTraceModel& MDL, base_color_c& C, Fvector& P, Fvector& N, base_lighting& lights, u32 flags, Face* skip)
{
	auto processLight = [&]<typename T>(R_Light & L, T & accumulator, bool isSunOrHemi)
	{   
		Fvector Ldir;
		Fvector Pnew = P;
		Pnew.mad(N, 0.01f);
		float att = 0.0f;

		switch (L.type)
		{
		case LT_DIRECT:
		{
			Ldir.invert(L.direction);
			float D = Ldir.dotproduct(N);
			if (D <= 0)					return;

			float trace = MDL.RaytraceEmbreeProcess( Pnew, Ldir, 1000.f, skip );
			att = isSunOrHemi ? L.energy * trace : D * L.energy * trace;
			break;
		}
		case LT_POINT:
		{
			float sqD = P.distance_to_sqr(L.position);
			if (sqD > L.range2)
				return;

			Ldir.sub(L.position, P).normalize_safe();
			float D = Ldir.dotproduct(N);
			if (D <= 0)
				return;

			float R = _sqrt(sqD);
			float trace = MDL.RaytraceEmbreeProcess( Pnew, Ldir, R, skip);
			float scale = D * L.energy * trace;

			if (isSunOrHemi)
			{
				att = scale / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD);
			}
			else
			{
				att = scale * (1 / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD) - R * L.falloff);
			}
			break;
		}
		case LT_SECONDARY:
		{
			float sqD = P.distance_to_sqr(L.position);
			if (sqD > L.range2)
				return;

			Ldir.sub(L.position, P).normalize_safe();
			float D = Ldir.dotproduct(N);
			if (D <= 0)
				return;

			D *= -Ldir.dotproduct(L.direction);
			if (D <= 0)
				return;

			float R = _sqrt(sqD);
			float trace = MDL.RaytraceEmbreeProcess( Pnew, Ldir, R, skip );
			att = powf(D, 0.125f) * L.energy * trace * (1 - R / L.range);
			break;
		}
		}

		if (isSunOrHemi)
		{
			if constexpr (std::is_arithmetic_v<T>)
			{
				accumulator += att;
			}
			else
			{
				accumulator.add(att);
			}
		}
		else
		{
			C.rgb.x += att * L.diffuse.x;
			C.rgb.y += att * L.diffuse.y;
			C.rgb.z += att * L.diffuse.z;
		}
	};

	// RGB Lights
	if (!(flags & LP_dont_rgb))
	{
		for (R_Light& L : lights.rgb)
		{
			processLight(L, C.rgb, false);
		}
	}

	// Sun Lights
	if (!(flags & LP_dont_sun))
	{
		for (R_Light& L : lights.sun)
		{
			processLight(L, C.sun, true);
		}
	}

	// Hemi Lights
	if (!(flags & LP_dont_hemi))
	{
		for (R_Light& L : lights.hemi)
		{
			processLight(L, C.hemi, true);
		}
	}
}

thread_local xr_vector< RayTask > rays;
void LightPoint_Jitters(xr_vector<JiterPixel>& world_pos, base_lighting& lights, u32 flags)
{
	rays.clear();
	auto processLight = [&](JiterPixel& wPX, DeflectorLType LType, R_Light& L, bool isSunOrHemi)
	{
		Fvector Ldir;
		Fvector Pnew = wPX.wP;
		Pnew.mad(wPX.wN, 0.01f);
		float att = 0.0f;

		Fvector P = wPX.wP;
		Fvector N = wPX.wN;
		float R = 0;

		switch (L.type)
		{
			case LT_DIRECT:
			{
				Ldir.invert(L.direction);
				float D = Ldir.dotproduct(N);
				if (D <= 0) return;

				att = isSunOrHemi ? L.energy : D * L.energy;
				R = 1000.0f;
			} break;

			case LT_POINT:
			case LT_SECONDARY:
			{
				float sqD = P.distance_to_sqr(L.position);
				if (sqD > L.range2)					return;

				Ldir.sub(L.position, P).normalize_safe();
				float D = Ldir.dotproduct(N);
				if (D <= 0)							return;
 				R = _sqrt(sqD);

				if (L.type == LT_SECONDARY)
				{
 					D *= -Ldir.dotproduct(L.direction);
					if (D <= 0)						return;
 					
					att = powf(D, 0.125f) * L.energy * (1 - R / L.range);
				}
				else 
				{
 					float scale = D * L.energy;
					if (isSunOrHemi)
 						att = scale / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD);
 					else
 						att = scale * (1 / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD) - R * L.falloff);
 				}  

			} break;
		}

		rays.emplace_back(Pnew, Ldir, R, wPX.skip, att, LType, &wPX.C);
	};

	// RGB Lights
	if (!(flags & LP_dont_rgb))
	{
		for (R_Light& L : lights.rgb)
		{
			for (auto& wPX : world_pos)	// Именно такой порядок ! (Однонаправленые Лучи)
				processLight(wPX, DeflectorLType::eDefRgb, L, false);
		}
	}

	// Sun Lights
	if (!(flags & LP_dont_sun))
	{
		for (R_Light& L : lights.sun)
		{
			for (auto& wPX : world_pos) // Именно такой порядок ! (Однонаправленые Лучи)
				processLight(wPX, DeflectorLType::eDefSun, L, true);
		}
	}

	// Hemi Lights
	if (!(flags & LP_dont_hemi))
	{
		for (R_Light& L : lights.hemi)
		{
			for (auto& wPX : world_pos) // Именно такой порядок ! (Однонаправленые Лучи)
				processLight(wPX, DeflectorLType::eDefHemi, L, true);
		}
	}
 
	// Packed Rays Process !
	EmbreeMain.RaytrraceRayPack(rays);
}

void LightPoint_Details(xr_vector<DetailsTask>& world_pos, base_lighting& lights, u32 flags)
{
	rays.clear();
	auto processLight = [&](DetailsTask& wPX, DeflectorLType LType, R_Light& L, bool isSunOrHemi)
	{
		Fvector Ldir;
		Fvector Pnew = wPX.wP;
		Pnew.mad(wPX.wN, 0.01f);
		float att = 0.0f;

		Fvector P = wPX.wP;
		Fvector N = wPX.wN;
		float R = 0;

		switch (L.type)
		{
			case LT_DIRECT:
			{
				Ldir.invert(L.direction);
				float D = Ldir.dotproduct(N);
				if (D <= 0) return;

				att = isSunOrHemi ? L.energy : D * L.energy;
				R = 1000.0f;
			} break;

			case LT_POINT:
			case LT_SECONDARY:
			{
				float sqD = P.distance_to_sqr(L.position);
				if (sqD > L.range2)					return;
 				 
				Ldir.sub(L.position, P).normalize_safe();
				float D = Ldir.dotproduct(N);
				if (D <= 0)							return;
				R = _sqrt(sqD);

				if (L.type == LT_SECONDARY)
				{
					D *= -Ldir.dotproduct(L.direction);
					if (D <= 0)						return;

					att = powf(D, 0.125f) * L.energy * (1 - R / L.range);
				}
				else
				{
					float scale = D * L.energy;
					if (isSunOrHemi)
						att = scale / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD);
					else
						att = scale * (1 / (L.attenuation0 + L.attenuation1 * R + L.attenuation2 * sqD) - R * L.falloff);
				}

			} break;
		}

		rays.emplace_back(Pnew, Ldir, R, nullptr, att, LType, &wPX.C);
	};

	// RGB Lights
	if (!(flags & LP_dont_rgb))
	{
		for (R_Light& L : lights.rgb)
		{
			for (auto& wPX : world_pos)	// Именно такой порядок ! (Однонаправленые Лучи)
				processLight(wPX, DeflectorLType::eDefRgb, L, false);
		}
	}

	// Sun Lights
	if (!(flags & LP_dont_sun))
	{
		for (R_Light& L : lights.sun)
		{
			for (auto& wPX : world_pos) // Именно такой порядок ! (Однонаправленые Лучи)
				processLight(wPX, DeflectorLType::eDefSun, L, true);
		}
	}

	// Hemi Lights
	if (!(flags & LP_dont_hemi))
	{
		for (R_Light& L : lights.hemi)
		{
			for (auto& wPX : world_pos) // Именно такой порядок ! (Однонаправленые Лучи)
				processLight(wPX, DeflectorLType::eDefHemi, L, true);
		}
	}

	// Packed Rays Process !
	EmbreeMain.RaytrraceRayPack(rays);
}
