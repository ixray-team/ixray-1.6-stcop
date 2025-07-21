#include "stdafx.h"
#include "xrDeflector.h"
#include "R_light.h"
#include "light_point.h"
#include "base_lighting.h"
#include "xrLC_GlobalData.h"

#include "EmbreeRayTrace.h"

void Embree::VertexEmbree::Set(Fvector& vertex)
{
	x = vertex.x;
	y = vertex.y;
	z = vertex.z;
}

Fvector Embree::VertexEmbree::Get()
{
	Fvector vertex;
	x = vertex.x;
	y = vertex.y;
	z = vertex.z;
	return vertex;
}

void Embree::TriEmbree::SetVertexes(CDB::TRI& triangle, Fvector* verts, VertexEmbree* emb_verts, size_t& last_index)
{
	point1 = last_index;
	point2 = last_index + 1;
	point3 = last_index + 2;

	int v1 = triangle.verts[0];
	int v2 = triangle.verts[1];
	int v3 = triangle.verts[2];


	emb_verts[last_index].Set(verts[v1]);
	emb_verts[last_index + 1].Set(verts[v2]);
	emb_verts[last_index + 2].Set(verts[v3]);

	last_index += 3;
}


void Embree::SetRay1(RTCRay& rayhit, Fvector& pos, Fvector& dir, float range)
{
	rayhit.dir_x = dir.x;
	rayhit.dir_y = dir.y;
	rayhit.dir_z = dir.z;
	rayhit.org_x = pos.x;
	rayhit.org_y = pos.y;
	rayhit.org_z = pos.z;
	rayhit.tnear = 0.101f;
	rayhit.tfar = range;
	rayhit.mask = (unsigned int)(-1);
	rayhit.flags = 0;
}

void Embree::SetRay1(RTCRayHit& rayhit, Fvector& pos, Fvector& dir, float range)
{
	rayhit.ray.dir_x = dir.x;
	rayhit.ray.dir_y = dir.y;
	rayhit.ray.dir_z = dir.z;
	rayhit.ray.org_x = pos.x;
	rayhit.ray.org_y = pos.y;
	rayhit.ray.org_z = pos.z;
	rayhit.ray.tnear = 0.101f;
	rayhit.ray.tfar = range;
	rayhit.ray.mask = (unsigned int)(-1);
	rayhit.ray.flags = 0;
}

// OFF PACKED PROCESSING
void GetEmbreeDeviceProperty(LPCSTR msg, RTCDevice& device, RTCDeviceProperty prop)
{
	clMsg("EmbreeDevProp: %s : %llu", msg, rtcGetDeviceProperty(device, prop));
}

void Embree::errorFunction(void* userPtr, RTCError error, const char* str)
{
	clMsg("error %d: %s", error, str);
	DebugBreak();
}


void Embree::IntelEmbreeSettings(RTCDevice& device, bool avx, bool sse)
{
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_RAY_MASK_SUPPORTED", device, RTC_DEVICE_PROPERTY_RAY_MASK_SUPPORTED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_BACKFACE_CULLING_ENABLED", device, RTC_DEVICE_PROPERTY_BACKFACE_CULLING_ENABLED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_NATIVE_RAY4_SUPPORTED", device, RTC_DEVICE_PROPERTY_NATIVE_RAY4_SUPPORTED);

	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_NATIVE_RAY8_SUPPORTED", device, RTC_DEVICE_PROPERTY_NATIVE_RAY8_SUPPORTED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_NATIVE_RAY16_SUPPORTED", device, RTC_DEVICE_PROPERTY_NATIVE_RAY16_SUPPORTED);
	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_IGNORE_INVALID_RAYS_ENABLED", device, RTC_DEVICE_PROPERTY_IGNORE_INVALID_RAYS_ENABLED);

	GetEmbreeDeviceProperty("RTC_DEVICE_PROPERTY_TASKING_SYSTEM", device, RTC_DEVICE_PROPERTY_TASKING_SYSTEM);
}

float EmbreeRayTrace(R_Light& L, Fvector& P, Fvector& D, float R, Face* skip, BOOL bUseFaceDisable)
{
	return RaytraceEmbreeProcess(L, P, D, R, skip);
}

void LightPointEmbree(base_color_c& C, Fvector& P, Fvector& N, base_lighting& lights, u32 flags, void* data_skip)
{
	Fvector		Ldir, Pnew;
	Pnew.mad(P, N, 0.01f);

	BOOL		bUseFaceDisable = flags & LP_UseFaceDisable;
	float		MAX_DISTANCE = 1000.0f;

	Face* skip = (Face*)data_skip;

	if (0 == (flags & LP_dont_rgb))
	{
		R_Light* L = &*lights.rgb.begin(), * E = &*lights.rgb.end();
		for (; L != E; L++)
		{
			switch (L->type)
			{
			case LT_DIRECT:
			{
				// Cos
				Ldir.invert(L->direction);
				float D = Ldir.dotproduct(N);
				if (D <= 0) continue;

				// Trace Light
				float scale = D * L->energy * EmbreeRayTrace(*L, Pnew, Ldir, MAX_DISTANCE, skip, bUseFaceDisable);
				C.rgb.x += scale * L->diffuse.x;
				C.rgb.y += scale * L->diffuse.y;
				C.rgb.z += scale * L->diffuse.z;
			}
			break;
			case LT_POINT:
			{
				// Distance
				float sqD = P.distance_to_sqr(L->position);
				if (sqD > L->range2) continue;

				// Dir
				Ldir.sub(L->position, P);
				Ldir.normalize_safe();
				float D = Ldir.dotproduct(N);
				if (D <= 0)			continue;

				// Trace Light
				float R = _sqrt(sqD);
				float scale = D * L->energy * EmbreeRayTrace(*L, Pnew, Ldir, R, skip, bUseFaceDisable);
				float A;

				if (inlc_global_data()->gl_linear())
				{
					A = 1 - R / L->range;
				}
				else
				{
					//	Igor: let A equal 0 at the light boundary
					A = scale *
						(
							1 / (L->attenuation0 + L->attenuation1 * R + L->attenuation2 * sqD) -
							R * L->falloff
							);

				}

				C.rgb.x += A * L->diffuse.x;
				C.rgb.y += A * L->diffuse.y;
				C.rgb.z += A * L->diffuse.z;
			}
			break;
			case LT_SECONDARY:
			{
				// Distance
				float sqD = P.distance_to_sqr(L->position);
				if (sqD > L->range2) continue;

				// Dir
				Ldir.sub(L->position, P);
				Ldir.normalize_safe();
				float	D = Ldir.dotproduct(N);
				if (D <= 0) continue;
				D *= -Ldir.dotproduct(L->direction);
				if (D <= 0) continue;

				// Jitter + trace light -> monte-carlo method
				Fvector	Psave = L->position, Pdir;
				L->position.mad(Pdir.random_dir(L->direction, PI_DIV_4), .05f);

				float R = _sqrt(sqD);
				float scale = powf(D, 1.f / 8.f) * L->energy * EmbreeRayTrace(*L, Pnew, Ldir, R, skip, bUseFaceDisable);
				float A = scale * (1 - R / L->range);
				L->position = Psave;

				C.rgb.x += A * L->diffuse.x;
				C.rgb.y += A * L->diffuse.y;
				C.rgb.z += A * L->diffuse.z;
			}
			break;
			}
		}
	}

	if (0 == (flags & LP_dont_sun))
	{
		R_Light* L = &*(lights.sun.begin()), * E = &*(lights.sun.end());
		for (; L != E; L++)
		{
			if (L->type == LT_DIRECT)
			{
				// Cos
				Ldir.invert(L->direction);
				float D = Ldir.dotproduct(N);
				if (D <= 0) continue;

				// Trace Light
				float scale = L->energy * EmbreeRayTrace(*L, Pnew, Ldir, MAX_DISTANCE, skip, bUseFaceDisable);
				C.sun += scale;
			}
			else
			{
				// Distance
				float sqD = P.distance_to_sqr(L->position);
				if (sqD > L->range2) continue;

				// Dir
				Ldir.sub(L->position, P);
				Ldir.normalize_safe();
				float D = Ldir.dotproduct(N);
				if (D <= 0)			continue;

				// Trace Light
				float R = _sqrt(sqD);
				float scale = D * L->energy * EmbreeRayTrace(*L, Pnew, Ldir, R, skip, bUseFaceDisable);
				float A = scale / (L->attenuation0 + L->attenuation1 * R + L->attenuation2 * sqD);

				C.sun += A;
			}
		}
	}

	if (0 == (flags & LP_dont_hemi))
	{
		R_Light* L = &*lights.hemi.begin(), * E = &*lights.hemi.end();
		for (; L != E; L++)
		{
			if (L->type == LT_DIRECT)
			{
				// Cos
				Ldir.invert(L->direction);
				float D = Ldir.dotproduct(N);
				if (D <= 0) continue;


				// Trace Light
				Fvector		PMoved;
				PMoved.mad(Pnew, Ldir, 0.001f);
				float scale = L->energy * EmbreeRayTrace(*L, PMoved, Ldir, MAX_DISTANCE, skip, bUseFaceDisable);
				C.hemi += scale;
			}
			else
			{
				// Distance
				float sqD = P.distance_to_sqr(L->position);
				if (sqD > L->range2) continue;

				// Dir
				Ldir.sub(L->position, P);
				Ldir.normalize_safe();
				float D = Ldir.dotproduct(N);
				if (D <= 0) continue;

				// Trace Light
				float R = _sqrt(sqD);
				float scale = D * L->energy * EmbreeRayTrace(*L, Pnew, Ldir, R, skip, bUseFaceDisable);
				float A = scale / (L->attenuation0 + L->attenuation1 * R + L->attenuation2 * sqD);

				C.hemi += A;
			}

		}
	}
}