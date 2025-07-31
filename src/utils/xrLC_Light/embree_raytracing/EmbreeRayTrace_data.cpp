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


void Embree::SetRay1(RTCRay& rayhit, Fvector& pos, Fvector& dir, float near_, float range)
{
	rayhit.dir_x = dir.x;
	rayhit.dir_y = dir.y;
	rayhit.dir_z = dir.z;
	rayhit.org_x = pos.x;
	rayhit.org_y = pos.y;
	rayhit.org_z = pos.z;
	rayhit.tnear = near_;
	rayhit.tfar = range;
	rayhit.mask = (unsigned int)(-1);
	rayhit.flags = 0;
}

void Embree::SetRay1(RTCRayHit& rayhit, Fvector& pos, Fvector& dir, float near_, float range)
{
	rayhit.ray.dir_x = dir.x;
	rayhit.ray.dir_y = dir.y;
	rayhit.ray.dir_z = dir.z;
	rayhit.ray.org_x = pos.x;
	rayhit.ray.org_y = pos.y;
	rayhit.ray.org_z = pos.z;
	rayhit.ray.tnear = near_;
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
  