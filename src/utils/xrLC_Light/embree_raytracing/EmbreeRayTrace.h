#pragma once

#include "R_light.h"
#include "base_lighting.h"
#include "base_color.h"
#include "../../xrCore/Collision/xrCDB.h"

#include "xrFace.h"
#include <embree4/rtcore.h>

#include "EmbreeGeometryBuilder.h"

struct FaceDataIntel
{
	Fvector v1, v2, v3;
	void* ptr;
};

// ВАЖНЫЙ ПАРАМЕТР TNEAR Для пересечения с водой
void SetRay1(RTCRay& rayhit, Fvector& pos, Fvector& dir, float near_, float range);
void SetRay1(RTCRayHit& rayhit, Fvector& pos, Fvector& dir, float near_, float range);
 
struct BuildData
{
	xr_vector<CDB::TRI> build_faces;
	xr_vector<Fvector>  build_verts;

	u32		  build_vcnt;
	u32		  build_fcnt;
};

// Vertex, Tri Buffers
class EmbreeData
{
public:
	BuildData  build_data;

	/** NORMAL GEOM **/
	TriangleContainer			static_geom;
	TriangleContainer			static_geom_transp;
 
	void BuildRaytraceModel_2	();
 	void BuildRaytraceModel		();
	void BuildRcast();
 	
	// Loading 
	bool  isInitialized = false;
	void  RemoveGeometry(bool isDealloc);
	float RaytraceEmbreeProcess(R_Light& L, Fvector& P, Fvector& N, float range, void* skip);
	void  InitializeGeometry();
 
	void IntializeDevice();
	void IntelEmbereLOAD();
	void IntelEmbereUNLOAD();

	// Details Loading 
	void ConsturctGeometry();
	void InitEmbreeDetails();
};

extern EmbreeData EmbreeMain;
 
void GetEmbreeDeviceProperty(LPCSTR msg, RTCDevice& device, RTCDeviceProperty prop);