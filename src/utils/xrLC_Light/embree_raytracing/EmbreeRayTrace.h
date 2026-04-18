#pragma once

#include "R_light.h"
#include "base_lighting.h"
#include "base_color.h"
#include "../../xrCore/Collision/xrCDB.h"

#include "xrFace.h"
#include <embree4/rtcore.h>

#include "EmbreeGeometryBuilder.h"

struct FaceDataEmbree
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
static RTCDevice	EmbreeDevice		= nullptr;
static bool			isDeviceInitialized = false;

const char* GetDeviceConfig();
void InitializeEmbreeDevice();

class EmbreeRayTraceModel
{
protected:
	RTCSceneFlags	scene_flags		= RTC_SCENE_FLAG_NONE;
	RTCBuildQuality scene_quality	= RTC_BUILD_QUALITY_LOW;

	RTCScene	IntelScene			= nullptr;
	RTCGeometry IntelGeometryNormal = nullptr;
	RTCGeometry IntelGeometryTransp = nullptr;
 
	/** NORMAL GEOM **/
	TriangleContainer			static_geom;
	TriangleContainer			static_geom_transp;
 

	void RemoveGeometry();
 	void CommitScene();

	void BuildModel(xr_vector<FaceDataEmbree> & faces);
	void BuildRaytraceModel_2	();
 	void BuildRaytraceModel		();
 
public:
	// Rcast Model Constructing (Build.cform)
	BuildData	build_data;
	void BuildRcast();

	// Loading 
	float RaytraceEmbreeProcess( Fvector& P, Fvector& N, float range, void* skip);
	void  InitializeGeometry();		// Rcast-model
	void  InitializeGeometry_Model(xr_vector<FaceDataEmbree> & faces); // Single-Models (xrMU-Model)
  
 	void  IntelEmbereUnloadAll();

	// Details Loading 
	RTCScene	IntelSceneDetails = nullptr;
	RTCGeometry IntelGeometryDetails = nullptr;
	float RaytraceEmbreeDetails(Fvector& P, Fvector& N, float range);
	void InitEmbreeDetails();
};

extern EmbreeRayTraceModel EmbreeMain;
 
void GetEmbreeDeviceProperty(const char* msg, RTCDevice& device, RTCDeviceProperty prop);