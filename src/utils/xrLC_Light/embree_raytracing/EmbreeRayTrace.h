#pragma once
#include <embree4/rtcore.h>
#include "EmbreeGeometryBuilder.h"
#include "xrRaysDefines.h"

// ВАЖНЫЙ ПАРАМЕТР TNEAR Для пересечения с водой
void SetRay1(RTCRay& rayhit, const Fvector& pos, const Fvector& dir, float near_, float range);
void SetRay4(RTCRay4& rayhit, u32 IDX, const Fvector& pos, const Fvector& dir, float near_, float range);
void SetRay8(RTCRay8& rayhit, u32 IDX, const Fvector& pos, const Fvector& dir, float near_, float range);
 
// Vertex, Tri Buffers
extern RTCDevice	EmbreeDevice;
static bool	isDeviceInitialized = false;

const char* GetDeviceConfig();
void InitializeEmbreeDevice();


static void ConvertMatrix(const Fmatrix& M, float out[16])
{
	// Embree expects COLUMN MAJOR
 	out[0] = M._11;
	out[1] = M._12;
	out[2] = M._13;
	out[3] = M._14;

	out[4] = M._21;
	out[5] = M._22;
	out[6] = M._23;
	out[7] = M._24;

	out[8] = M._31;
	out[9] = M._32;
	out[10] = M._33;
	out[11] = M._34;

	out[12] = M._41;
	out[13] = M._42;
	out[14] = M._43;
	out[15] = M._44;
}

// Packed Rays Structure!
 
struct RayQueryContext : RTCRayQueryContext
{
	alignas(32) void* skip[8];
	alignas(32) float energy[8];
	alignas(32) u8	  hits_result[8];
};

struct alignas(32) UserGeomData
{
	u32 DummyType = 0;
 	xr_vector<void*> dummys; // точно ли оно удаляется??
};


class EmbreeInstancedModel
{
	RTCScene InstaceScene;
	RTCGeometry GeometryTransp;
	RTCGeometry GeometryOpacue;
public:
	~EmbreeInstancedModel();
	void InitializeModel(xr_vector<FaceDataEmbree>& faces);
	void SetInstance(RTCScene scene, Fmatrix& xform);
};


class EmbreeRayTraceModel
{
public:
	RTCScene					IntelScene;	
 
protected:
	RTCSceneFlags				scene_flags			= RTC_SCENE_FLAG_NONE;
	RTCBuildQuality				scene_quality		= RTC_BUILD_QUALITY_HIGH;

	RTCGeometry					IntelGeometryNormal = nullptr;
	RTCGeometry					IntelGeometryTransp = nullptr;

	/** NORMAL GEOM **/
	TriangleContainer			transp_geom;
	TriangleContainer			opacue_geom;
  
	void BuildRayTraceModel();
	void BuildRayTraceModel_Instaced();

	// Details Model
public:
	// Loading 
 	float RaytraceEmbreeProcess( Fvector& P, Fvector& N, float range, void* skip);
	void  RaytrraceRayPack(xr_vector< RayTask >& rays);

  
	void  AttachGeomToScene(bool isMain, u8 uDataType);
	void  InitializeGeometry();		// Rcast-model
	void  InitializeGeometry_Model(xr_vector<FaceDataEmbree> & faces); // Single-Models (xrMU-Model)
	
	void  InitializeDetails(xr_vector<FaceDataEmbree>& faces);

 	void  IntelEmbereUnloadAll();
	void  RemoveGeometry();

	// Details Loading 
 	// void InitEmbreeDetails();
	
	// Instances
	xr_vector<EmbreeInstancedModel> instanced;

	void UpdateSceneFlags();
};

extern EmbreeRayTraceModel EmbreeMain;
 
void GetEmbreeDeviceProperty(const char* msg, RTCDevice& device, RTCDeviceProperty prop);