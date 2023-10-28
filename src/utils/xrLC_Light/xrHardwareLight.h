#pragma once
#include "../../xrCDB/xrCDB.h"
#include "xrRayDefinition.h"
#include "base_lighting.h"
#include "b_build_texture.h"
#include "optix\putil\Buffer.h"
#include "xrFaceDefs.h"
#include "base_color.h"

struct xrHardwareLightBatchTask
{

};

class XRLC_LIGHT_API xrHardwareLight
{
public:

	static BOOL IsHardwareAccelerationSupported();

	static xrHardwareLight& Get();

	static bool IsEnabled();

	xrHardwareLight();
	~xrHardwareLight();

	void LoadLevel(CDB::MODEL* RaycastModel, base_lighting& Ligtings, xr_vector<b_BuildTexture>& Textures);

	void PerformRaycast(xr_vector<RayRequest>& InRays, int flag, xr_vector<base_color_c>& OutHits);

	void CalculateLightmap(int DeflectorID, struct lm_layer& LightMapRef);

	void PerformAdaptiveHT();

	float GetEnergyFromSelectedLight(xr_vector<int>& RGBLightIndexes, xr_vector<int>& SunLightIndexes, xr_vector<int>& HemiLightIndexes);

	xr_vector<Fvector> GetDebugPCHitData();
	xr_vector<Fvector> GetDebugGPUHitData();

	//here goes special functions for new batching concept


private:

	void GetLevelIndices(vecVertex& InLevelVertices, vecFace& InLevelFaces, xr_vector <PolyIndexes>& OutLevelIndices, xr_vector<HardwareVector>& OutLevelVertexes);


	__forceinline void CheckCudaError(cudaError_t ErrorCode);

	//Master struct
	Buffer<xrHardwareLCGlobalData>* GlobalData;

	//Light data
	Buffer<LightSizeInfo>* LightSizeBuffer;
	Buffer<R_Light>* LightBuffer;

	//Level geometry data
	Buffer<PolyIndexes>* TrisBuffer;
	Buffer<Fvector>* VertBuffer;
	Buffer<HardwareVector>* VertNormalBuffer;

	//Textures
	Buffer<xrHardwareTexture>* TextureBuffer;

	static bool _IsEnabled;

};