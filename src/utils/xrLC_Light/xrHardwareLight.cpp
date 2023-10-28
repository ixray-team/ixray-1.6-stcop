#include "stdafx.h"
#include "optix\optix_primepp.h"
#include "xrFaceDefs.h"
#include "xrFace.h"
#include "xrHardwareLight.h"
#include "xrDeflector.h"
#include "xrLC_GlobalData.h"
#include "light_point.h"
#include "base_color.h"
#include <cuda_runtime.h>


struct OptixGlobal
{
	optix::prime::Context PrimeContext;

	//level buffers
	optix::prime::BufferDesc LevelIndixes;
	optix::prime::BufferDesc LevelVertexes;
	optix::prime::Model LevelModel;

	xr_vector <PolyIndexes> LevelIndices;
	xr_vector <HardwareVector> LevelVertexData;

	xr_vector<Fvector> CPURayVertexData;
	xr_vector<Fvector> GPURayVertexData;
};
OptixGlobal* g_pOptix = nullptr;

///#REFACTOR
//static buffer declaration, replace with pimpl paradigm when refactor time happen
Buffer<HardwareVector> CDBVertexesBuffer;
Buffer<PolyIndexes> CDBTrisIndexBuffer;
Buffer<TrisAdditionInfo> CDBTrisAdditionBuffer;

extern "C"
{
	cudaError_t RunCheckRayOptimizeOut(xrHardwareLCGlobalData* GlobalData, RayRequest* RequestedRays, char* StatusBuffer, u64 MaxPossibleRays, int flag);
	cudaError_t RunGenerateRaysForTask(xrHardwareLCGlobalData* GlobalData, RayRequest* RequestedRays, Ray* RayBuffer, u32* AliveRaysIndexes, u32 AliveRaysCount, int flag);
	cudaError_t RunProcessHits(xrHardwareLCGlobalData* GlobalData, Ray* RayBuffer, Hit* HitBuffer, float* ColorBuffer, char* RayStatusBuffer, u32* AliveRaysIndexes, u32 AliveRaysCount, bool IsFirstTime, int flag, u64* FacesToSkip);
	cudaError_t RunFinalizeRays(xrHardwareLCGlobalData* GlobalData, float* EnergyBuffer, RayRequest* RequestedRays, u32 RequestedRaysCount, base_color_c* OutColors, u32* AliveRaysIndexes, u32 AliveRaysCount, int flag, int* SurfacePointStartLoc);
}


inline void CheckCudaErr(cudaError_t error) {
	if (error != cudaError_t::cudaSuccess)
	{

		DebugBreak();
	}
}


BOOL xrHardwareLight::IsHardwareAccelerationSupported() {
	RTPcontext Context;
	RTPresult Result = rtpContextCreate(RTP_CONTEXT_TYPE_CUDA, &Context);

	if (Result == RTP_SUCCESS)
	{
		rtpContextDestroy(Context);
		return TRUE;
	}
	return FALSE;
}

xrHardwareLight& xrHardwareLight::Get() {
	static xrHardwareLight MainSingleton;
	return MainSingleton;
}

bool xrHardwareLight::IsEnabled() {
	return _IsEnabled;
}

xrHardwareLight::xrHardwareLight() :
	LightBuffer(nullptr),
	LightSizeBuffer(nullptr),
	TrisBuffer(nullptr),
	VertBuffer(nullptr),
	TextureBuffer(nullptr),
	GlobalData(nullptr),
	VertNormalBuffer(nullptr)
{
	g_pOptix = new OptixGlobal;
	g_pOptix->PrimeContext = optix::prime::Context::create(RTP_CONTEXT_TYPE_CUDA);
	LightSizeBuffer = new Buffer<LightSizeInfo>(1, RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
}

xrHardwareLight::~xrHardwareLight() 
{
	if (LightBuffer != nullptr)		delete LightBuffer;
	if (LightSizeBuffer != nullptr)		delete LightSizeBuffer;
	if (TrisBuffer != nullptr)		delete TrisBuffer;
	if (VertBuffer != nullptr)		delete VertBuffer;
	if (TextureBuffer != nullptr)		delete TextureBuffer;
	if (GlobalData != nullptr)		delete GlobalData;
	if (VertNormalBuffer != nullptr)	delete VertNormalBuffer;

	CDBVertexesBuffer.free();
	CDBTrisIndexBuffer.free();
	CDBTrisAdditionBuffer.free();

	delete g_pOptix;
}

void xrHardwareLight::LoadLevel(CDB::MODEL* RaycastModel, base_lighting& Lightings, xr_vector<b_BuildTexture>& Textures) {
	Progress(0.0f);
	_IsEnabled = true;
	cudaError_t DebugErr = cudaError_t::cudaSuccess;
	vecFace& Polygons = inlc_global_data()->g_faces();

	//###LEVEL GEOMETRY

	//load CDB::MODEL and original model
	//LOAD CDB::MODEL
	HardwareModel OptimizedModel;

	//vertexes
	CDBVertexesBuffer.alloc(RaycastModel->get_verts_count(), RTP_BUFFER_TYPE_CUDA_LINEAR, UNLOCKED);
	DebugErr = cudaMemcpy(CDBVertexesBuffer.ptr(), RaycastModel->get_verts(), RaycastModel->get_verts_count() * sizeof(Fvector), cudaMemcpyHostToDevice);
	CheckCudaErr(DebugErr);
	OptimizedModel.Vertexes = CDBVertexesBuffer.ptr();
	OptimizedModel.VertexCount = CDBVertexesBuffer.count();
	OptimizedModel.VertexNormal = nullptr;
	//normals
	//skip normals no way to retrieve it from optimized model
	//hmm..... 
	//RaycastModel->get_tris()->
	xr_vector <PolyIndexes> OptimizedMeshTris;
	OptimizedMeshTris.reserve(RaycastModel->get_tris_count());

	xr_vector<TrisAdditionInfo> OptimizedTrisAdditionInfo;
	OptimizedTrisAdditionInfo.reserve(RaycastModel->get_tris_count());
	for (int i = 0; i < RaycastModel->get_tris_count(); i++)
	{
		CDB::TRI Tris = RaycastModel->get_tris()[i];
		PolyIndexes indx{ Tris.verts[0], Tris.verts[1], Tris.verts[2] };

		OptimizedMeshTris.push_back(indx);

		TrisAdditionInfo AdditionInfo;

		base_Face* FaceRef = convert_nax(Tris.dummy);
		const Shader_xrLC& TrisShader = FaceRef->Shader();
		AdditionInfo.CastShadow = !!TrisShader.flags.bLIGHT_CastShadow;

		Fvector2* pCDBTexCoord = FaceRef->getTC0();
		AdditionInfo.TexCoords = *pCDBTexCoord;

		b_material& TrisMaterial = inlc_global_data()->materials()[FaceRef->dwMaterial];
		AdditionInfo.TextureID = TrisMaterial.surfidx;

		//#WARNING: :(
		AdditionInfo.FaceID = (u64)&FaceRef;
		OptimizedTrisAdditionInfo.push_back(AdditionInfo);
	}


	CDBTrisIndexBuffer.alloc(OptimizedMeshTris.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, UNLOCKED);
	DebugErr = cudaMemcpy(CDBTrisIndexBuffer.ptr(), OptimizedMeshTris.data(), OptimizedMeshTris.size() * sizeof(PolyIndexes), cudaMemcpyHostToDevice);
	CheckCudaErr(DebugErr);

	CDBTrisAdditionBuffer.alloc(OptimizedTrisAdditionInfo.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, UNLOCKED);
	DebugErr = cudaMemcpy(CDBTrisAdditionBuffer.ptr(), OptimizedTrisAdditionInfo.data(), OptimizedTrisAdditionInfo.size() * sizeof(TrisAdditionInfo), cudaMemcpyHostToDevice);
	CheckCudaErr(DebugErr);

	OptimizedModel.Tris = CDBTrisIndexBuffer.ptr();
	OptimizedModel.TrianglesAdditionInfo = CDBTrisAdditionBuffer.ptr();

	Progress(0.1f);

	//ORIGINAL MODEL
#if 0
	HardwareModel OriginalModel;

	vecVertex& LevelVertexesData = inlc_global_data()->g_vertices();

	GetLevelIndices(LevelVertexesData, Polygons, LevelIndices, LevelVertexData);

	//at this moment our progress bar is on 0.6
	TrisBuffer = new Buffer<PolyIndexes>(LevelIndices.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, UNLOCKED);
	VertBuffer = new Buffer<Fvector>(LevelVertexesData.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, UNLOCKED);
	VertNormalBuffer = new Buffer<HardwareVector>(LevelVertexesData.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, UNLOCKED);

	DebugErr = cudaMemcpy(TrisBuffer->ptr(), LevelIndices.data(), LevelIndices.size() * sizeof(PolyIndexes), cudaMemcpyHostToDevice);
	DebugErr = cudaMemcpy(VertBuffer->ptr(), LevelVertexData.data(), LevelVertexData.size() * sizeof(HardwareVector), cudaMemcpyHostToDevice);
	Progress(0.65f);

	//load Vertex normals
	xr_vector <HardwareVector> VertexNormals;
	VertexNormals.reserve(LevelVertexesData.size());

	for (Vertex* Vert : LevelVertexesData)
	{

		HardwareVector VertexNormal = Vert->N;
		VertexNormals.push_back(VertexNormal);
	}

	DebugErr = cudaMemcpy(VertNormalBuffer->ptr(), VertexNormals.data(), VertexNormals.size() * sizeof(HardwareVector), cudaMemcpyHostToDevice);

	OriginalModel.Tris = TrisBuffer->ptr();
	OriginalModel.Vertexes = (HardwareVector*)VertBuffer->ptr();
	OriginalModel.VertexCount = VertBuffer->count();
	OriginalModel.VertexNormal = VertNormalBuffer->ptr();
	//NYI
	OriginalModel.TrianglesAdditionInfo = nullptr;
#endif


	Progress(0.7f);
	//OPTIX INIT

	g_pOptix->LevelIndixes = g_pOptix->PrimeContext->createBufferDesc(RTP_BUFFER_FORMAT_INDICES_INT3, RTP_BUFFER_TYPE_CUDA_LINEAR, CDBTrisIndexBuffer.ptr());
	g_pOptix->LevelIndixes->setRange(0, CDBTrisIndexBuffer.count());

	g_pOptix->LevelVertexes = g_pOptix->PrimeContext->createBufferDesc(RTP_BUFFER_FORMAT_VERTEX_FLOAT3, RTP_BUFFER_TYPE_CUDA_LINEAR, CDBVertexesBuffer.ptr());
	g_pOptix->LevelVertexes->setRange(0, CDBVertexesBuffer.count());

	g_pOptix->LevelModel = g_pOptix->PrimeContext->createModel();
	g_pOptix->LevelModel->setTriangles(g_pOptix->LevelIndixes, g_pOptix->LevelVertexes);
	g_pOptix->LevelModel->update(0);

	Progress(0.75f);


	{
		//###TEXTURES
		xr_vector <xrHardwareTexture> TextureDescription; TextureDescription.reserve(Textures.size());
		u32 OverallTextures = Textures.size();
		for (u32 TextureID = 0; TextureID < OverallTextures; TextureID++)
		{
			b_texture& Tex = Textures[TextureID];
			//Manually alloc device ptr to hold texture data
			__int64 TextureMem = (Tex.dwHeight * Tex.dwWidth) * 4;
			void* DeviceMem = nullptr;
			if (Tex.pSurface != nullptr)
			{
				DebugErr = cudaMalloc(&DeviceMem, TextureMem);
				CheckCudaErr(DebugErr);
				//copy texture data
				DebugErr = cudaMemcpy(DeviceMem, Tex.pSurface, TextureMem, cudaMemcpyHostToDevice);
				CheckCudaErr(DebugErr);
			}

			//create tex description
			xrHardwareTexture TexDesc;
			TexDesc.Width = Tex.dwWidth;
			TexDesc.Height = Tex.dwHeight;
			TexDesc.IsHaveAlpha = !!Tex.bHasAlpha;
			TexDesc.Pixels = (xrHardwarePixel*)DeviceMem;

			TextureDescription.push_back(TexDesc);

			float CurrentStageProgress = float(TextureID) / float(OverallTextures);
			Progress(0.75f + (0.2f * CurrentStageProgress));
		}

		TextureBuffer = new Buffer<xrHardwareTexture>(Textures.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, UNLOCKED);
		DebugErr = cudaMemcpy(TextureBuffer->ptr(), TextureDescription.data(), Textures.size() * sizeof(xrHardwareTexture), cudaMemcpyHostToDevice);
		CheckCudaErr(DebugErr);

	}

	Progress(0.95f);
	//###LIGHT INFO

	int RGBSize = Lightings.rgb.size();
	int SunSize = Lightings.sun.size();
	int HemiSize = Lightings.hemi.size();
	LightSizeInfo LightSize{ RGBSize, SunSize, HemiSize };

	DebugErr = cudaMemcpy(LightSizeBuffer->ptr(), &LightSize, sizeof(LightSizeInfo), cudaMemcpyHostToDevice);
	CheckCudaErr(DebugErr);

	LightBuffer = new Buffer<R_Light>(RGBSize + SunSize + HemiSize, RTP_BUFFER_TYPE_CUDA_LINEAR, UNLOCKED);
	R_Light* LightDevicePtr = LightBuffer->ptr();
	__int64 RGBLightDataSize = sizeof(R_Light) * RGBSize;
	__int64 SunLightDataSize = sizeof(R_Light) * SunSize;
	__int64 HemiLightDataSize = sizeof(R_Light) * HemiSize;

	//	RGB
	DebugErr = cudaMemcpy(LightDevicePtr, Lightings.rgb.data(), RGBLightDataSize, cudaMemcpyHostToDevice);
	CheckCudaErr(DebugErr);

	//	Sun
	LightDevicePtr += RGBSize;
	DebugErr = cudaMemcpy(LightDevicePtr, Lightings.sun.data(), SunLightDataSize, cudaMemcpyHostToDevice);
	CheckCudaErr(DebugErr);

	//	Hemi
	LightDevicePtr += SunSize;
	DebugErr = cudaMemcpy(LightDevicePtr, Lightings.hemi.data(), HemiLightDataSize, cudaMemcpyHostToDevice);
	CheckCudaErr(DebugErr);

	//finally create a single master ptr, to access all data in one place
	GlobalData = new Buffer<xrHardwareLCGlobalData>(1, RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
	xrHardwareLCGlobalData StructGlobalData;

	StructGlobalData.LightData = LightBuffer->ptr();
	StructGlobalData.LightSize = LightSizeBuffer->ptr();
	StructGlobalData.Textures = TextureBuffer->ptr();

	StructGlobalData.RaycastModel = OptimizedModel;

	DebugErr = cudaMemcpy(GlobalData->ptr(), &StructGlobalData, sizeof(xrHardwareLCGlobalData), cudaMemcpyHostToDevice);
	CheckCudaErr(DebugErr);
	Progress(1.0f);
}

void xrHardwareLight::PerformRaycast(xr_vector<RayRequest>& InRays, int flag, xr_vector<base_color_c>& OutHits) {
	//We use all static light in our calculations for now
	if (InRays.size() == 0)
	{
		clMsg("! PerformRaycast: Invoked without rays...");
		return;
	}

	cudaError_t DebugErr = cudaError_t::cudaSuccess;
	base_lighting& AllLights = inlc_global_data()->L_static();

	auto ZeroOutput = [](int RequestedRays, xr_vector<base_color_c>& TargetArray)
		{
			TargetArray.clear();
			TargetArray.reserve(RequestedRays);

			base_color_c ZeroColor;

			for (int i = 0; i < RequestedRays; ++i)
			{
				TargetArray.push_back(ZeroColor);
			}
		};

	u32 MaxRaysPerPoint = 0;
	if ((flag & LP_dont_rgb) == 0)
	{
		MaxRaysPerPoint += AllLights.rgb.size();
	}
	if ((flag & LP_dont_sun) == 0)
	{
		MaxRaysPerPoint += AllLights.sun.size();
	}
	if ((flag & LP_dont_hemi) == 0)
	{
		MaxRaysPerPoint += AllLights.hemi.size();
	}

	if (MaxRaysPerPoint == 0)
	{
		clMsg("! PerformRaycast invoked, but no lights can be accepted");
		clMsg("All static lights RGB: %d Sun: %d Hemi: %d", AllLights.rgb.size(), AllLights.sun.size(), AllLights.hemi.size());
		return;
	}

	u64 MaxPotentialRays = MaxRaysPerPoint * InRays.size();

	Buffer < RayRequest > RayBuffer(InRays.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
	DebugErr = cudaMemcpy(RayBuffer.ptr(), InRays.data(), InRays.size() * sizeof(RayRequest), cudaMemcpyHostToDevice);
	CheckCudaErr(DebugErr);

	Buffer<char> RayEnableStatusBuffer(MaxPotentialRays, RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
	DebugErr = cudaMemset(RayEnableStatusBuffer.ptr(), 0, MaxPotentialRays);
	CheckCudaErr(DebugErr);

	DebugErr = RunCheckRayOptimizeOut(GlobalData->ptr(), RayBuffer.ptr(), RayEnableStatusBuffer.ptr(), MaxPotentialRays, flag);
	CheckCudaErr(DebugErr);

	const char* StatusBuffer = RayEnableStatusBuffer.hostPtr();

	char* pItStatusBuffer = const_cast <char*> (StatusBuffer);
	//Let's count overall alive vertexes
	//u32 AliveRays = 0;
	//#HOTFIX: Build support array for ray finalizator
	//that array is a map, that correspond to each surface point that we have
	xr_vector <int> SurfacePoint2StartLoc;

	xr_vector <u32> AliveRaysIndexes;
	for (u32 Index = 0; Index < MaxPotentialRays; Index++)
	{
		char IsRayAlive = *pItStatusBuffer;
		if (IsRayAlive)
		{
			AliveRaysIndexes.push_back(Index);

			//conditionaly add a surface start point
			u32 SurfaceID = Index / MaxRaysPerPoint;
			u32 RegisteredPoints = SurfacePoint2StartLoc.size();
			if (RegisteredPoints < SurfaceID)
			{
				//wait... we miss a whole surface point?
				//how many surface points missed?
				u32 Differents = SurfaceID - RegisteredPoints;
				//add all missing. Declare it as -1, so we can skip them in FINALIZE stage
				for (int i = 0; i < Differents; ++i)
				{
					SurfacePoint2StartLoc.push_back(-1);
				}
			}
			if (RegisteredPoints == SurfaceID)
			{
				SurfacePoint2StartLoc.push_back((int)AliveRaysIndexes.size() - 1);
			}
		}
		pItStatusBuffer++;
	}
	//Status("%d rays optimized out", MaxPotentialRays - AliveRaysIndexes.size());
	if (AliveRaysIndexes.size() == 0)
	{
		//all rays are optimized
		ZeroOutput(InRays.size(), OutHits);
		return;
	}

	//create rays buffer and fill them through cuda
	Buffer <u32> DeviceAliveRaysIndexes(AliveRaysIndexes.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
	DebugErr = cudaMemcpy(DeviceAliveRaysIndexes.ptr(), AliveRaysIndexes.data(), AliveRaysIndexes.size() * sizeof(u32), cudaMemcpyHostToDevice);
	CheckCudaErr(DebugErr);

	Buffer <int> DeviceSurfacePoint2StartLoc(InRays.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
	DebugErr = cudaMemcpy(DeviceSurfacePoint2StartLoc.ptr(), SurfacePoint2StartLoc.data(), SurfacePoint2StartLoc.size() * sizeof(int), cudaMemcpyHostToDevice);
	CheckCudaErr(DebugErr);

	Buffer<Ray> OptimizedRaysVec(AliveRaysIndexes.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);

	DebugErr = RunGenerateRaysForTask(GlobalData->ptr(), RayBuffer.ptr(), OptimizedRaysVec.ptr(), DeviceAliveRaysIndexes.ptr(), AliveRaysIndexes.size(), flag);
	CheckCudaErr(DebugErr);

	Buffer <Hit> OptimizedHitsVec(AliveRaysIndexes.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
	DebugErr = cudaMemset(OptimizedHitsVec.ptr(), 0, OptimizedHitsVec.count() * sizeof(Hit));
	CheckCudaErr(DebugErr);

	optix::prime::BufferDesc OptmizedHitDescBuffer = g_pOptix->PrimeContext->createBufferDesc((RTPbufferformat)Hit::format, OptimizedHitsVec.type(), OptimizedHitsVec.ptr());
	OptmizedHitDescBuffer->setRange(0, OptimizedHitsVec.count());

	optix::prime::BufferDesc OptimizedRaysDescBuffer = g_pOptix->PrimeContext->createBufferDesc((RTPbufferformat)Ray::format, OptimizedRaysVec.type(), OptimizedRaysVec.ptr());
	OptimizedRaysDescBuffer->setRange(0, OptimizedRaysVec.count());

	optix::prime::Query LevelQuery = g_pOptix->LevelModel->createQuery(RTP_QUERY_TYPE_CLOSEST);
	LevelQuery->setHits(OptmizedHitDescBuffer);
	LevelQuery->setRays(OptimizedRaysDescBuffer);


	//if "skip face" mode enabled - load special buffer for every requested surface point

	u64* FacesToSkip = nullptr;
	bool SkipFaceMode = !!(flag & LP_UseFaceDisable);
	Buffer <u64> FaceToSkip(InRays.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);

	//go go round system!

	u32 AliveRays = AliveRaysIndexes.size();
	Buffer<float> RayEnergy(AliveRaysIndexes.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
	bool IsFirstCall = true;
	int Rounds = 0;

	if (SkipFaceMode)
	{
		xr_vector <u64> HostFaceToSkip; HostFaceToSkip.reserve(InRays.size());
		for (int RayRequestIndex = 0; RayRequestIndex < InRays.size(); ++RayRequestIndex)
		{
			HostFaceToSkip.push_back((u64)InRays[RayRequestIndex].FaceToSkip);
		}

		DebugErr = cudaMemcpy(FaceToSkip.ptr(), HostFaceToSkip.data(), HostFaceToSkip.size() * sizeof(u64), cudaMemcpyHostToDevice);
		CheckCudaErr(DebugErr);
		HostFaceToSkip.clear();

		FacesToSkip = FaceToSkip.ptr();
	}

	//OptimizedRaysVec.hostPtr();

	while (AliveRays)
	{
		LevelQuery->execute(0);
		AliveRays = 0;

		DebugErr = RunProcessHits(GlobalData->ptr(), OptimizedRaysVec.ptr(), OptimizedHitsVec.ptr(), RayEnergy.ptr(), RayEnableStatusBuffer.ptr(), DeviceAliveRaysIndexes.ptr(), AliveRaysIndexes.size(), IsFirstCall, flag, FacesToSkip);
		CheckCudaErr(DebugErr);
		//DEBUG CODE START

// 		OptimizedHitsVec.hostPtr();
// 		RayEnergy.hostPtr();
// 		OptimizedRaysVec.hostPtr();

		//DEBUG CODE END

		//we go to another cycle, if we don't reach end yet
		//so let's count...

		const char* pIsRayAlive = RayEnableStatusBuffer.hostPtr();

		for (int Index = 0; Index < RayEnableStatusBuffer.count(); ++Index)
		{
			if (pIsRayAlive[Index]) ++AliveRays;
		}

		if (IsFirstCall) IsFirstCall = false;
		++Rounds;
	}

	ZeroOutput(InRays.size(), OutHits);

	Buffer <base_color_c> FinalColorBuffer(InRays.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
	DebugErr = cudaMemcpy(FinalColorBuffer.ptr(), OutHits.data(), OutHits.size() * sizeof(base_color_c), cudaMemcpyHostToDevice);
	CheckCudaErr(DebugErr);

	DebugErr = RunFinalizeRays(GlobalData->ptr(), RayEnergy.ptr(), RayBuffer.ptr(), RayBuffer.count(), FinalColorBuffer.ptr(), DeviceAliveRaysIndexes.ptr(), DeviceAliveRaysIndexes.count(), flag, DeviceSurfacePoint2StartLoc.ptr());
	CheckCudaErr(DebugErr);
	//FinalColorBuffer.hostPtr();
	//copy directly back
	DebugErr = cudaMemcpy(OutHits.data(), FinalColorBuffer.ptr(), FinalColorBuffer.count() * sizeof(base_color_c), cudaMemcpyDeviceToHost);
	CheckCudaErr(DebugErr);
}

void xrHardwareLight::PerformAdaptiveHT() {
	Progress(0.0f);
	cudaError_t DebugErr = cudaError_t::cudaSuccess;

	vecVertex& LevelVertexData = inlc_global_data()->g_vertices();

	xr_vector<RayRequest> AdaptiveHTRays;
	AdaptiveHTRays.reserve(LevelVertexData.size());

	int OverallVertexes = LevelVertexData.size();
	for (int VertexIndex = 0; VertexIndex < OverallVertexes; ++VertexIndex)
	{
		Vertex* Vert = LevelVertexData[VertexIndex];
		RayRequest NewRequest{ Vert->P, Vert->N, nullptr };
		AdaptiveHTRays.push_back(NewRequest);
	}
	Progress(0.1f);

	xr_vector<base_color_c> FinalColors;
	PerformRaycast(AdaptiveHTRays, LP_dont_rgb + LP_dont_sun, FinalColors);
	Progress(0.9f);

	for (int VertexIndex = 0; VertexIndex < OverallVertexes; ++VertexIndex)
	{
		Vertex* Vert = LevelVertexData[VertexIndex];
		base_color_c& VertexColor = FinalColors[VertexIndex];

		VertexColor.mul(0.5f);
		Vert->C._set(VertexColor);
	}

	Progress(1.0f);
}

float xrHardwareLight::GetEnergyFromSelectedLight(xr_vector<int>& RGBLightIndexes, xr_vector<int>& SunLightIndexes, xr_vector<int>& HemiLightIndexes) {
	Buffer <int> DeviceRGBLightIndexes(RGBLightIndexes.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
	cudaMemcpy(DeviceRGBLightIndexes.ptr(), RGBLightIndexes.data(), sizeof(int) * RGBLightIndexes.size(), cudaMemcpyHostToDevice);

	Buffer <int> DeviceSunLightIndexes(SunLightIndexes.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
	cudaMemcpy(DeviceSunLightIndexes.ptr(), SunLightIndexes.data(), sizeof(int) * SunLightIndexes.size(), cudaMemcpyHostToDevice);

	Buffer <int> DeviceHemiLightIndexes(HemiLightIndexes.size(), RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
	cudaMemcpy(DeviceHemiLightIndexes.ptr(), HemiLightIndexes.data(), sizeof(int) * HemiLightIndexes.size(), cudaMemcpyHostToDevice);
	//count overall needed rays
	int OverallRaysCount = RGBLightIndexes.size() + SunLightIndexes.size() + HemiLightIndexes.size();

	//and this is the most vulnurable part of all cycle
	Buffer<Hit> HitsVec(OverallRaysCount, RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
	optix::prime::BufferDesc HitDescBuffer = g_pOptix->PrimeContext->createBufferDesc((RTPbufferformat)Hit::format, HitsVec.type(), HitsVec.ptr());
	HitDescBuffer->setRange(0, OverallRaysCount);

	Buffer<Ray> RaysVec(OverallRaysCount, RTP_BUFFER_TYPE_CUDA_LINEAR, LOCKED);
	//and now, i summon my most handfull friend - CUDA, i choose YOU!

	optix::prime::BufferDesc RaysDescBuffer = g_pOptix->PrimeContext->createBufferDesc((RTPbufferformat)Ray::format, RaysVec.type(), RaysVec.ptr());
	RaysDescBuffer->setRange(0, OverallRaysCount);


	return 1.0f;
}

xr_vector<Fvector> xrHardwareLight::GetDebugPCHitData() {
	return g_pOptix->CPURayVertexData;
}

xr_vector<Fvector> xrHardwareLight::GetDebugGPUHitData() {
	return g_pOptix->GPURayVertexData;
}

void xrHardwareLight::GetLevelIndices(vecVertex& InLevelVertices, vecFace& InLevelFaces, xr_vector <PolyIndexes>& OutLevelIndices, xr_vector<HardwareVector>& OutLevelVertexes) {
	//display progress

	OutLevelVertexes.reserve(InLevelVertices.size());
	OutLevelIndices.reserve(InLevelFaces.size() * 3);
	xr_map <Vertex*, int> IndexMap;

	//from 0.1 to 0.3
	int OverallLevelIndexes = InLevelVertices.size();
	for (int VertexIndex = 0; VertexIndex < OverallLevelIndexes; ++VertexIndex)
	{
		Vertex* Vert = InLevelVertices[VertexIndex];

		OutLevelVertexes.push_back(Vert->P);
		IndexMap.insert(std::pair<Vertex*, int>(Vert, VertexIndex));

		float CurrentStageProgress = float(VertexIndex) / float(OverallLevelIndexes);
		Progress(0.1f + (0.2f * CurrentStageProgress));
	}

	//from 0.3 to 0.6
	int OverallFaceIndexes = InLevelFaces.size();

	for (int FaceIndex = 0; FaceIndex < OverallFaceIndexes; ++FaceIndex)
	{
		Face* FaceObj = InLevelFaces[FaceIndex];

		PolyIndexes FaceIndexes;

		auto IndexValue = IndexMap.find(FaceObj->v[0]);
		FaceIndexes.Index1 = IndexValue->second;

		IndexValue = IndexMap.find(FaceObj->v[1]);
		FaceIndexes.Index2 = IndexValue->second;

		IndexValue = IndexMap.find(FaceObj->v[2]);
		FaceIndexes.Index3 = IndexValue->second;

		OutLevelIndices.push_back(FaceIndexes);

		float CurrentStageProgress = float(FaceIndex) / float(OverallFaceIndexes);
		Progress(0.3f + (0.3f * CurrentStageProgress));
	}
}

void xrHardwareLight::CheckCudaError(cudaError_t ErrorCode) {
	if (ErrorCode != cudaError::cudaSuccess)
	{
		clMsg(" [xrHardwareLight]: One of the call return non successful cuda error %d", ErrorCode);
	}
}

bool xrHardwareLight::_IsEnabled;