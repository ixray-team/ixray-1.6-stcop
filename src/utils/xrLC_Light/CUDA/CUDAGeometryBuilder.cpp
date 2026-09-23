#include "stdafx.h"
#include "CUDAGeometryBuilder.h"
#include "../../xrLC/Build.h"
// Scene Global Data
#include "../xrLC_GlobalData.h"
#include "../xrMU_Model.h"
#include "../xrMU_Model_Reference.h"
#include <embree_raytracing/EmbreeRayTrace.h>
#include "global_calculation_data.h"
#include "../../xrForms/CompilersUI.h"
#include "../xrDeflector.h"
#include "../Lightmap.h"

bool OptixGeometryBuilder::BuildBLAS(OptixDeviceContext context, OptixMeshBuffers& outBuffers)
{
    if (vertices.empty() || triangles.empty()) return false;
  
    // 0. Временные буферы для построения
    CUdeviceptr  d_tempBuffer;
    CUdeviceptr  d_tmp_vertexBuffer;
    CUdeviceptr  d_tmp_indexBuffer;
   
    // 1. Загружаем вершины на GPU
    CUDA_CHECK_2(cuMemAlloc(&d_tmp_vertexBuffer, sizeof(Fvector) * vertices.size()));
    CUDA_CHECK_2(cuMemcpyHtoD(d_tmp_vertexBuffer, vertices.data(), sizeof(Fvector) * vertices.size()));


    // 2. Загружаем индексы на GPU
	CUDA_CHECK_2(cuMemAlloc(&d_tmp_indexBuffer, sizeof(CDB::TRI) * triangles.size()));
	CUDA_CHECK_2(cuMemcpyHtoD(d_tmp_indexBuffer, triangles.data(), sizeof(CDB::TRI) * triangles.size()));


    // 3. Настройка входных данных для BLAS
    OptixBuildInput buildInput = {};
    buildInput.type                                 = OPTIX_BUILD_INPUT_TYPE_TRIANGLES;

    buildInput.triangleArray.vertexFormat           = OPTIX_VERTEX_FORMAT_FLOAT3;
    buildInput.triangleArray.vertexStrideInBytes    = sizeof(Fvector);
    buildInput.triangleArray.numVertices            = static_cast<uint32_t>(vertices.size());
    buildInput.triangleArray.vertexBuffers          = &d_tmp_vertexBuffer;

    buildInput.triangleArray.indexFormat            = OPTIX_INDICES_FORMAT_UNSIGNED_INT3;
    buildInput.triangleArray.indexStrideInBytes     = sizeof(CDB::TRI);
    buildInput.triangleArray.numIndexTriplets       = static_cast<uint32_t>(triangles.size());
    buildInput.triangleArray.indexBuffer            = d_tmp_indexBuffer;

    static uint32_t flags                           = OPTIX_GEOMETRY_FLAG_NONE; 
    buildInput.triangleArray.flags = &flags;
    buildInput.triangleArray.numSbtRecords = 1;

    // 4. Настройка параметров сборки
    OptixAccelBuildOptions accelOptions = {};
    accelOptions.operation           = OPTIX_BUILD_OPERATION_BUILD;
    accelOptions.buildFlags          = OPTIX_BUILD_FLAG_PREFER_FAST_TRACE;
    accelOptions.buildFlags         |= OPTIX_BUILD_FLAG_ALLOW_COMPACTION;

    // 5. Вычисление требуемой памяти
    OptixAccelBufferSizes bufferSizes;
    OPTIX_CHECK(optixAccelComputeMemoryUsage(context, &accelOptions, &buildInput, 1, &bufferSizes));
     
    // 6. Выделение памяти
	CUDA_CHECK_2(cuMemAlloc(&d_tempBuffer, bufferSizes.tempSizeInBytes));
	CUDA_CHECK_2(cuMemAlloc(&outBuffers.blasBuffer, bufferSizes.outputSizeInBytes));
     
    // 7. Готовим дескриптор для запроса размера компактации
    OptixAccelEmitDesc emitDesc = {};
    CUdeviceptr d_compactedSize;
	CUDA_CHECK_2(cuMemAlloc(&d_compactedSize, sizeof(uint64_t)));
    emitDesc.type = OPTIX_PROPERTY_TYPE_COMPACTED_SIZE;
    emitDesc.result = d_compactedSize;

    CUstream stream;
	CUDA_CHECK_2(cuStreamCreate(&stream, CU_STREAM_DEFAULT));
    // 8. Сборка BLAS
    OPTIX_CHECK(  optixAccelBuild
    (
        context,
        stream, // CUDA stream
        &accelOptions,
        &buildInput,
        1,
        d_tempBuffer,
        bufferSizes.tempSizeInBytes,
        outBuffers.blasBuffer,
        bufferSizes.outputSizeInBytes,
        &outBuffers.blasHandle,
        &emitDesc, 1
    ));

    CUDA_CHECK_2(cuStreamSynchronize(stream));

    // 9. Узнаём размер скомпактированной структуры
    uint64_t compactedSize = 0;
    CUDA_CHECK_2(cuMemcpyDtoH(&compactedSize, d_compactedSize, sizeof(uint64_t)));
    CUDA_CHECK_2(cuMemFree(d_compactedSize));
    
    // 10. Компактация, если это выгодно
    size_t size_precompact = bufferSizes.outputSizeInBytes;
    if (compactedSize != 0 && compactedSize < bufferSizes.outputSizeInBytes)
    {
        CUdeviceptr d_compactedBuffer;
		CUDA_CHECK_2(cuMemAlloc(&d_compactedBuffer, compactedSize));
        OptixTraversableHandle compactedHandle;
        OPTIX_CHECK(optixAccelCompact(
            context,
            stream, // stream
            outBuffers.blasHandle,
            d_compactedBuffer,
            compactedSize,
            &compactedHandle
        ));

        CUDA_CHECK_2(cuStreamSynchronize(stream));

		// Освобождаем старый буфер
        CUDA_CHECK_2(cuMemFree(outBuffers.blasBuffer));

        // Сохраняем компактный
        outBuffers.blasBuffer = d_compactedBuffer;
        outBuffers.blasHandle = compactedHandle;
    }
 
    // 11. Освобождаем временный буфер
    CUDA_CHECK_2(cuStreamDestroy(stream));
    
    CUDA_CHECK_2(cuMemFree(d_tempBuffer));
	CUDA_CHECK_2(cuMemFree(d_tmp_vertexBuffer));
	CUDA_CHECK_2(cuMemFree(d_tmp_indexBuffer));
      
    return true;
}

bool OptixGeometryBuilder::BuildTLAS(OptixDeviceContext context, OptixMeshBuffers& outScene)
{
    if (outScene.blasHandle == 0) {
        Msg("! ERROR: Invalid BLAS handle");
        return false;
    }

    // 1. Строим TLAS (один экземпляр BLAS)
    OptixInstance instance = {};
    float transform[12] = {
        1.0f, 0.0f, 0.0f, 0.0f,
        0.0f, 1.0f, 0.0f, 0.0f,
        0.0f, 0.0f, 1.0f, 0.0f
    };

    memcpy(instance.transform, transform, sizeof(transform));
    instance.instanceId = 0;
    instance.sbtOffset = 0;
    instance.visibilityMask = 255;
    instance.flags = OPTIX_INSTANCE_FLAG_NONE;
    instance.traversableHandle = outScene.blasHandle;

    // 2. Алокация под GPU
    CUdeviceptr d_instances;
	CUDA_CHECK_2(cuMemAlloc(&d_instances, sizeof(OptixInstance)));
	CUDA_CHECK_2(cuMemcpyHtoD(d_instances, &instance, sizeof(OptixInstance)));

    // 3. Входные данные для структуры 
    OptixBuildInput buildInput = {};
    buildInput.type = OPTIX_BUILD_INPUT_TYPE_INSTANCES;
    buildInput.instanceArray.instances = d_instances;
    buildInput.instanceArray.numInstances = 1;


    // 4. Настройка параметров сборки
    OptixAccelBuildOptions buildOptions = {};
    buildOptions.buildFlags = OPTIX_BUILD_FLAG_PREFER_FAST_TRACE; // OPTIX_BUILD_FLAG_PREFER_FAST_TRACE
    buildOptions.operation  = OPTIX_BUILD_OPERATION_BUILD;

    // 5. Вычисление требуемой памяти
    OptixAccelBufferSizes bufferSizes;
    OPTIX_CHECK(optixAccelComputeMemoryUsage(context, &buildOptions, &buildInput, 1, &bufferSizes));

    // 6. Выделение памяти
    CUdeviceptr d_tempBuffer;
	CUDA_CHECK_2(cuMemAlloc(&d_tempBuffer, bufferSizes.tempSizeInBytes));
	CUDA_CHECK_2(cuMemAlloc(&outScene.tlasBuffer, bufferSizes.outputSizeInBytes));
 
    CUstream stream;
	CUDA_CHECK_2(cuStreamCreate(&stream, CU_STREAM_DEFAULT));

    OPTIX_CHECK(optixAccelBuild(
        context,
        stream,
        &buildOptions,
        &buildInput,
        1,
        d_tempBuffer,
        bufferSizes.tempSizeInBytes,
        outScene.tlasBuffer,
        bufferSizes.outputSizeInBytes,
        &outScene.tlasHandle,
        nullptr, 0
     ));

    CUDA_CHECK_2(cuStreamSynchronize(stream));
	CUDA_CHECK_2(cuStreamDestroy(stream));

    CUDA_CHECK_2(cuMemFree(d_tempBuffer));
	CUDA_CHECK_2(cuMemFree(d_instances));

    return true;
}
  
struct FaceDataEmbree;
bool XRay::RayTrace::CUDA::BuildSceneFromLCGlobalData(OptixDeviceContext Context, OptixMeshBuffers& OutScene)
{
    OptixGeometryBuilder GeometryBuilder;
    size_t StartMemory = GetHeapMemory();

    if (gCompilerMode.builder_type == LCBuildingType::eLC)
    {
        xrLC_GlobalData* GlobalData = lc_global_data();
        if (!GlobalData)
        {
            return false;
        }

        // 1. Обрабатываем статическую геометрию
        for (Face* FacePtr : GlobalData->g_faces())
        {
            const Shader_xrLC& FaceShader = FacePtr->Shader();
            if (!FaceShader.flags.bLIGHT_CastShadow)
            {
                continue;
            }

            u16 SurfaceID = GlobalData->materials()[FacePtr->dwMaterial].surfidx;
            b_texture& Texture = GlobalData->textures()[SurfaceID];

            bool IsTransparent = (!Texture.pSurface.Empty() && Texture.bHasAlpha);
            FacePtr->flags.bOpaque = !IsTransparent;
            GeometryBuilder.AddFace(FacePtr, FacePtr->v[0]->P, FacePtr->v[1]->P, FacePtr->v[2]->P, 1);
        }

        // 2. Обрабатываем MU-референсы
        xr_vector<FaceDataEmbree> TempBuffer;
        for (auto Ref : GlobalData->mu_refs())
        {
            TempBuffer.clear();
            Ref->export_cform_rcast_new(TempBuffer);

            for (auto& PackedFace : TempBuffer)
            {
                Face* FacePtr = (Face*)PackedFace.ptr;
                b_material& Material = GlobalData->materials()[FacePtr->dwMaterial];
                b_texture& Texture = GlobalData->textures()[Material.surfidx];

                bool IsTransparent = (!Texture.pSurface.Empty() && Texture.bHasAlpha);
                FacePtr->flags.bOpaque = IsTransparent;
                GeometryBuilder.AddFace(FacePtr, PackedFace.v1, PackedFace.v2, PackedFace.v3, 0, Ref);
            }
        }
        TempBuffer.clear();
        TempBuffer.shrink_to_fit();
    }
    else if (gCompilerMode.builder_type == LCBuildingType::eDO)
    {
       auto GlobalData = &gl_data;
       if (!GlobalData)
       {
           return false;
       }
       
        // 1. Обрабатываем статическую геометрию
       for (auto& FaceData : GlobalData->building_embree_faces)
       {
           u16 SurfaceID = GlobalData->g_materials[FaceData.dwMaterial].surfidx;
           b_texture& Texture = GlobalData->g_textures[SurfaceID];
       
           bool IsTransparent = (!Texture.pSurface.Empty() && Texture.bHasAlpha);
           FaceData.bOpaque = !IsTransparent;
           GeometryBuilder.AddFace(&FaceData, FaceData.v1, FaceData.v2, FaceData.v3, 0);
       }
    }
 
    GeometryBuilder.RemoveDublicates();  
    GeometryBuilder.RemoveDublicateFaces();
    Msg("*[GPU Accel Structure] Collected Structure Faces Memory: %u mb", u32( (GetHeapMemory() - StartMemory) / 1024 / 1024));
  
    StartMemory = GetHeapMemory();

    // 3. Строим BLAS
    if (!GeometryBuilder.BuildBLAS(Context, OutScene))
    {
        return false;
    }
  
     // 4. Строим TLAS
    if (!GeometryBuilder.BuildTLAS(Context, OutScene))
    {
        return false;
    }
    Msg("*[GPU Accel Structure] Cpu (GPU Used) Memory: %u mb", u32( (GetHeapMemory() - StartMemory) / 1024 / 1024));

    Fbox SceneBox;
    SceneBox.invalidate();
    for (const Fvector& Vertex : GeometryBuilder.vertices)
    {
        SceneBox.modify(Vertex);
    }

    xr_vector<u32> PreviewIndices;
    PreviewIndices.reserve(GeometryBuilder.triangles.size() * 3);
    for (const CDB::TRI& Triangle : GeometryBuilder.triangles)
    {
        PreviewIndices.push_back(Triangle.verts[0]);
        PreviewIndices.push_back(Triangle.verts[1]);
        PreviewIndices.push_back(Triangle.verts[2]);
    }
    Fvector PreviewCenter;
    Fvector PreviewSize;
    SceneBox.getcenter(PreviewCenter);
    SceneBox.getsize(PreviewSize);
    PublishLightPreviewScene(GeometryBuilder.vertices, PreviewIndices, PreviewCenter, PreviewSize.magnitude() * 0.5f);
    XRay::RayTrace::CUDA::RememberPreviewFaces(GeometryBuilder.facePointers, GeometryBuilder.FaceKinds, GeometryBuilder.FaceExtras);

    XRay::RayTrace::CUDA::InitializeFaces(GeometryBuilder.facePointers);

    GeometryBuilder.Clear();
    GeometryBuilder.MemoryDealoc();
    return true;
}

static xr_vector<Face*> PreviewLevelFaces;
static xr_vector<xrMU_Reference*> PreviewMuRefs;
static xr_vector<_face*> PreviewMuFaces;
static xr_vector<u8> PreviewCornerRgb;
static xr_vector<float> PreviewUv;
static xr_vector<u32> PreviewLayerId;

static void EncodeBaked(const base_color& Src, u8* Dst)
{
	base_color_c Color;
	Src._get(Color);
	Dst[0] = u8_clr(Color.rgb.x + Color.sun + Color.hemi * 0.45f);
	Dst[1] = u8_clr(Color.rgb.y + Color.sun * 0.96f + Color.hemi * 0.50f);
	Dst[2] = u8_clr(Color.rgb.z + Color.sun * 0.82f + Color.hemi * 0.55f);
}

// Same addressing MergeLM blit uses: surface is (W+2B)*(H+2B), width/height are logical.
static LightPreviewMap CopyDeflectorLayer(const lm_layer& Layer)
{
	LightPreviewMap Map;
	if (Layer.surface.empty())
	{
		return Map;
	}

	const u32 Border = (u32)std::max(gCompilerMode.LC_BORDER, 0);
	const u32 Count = (u32)Layer.surface.size();

	if (Layer.width == 0 && Layer.height == 0)
	{
		Map.Width = 1;
		Map.Height = 1;
		Map.Rgb.resize(3);
		EncodeBaked(Layer.surface[Count / 2], Map.Rgb.data());
		return Map;
	}

	const u32 LogicalW = Layer.width ? Layer.width : 1;
	const u32 LogicalH = Layer.height ? Layer.height : 1;
	const u32 RealW = Layer.width + 2 * Border;
	const u32 RealH = Layer.height + 2 * Border;
	const bool Padded = (Border > 0 && Count == RealW * RealH);
	const u32 Stride = Padded ? RealW : LogicalW;
	const u32 OffsetX = Padded ? Border : 0;
	const u32 OffsetY = Padded ? Border : 0;
	if (!Padded && Count < LogicalW * LogicalH)
	{
		return Map;
	}

	Map.Width = LogicalW;
	Map.Height = LogicalH;
	Map.Rgb.assign((size_t)LogicalW * LogicalH * 3, 0);
	for (u32 Y = 0; Y < LogicalH; ++Y)
	{
		for (u32 X = 0; X < LogicalW; ++X)
		{
			const u32 SurfaceIndex = (Y + OffsetY) * Stride + (X + OffsetX);
			if (SurfaceIndex >= Count)
			{
				continue;
			}
			EncodeBaked(Layer.surface[SurfaceIndex], &Map.Rgb[((size_t)Y * LogicalW + X) * 3]);
		}
	}
	return Map;
}

void XRay::RayTrace::CUDA::RememberPreviewFaces(const xr_vector<void*>& Faces, const xr_vector<u8>& Kinds, const xr_vector<void*>& Extras)
{
	PreviewLevelFaces.assign(Faces.size(), nullptr);
	PreviewMuRefs.assign(Faces.size(), nullptr);
	PreviewMuFaces.assign(Faces.size(), nullptr);
	for (size_t Index = 0; Index < Faces.size(); ++Index)
	{
		const u8 Kind = (Index < Kinds.size()) ? Kinds[Index] : u8(0);
		if (Kind == 1)
		{
			PreviewLevelFaces[Index] = (Face*)Faces[Index];
		}
		else if (Kind == 0 && Index < Extras.size() && Extras[Index])
		{
			PreviewMuFaces[Index] = (_face*)Faces[Index];
			PreviewMuRefs[Index] = (xrMU_Reference*)Extras[Index];
		}
	}
	PreviewCornerRgb.assign(Faces.size() * 9, 196);
	PreviewUv.assign(Faces.size() * 6, 0.f);
	PreviewLayerId.assign(Faces.size(), u32(-1));
	PublishLightPreviewColors(PreviewCornerRgb);
}

void XRay::RayTrace::CUDA::CapturePreviewBakedColors(bool IncludeVertex)
{
	if (PreviewLevelFaces.empty())
	{
		return;
	}
	if (PreviewCornerRgb.size() != PreviewLevelFaces.size() * 9)
	{
		PreviewCornerRgb.assign(PreviewLevelFaces.size() * 9, 196);
	}

	if (IncludeVertex)
	{
		return;
	}

	xr_hash_map<Face*, u32> FaceToTri;
	FaceToTri.reserve(PreviewLevelFaces.size());
	for (size_t Index = 0; Index < PreviewLevelFaces.size(); ++Index)
	{
		if (PreviewLevelFaces[Index])
		{
			FaceToTri[PreviewLevelFaces[Index]] = (u32)Index;
		}
	}

	PreviewUv.assign(PreviewLevelFaces.size() * 6, 0.f);
	PreviewLayerId.assign(PreviewLevelFaces.size(), u32(-1));

	xr_vector<LightPreviewMap> Maps;
	u32 Mapped = 0;
	for (CDeflector* Deflector : lc_global_data()->g_deflectors())
	{
		if (!Deflector || Deflector->layer.surface.empty())
		{
			continue;
		}
		LightPreviewMap Map = CopyDeflectorLayer(Deflector->layer);
		if (Map.Rgb.empty())
		{
			continue;
		}
		const u32 Layer = (u32)Maps.size();
		Maps.push_back(std::move(Map));
		for (const UVtri& Poly : Deflector->UVpolys)
		{
			auto It = FaceToTri.find(Poly.owner);
			if (It == FaceToTri.end())
			{
				continue;
			}
			const u32 Triangle = It->second;
			PreviewLayerId[Triangle] = Layer;
			float* Uv = &PreviewUv[Triangle * 6];
			for (int Corner = 0; Corner < 3; ++Corner)
			{
				Uv[Corner * 2 + 0] = Poly.uv[Corner].x;
				Uv[Corner * 2 + 1] = Poly.uv[Corner].y;
			}
			++Mapped;
		}
	}

	PublishLightPreviewMaps(PreviewUv, PreviewLayerId, Maps);
	Msg("[Preview] CUDA LMaps: %u triangles, %u with UV, %u layers", (u32)PreviewLevelFaces.size(), Mapped, (u32)Maps.size());
}

void XRay::RayTrace::CUDA::CapturePreviewMU()
{
	if (PreviewMuRefs.empty() || PreviewCornerRgb.size() < PreviewMuRefs.size() * 9)
	{
		return;
	}

	xr_hash_map<_vertex*, u32> VertIndex;
	xrMU_Model* CachedModel = nullptr;
	u32 Painted = 0;

	for (size_t Index = 0; Index < PreviewMuRefs.size(); ++Index)
	{
		xrMU_Reference* Ref = PreviewMuRefs[Index];
		_face* FacePtr = (Index < PreviewMuFaces.size()) ? PreviewMuFaces[Index] : nullptr;
		if (!Ref || !FacePtr || !Ref->model || Ref->color.empty())
		{
			continue;
		}
		if (Index < PreviewLayerId.size() && PreviewLayerId[Index] != u32(-1))
		{
			continue;
		}

		if (Ref->model != CachedModel)
		{
			VertIndex.clear();
			VertIndex.reserve(Ref->model->m_vertices.size());
			for (u32 VertexIndex = 0; VertexIndex < Ref->model->m_vertices.size(); ++VertexIndex)
			{
				VertIndex[Ref->model->m_vertices[VertexIndex]] = VertexIndex;
			}
			CachedModel = Ref->model;
		}

		u8 Tmp[9];
		bool Ok = true;
		for (int Corner = 0; Corner < 3; ++Corner)
		{
			auto It = VertIndex.find(FacePtr->v[Corner]);
			if (It == VertIndex.end() || It->second >= Ref->color.size())
			{
				Ok = false;
				break;
			}
			EncodeBaked(Ref->color[It->second], Tmp + Corner * 3);
		}
		if (!Ok)
		{
			continue;
		}

		u8* Dst = &PreviewCornerRgb[Index * 9];
		for (int Channel = 0; Channel < 9; ++Channel)
		{
			Dst[Channel] = Tmp[Channel];
		}
		++Painted;
	}

	PublishLightPreviewColors(PreviewCornerRgb);
	Msg("[Preview] CUDA MU: %u triangles painted", Painted);
}
