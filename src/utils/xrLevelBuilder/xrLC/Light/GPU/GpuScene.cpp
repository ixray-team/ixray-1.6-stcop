#include "stdafx.h"
#include "GpuGeometryBuilder.h"
#include "GpuRayTracer.h"
#include "../../Build/Build.h"
// Scene Global Data
#include "../../Build/xrLC_GlobalData.h"
#include "../../MUModels/xrMU_Model.h"
#include "../../MUModels/xrMU_Model_Reference.h"
#include "../Embree/EmbreeRayTrace.h"
#include "../../DetailLight/global_calculation_data.h"
#include "../../../UI/CompilersUI.h"
#include "../xrDeflector.h"
#include "../Lightmap.h"

static void RememberPreviewFaces(const xr_vector<void*>& Faces, const xr_vector<u8>& Kinds, const xr_vector<void*>& Extras);

static void CollectLights(GpuScene& Scene)
{
	auto& Lights = gCompilerMode.builder_type == LCBuildingType::eLC ? lc_global_data()->L_static() : gl_data.g_lights;

	auto Add = [&Scene](R_Light& L, u16 Type)
	{
		GpuLight& Light		= Scene.Lights.emplace_back();
		Light.Type			= L.type;
		Light.LightType		= Type;
		Light.Diffuse.set	(L.diffuse.x, L.diffuse.y, L.diffuse.z);
		Light.Position		= L.position;
		Light.Direction		= L.direction;
		Light.Range			= L.range;
		Light.Range2		= L.range2;
		Light.Falloff		= L.falloff;
		Light.Attenuation0	= L.attenuation0;
		Light.Attenuation1	= L.attenuation1;
		Light.Attenuation2	= L.attenuation2;
		Light.Energy		= L.energy;
	};

	Scene.Lights.reserve(Lights.rgb.size() + Lights.hemi.size() + Lights.sun.size());
	for (auto& HEMI : Lights.hemi)
		Add(HEMI, GpuLightKind::Hemi);
	for (auto& SUN : Lights.sun)
		Add(SUN, GpuLightKind::Sun);
	for (auto& RGB : Lights.rgb)
		Add(RGB, GpuLightKind::RGB);

	Msg("[GPU] Loaded Lighting: %u size", (u32)Scene.Lights.size());
}

static void CollectFaces(GpuScene& Scene, const xr_vector<void*>& Faces)
{
	Scene.Faces.resize(Faces.size());

	bool isLC = gCompilerMode.builder_type == LCBuildingType::eLC;
	for (size_t IndexFace = 0; IndexFace < Faces.size(); IndexFace++)
	{
		void* Fv = Faces[IndexFace];
		GpuFace& FaceGPU = Scene.Faces[IndexFace];
		Fvector2* TC = nullptr;

		if (isLC)
		{
			Face* F				= (Face*)Fv;
			FaceGPU.SurfaceID	= lc_global_data()->materials()[F->dwMaterial].surfidx;
			FaceGPU.Opaque		= F->flags.bOpaque;
			TC					= F->getTC0();
		}
		else
		{
			FaceDataEmbree* F	= (FaceDataEmbree*)Fv;
			FaceGPU.SurfaceID	= gl_data.g_materials[F->dwMaterial].surfidx;
			FaceGPU.Opaque		= F->bOpaque;
			TC					= F->getTC0();
		}

		FaceGPU.TC[0] = TC[0];
		FaceGPU.TC[1] = TC[1];
		FaceGPU.TC[2] = TC[2];
	}
}

static void CollectTexturesAlpha(GpuScene& Scene)
{
	auto& glTextures = gCompilerMode.builder_type == LCBuildingType::eLC ? lc_global_data()->textures() : gl_data.g_textures;

	size_t Allocated = 0;
	Scene.Textures.resize(glTextures.size());
	for (size_t Index = 0; Index < glTextures.size(); Index++)
	{
		auto& T = glTextures[Index];
		GpuTexture& Data = Scene.Textures[Index];
		Data.Width  = T.dwWidth;
		Data.Height = T.dwHeight;

		if (T.pSurface.Empty() || !T.bHasAlpha)
		{
			continue;
		}

		Data.Alpha.resize(T.dwWidth * T.dwHeight);
		const uint32_t* raw = static_cast<const uint32_t*>(*T.pSurface);
		for (u32 i = 0; i < T.dwWidth * T.dwHeight; i++)
		{
			Data.Alpha[i] = (raw[i] >> 24) & 0xFF;
		}
		Allocated += Data.Alpha.size();
	}

	Msg("[GPU] Textures[%u] Alpha: %llu mb", (u32)Scene.Textures.size(), (u64)(Allocated / 1024 / 1024));
}

struct FaceDataEmbree;
void XRay::RayTrace::GPU::BuildScene(GpuScene& Scene)
{
    GpuGeometryBuilder GeometryBuilder;
    size_t StartMemory = GetHeapMemory();

    if (gCompilerMode.builder_type == LCBuildingType::eLC)
    {
        xrLC_GlobalData* GlobalData = lc_global_data();
        if (!GlobalData)
        {
            return;
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
    RememberPreviewFaces(GeometryBuilder.facePointers, GeometryBuilder.FaceKinds, GeometryBuilder.FaceExtras);

    Scene.Vertices.swap(GeometryBuilder.vertices);
    Scene.Indices.reserve(GeometryBuilder.triangles.size() * 3);
    for (const CDB::TRI& Triangle : GeometryBuilder.triangles)
    {
        Scene.Indices.push_back(Triangle.verts[0]);
        Scene.Indices.push_back(Triangle.verts[1]);
        Scene.Indices.push_back(Triangle.verts[2]);
    }

    CollectFaces(Scene, GeometryBuilder.facePointers);
    CollectLights(Scene);
    CollectTexturesAlpha(Scene);

    GeometryBuilder.Clear();
    GeometryBuilder.MemoryDealoc();
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

static void RememberPreviewFaces(const xr_vector<void*>& Faces, const xr_vector<u8>& Kinds, const xr_vector<void*>& Extras)
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

void XRay::RayTrace::GPU::ForgetPreviewFaces()
{
	// Face*/xrMU_Reference* are owned by the level and die with CBuild
	PreviewLevelFaces.clear();
	PreviewMuRefs.clear();
	PreviewMuFaces.clear();
	PreviewCornerRgb.clear();
	PreviewUv.clear();
	PreviewLayerId.clear();
}

void XRay::RayTrace::GPU::CapturePreviewBakedColors(bool IncludeVertex)
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
	Msg("[Preview] GPU LMaps: %u triangles, %u with UV, %u layers", (u32)PreviewLevelFaces.size(), Mapped, (u32)Maps.size());
}

void XRay::RayTrace::GPU::CapturePreviewMU()
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
	Msg("[Preview] GPU MU: %u triangles painted", Painted);
}
