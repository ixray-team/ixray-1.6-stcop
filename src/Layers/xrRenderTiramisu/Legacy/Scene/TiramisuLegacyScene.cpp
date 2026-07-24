#include "TiramisuLegacyScene.h"

#include "TiramisuLegacySceneRenderProxy.h"
#include "TiramisuLegacySceneSector.h"
#include "Legacy/Visual/XRayModelPool.h"
#include "Resources/Materials/TiramisuRenderMaterialsManager.h"
#include "Scene/TiramisuRenderScene.h"
#include "Scene/SceneProxy/TiramisuStaticMeshSceneProxy.h"
#include "src/xrCore/stream_reader.h"
#include "src/xrEngine/IGame_Level.h"
#include "src/xrEngine/IGame_Persistent.h"
#include "src/xrEngine/xrLevel.h"

TiramisuLegacyScene::~TiramisuLegacyScene()
{
	CheckIsGameThread();
	Clear();
}

void TiramisuLegacyScene::LoadLevel(IReader* FileReader)
{
	CheckIsGameThread();
	Clear();
	
    R_ASSERT(g_pGamePersistent);
    IReader* ShadersChunk;
    {
        ShadersChunk = FileReader->open_chunk(fsL_SHADERS);
        R_ASSERT2(ShadersChunk, "Level doesn't builded correctly.");
        u32 Count = ShadersChunk->r_u32();
        for (u32 i = 0; i < Count; i++)
        {
            string512 ShaderName, TextureList;
            const char*	ShaderNameAndTexture = static_cast<const char*>(ShadersChunk->pointer());
        	
        	ShadersChunk->skip_stringZ();
        	
            if (!ShaderNameAndTexture[0])
            {
            	Shaders.push_back(GRenderResourcesManager->MaterialsManager->Copy(GRenderResourcesManager->DefaultMaterial));
	            continue;
            }
        	
            xr_strcpy(ShaderName, ShaderNameAndTexture);
        	
            //TODO: delim)
        	if (char* delim = strchr(ShaderName, '/'))
            {
            	*delim = 0;
            	xr_strcpy(TextureList, delim + 1);
            }
        	xr_vector<shared_str> TextureNames;
            {
            	auto LambdaFixTextureName = [](const char* FileName)
            	{
            		char* Ext = strext(FileName);
            		if (Ext &&
						(0 == stricmp(Ext, ".tga") ||
							0 == stricmp(Ext, ".dds") ||
							0 == stricmp(Ext, ".bmp") ||
							0 == stricmp(Ext, ".ogm")))
            			*Ext = 0;
            	};
            	
            	if (TextureList[0])
            	{
            		char* P = (char*)TextureList;
            		svector<char, 128> N;
            		while (*P)
            		{
            			if (*P == ',')
            			{
            				// flush
            				N.push_back(0);
            				strlwr(N.begin());

            				LambdaFixTextureName(N.begin());
            				TextureNames.push_back(N.begin());
            				N.clear();
            			}
            			else
            			{
            				N.push_back(*P);
            			}
            			P++;
            		}
            		if (N.size())
            		{
            			// flush
            			N.push_back(0);
            			strlwr(N.begin());

            			LambdaFixTextureName(N.begin());
            			TextureNames.push_back(N.begin());
            		}
            	}
            }

        	shared_str MaterialName;
        	MaterialName.printf("level\\%d", i);
            TiramisuRenderMaterialInstanceDynamic* Material =
                GRenderResourcesManager->MaterialsManager->CreateLegacyInstanceDynamic(
                    MaterialName, ShaderName, TextureNames);
            Shaders.push_back(Material);
            Msg("Level shader:%s", ShaderName);
        }
        ShadersChunk->close();
    }
    {
        g_pGamePersistent->LoadTitle("st_loading_geometry");
        g_pGamePersistent->LoadTitle();
        CStreamReader* geom = FS.rs_open("$level$", "level.geom");
        R_ASSERT2(geom, "level.geom");
        LoadBuffers(geom);
        LoadSWIs(geom);
        FS.r_close(geom);

        // Visuals
        g_pGamePersistent->LoadTitle("st_loading_spatial_db");
        g_pGamePersistent->LoadTitle();
        ShadersChunk = FileReader->open_chunk(fsL_VISUALS);
        LoadVisuals(ShadersChunk);
        ShadersChunk->close();
    }
    LoadSectors(FileReader);

    // HOM.Load();
    // GRenderTarget->LoadLevel();
	
	SceneRenderProxy = new TiramisuLegacySceneRenderProxy;
	SceneRenderProxy->RenderData = StaticMeshRenderData; 
	GRenderResourcesManager->RenderScene->AddRenderSceneProxy(SceneRenderProxy);
}

void TiramisuLegacyScene::Clear()
{
	CheckIsGameThread();
	for (size_t i = 0; i < Visuals.size(); i++)
	{
		Visuals[i]->Release();
		xr_delete(Visuals[i]);
	}
	
	Visuals.clear();
	
	Portals.clear();
	Sectors.clear();
	for (TiramisuRenderMaterialInterface* Shader:Shaders)
	{
		GRenderResourcesManager->MaterialsManager->Free(Shader);
	}
	Shaders.clear();
	
	xr_delete(PortalsCollisionModel);
		
	RemoveRenderSceneProxy(SceneRenderProxy);
	
	ENQUEUE_RENDER_COMMAND(TiramisuLegacyScene::Clear)([InStaticMeshRenderData = StaticMeshRenderData]
	{
		CheckIsRenderThread();
		delete InStaticMeshRenderData;
	});
	
	StaticMeshRenderData = nullptr;
	LastSector = nullptr;
}



TiramisuLegacySceneSector* TiramisuLegacyScene::GetSectorByRay(const Fvector& Position, const Fvector& Direction)
{
	CheckIsGameThread();
	enum class ESectorHitSource
	{
		Portal,
		Geometry
	};

	int PortalId = -1;
	float PortalRange = 500.f;
	if (PortalsCollisionModel)
	{
		PortalsCollider.ray_query(PortalsCollisionModel, Position, Direction, PortalRange);
		if (PortalsCollider.r_count())
		{
			const CDB::RESULT* PortalResult = &(*PortalsCollider.r_begin());
			PortalId = PortalResult->id;
			PortalRange = PortalResult->range;
		}
	}

	int GeometryId = -1;
	float GeometryRange = PortalRange;
	PortalsCollider.ray_query(g_pGameLevel->ObjectSpace.GetStaticModel(), Position, Direction, GeometryRange);
	if (PortalsCollider.r_count())
	{
		const CDB::RESULT* GeometryResult = &(*PortalsCollider.r_begin());
		GeometryId = GeometryResult->id;
		GeometryRange = GeometryResult->range;
	}

	int SectorId = -1;
	ESectorHitSource HitSource = ESectorHitSource::Geometry;
	if (PortalId >= 0)
	{
		if (GeometryId >= 0 && GeometryRange + EPS < PortalRange)
		{
			SectorId = GeometryId;
			HitSource = ESectorHitSource::Geometry;
		}
		else
		{
			SectorId = PortalId;
			HitSource = ESectorHitSource::Portal;
		}
	}
	else if (GeometryId >= 0)
	{
		SectorId = GeometryId;
		HitSource = ESectorHitSource::Geometry;
	}
	else
	{
		return nullptr;
	}

	if (HitSource == ESectorHitSource::Portal)
	{
		const CDB::TRI& PortalTriangle = PortalsCollisionModel->get_tris()[SectorId];
		return Portals[PortalTriangle.dummy]->getSectorFacing(Position);
	}

	const CDB::TRI& GeometryTriangle = g_pGameLevel->ObjectSpace.GetStaticTris()[SectorId];
	return Sectors[GeometryTriangle.sector].get();
}

TiramisuLegacySceneSector* TiramisuLegacyScene::GetSector(Fvector Position)
{
	CheckIsGameThread();
	TiramisuLegacySceneSector* Result = nullptr;
	Fvector dir;
	PortalsCollider.ray_options(CDB::OPT_ONLYNEAREST);

	dir.set(0, -1, 0);
	Result = GetSectorByRay(Position, dir);
	if (!Result)
	{
		dir.set(0, 1, 0);
		Result = GetSectorByRay(Position, dir);
	}
	return Result;
}



void TiramisuLegacyScene::Calculate()
{
	CheckIsGameThread();
	RenderGraph.Clear();
	if (Sectors.empty() || !g_pGameLevel)
	{
		// В editor viewport новая сцена может существовать без загруженного
		// legacy level. В этом состоянии portal/collision traversal не нужен.
		LastSector = nullptr;
		return;
	}

	// SSA (Screen-Space Area) показывает, какую площадь объект занимает на экране:
	// чем объект больше и ближе к камере, тем выше SSA. Пороги ниже используются
	// для отсечения слишком маленьких порталов/секторов и выбора LOD геометрии.
	constexpr float GlodSsaStart = 256.f; // Верхний порог экранной площади для перехода LOD геометрии.
	constexpr float GlodSsaEnd = 64.f; // Нижний порог экранной площади для перехода LOD геометрии.
	constexpr float LodBias = 0.75f; // Смещение screen factor, регулирует агрессивность выбора LOD.
	constexpr float SsaDiscard = 3.5f; // Минимальный экранный размер, ниже которого портал/сектор отбрасывается.

	constexpr float PortalFadeSsaStart = 48.f; // SSA ниже этого порога включает fade портала.
	constexpr float PortalFadeSsaEnd = 32.f; // SSA ниже этого порога полностью отсекает портал.
	
	CFrustum ViewBase;
	const float FovFactor = _sqr(90.f / DevicePtr->fFOV); // Нормализует SSA относительно опорного FOV в 90 градусов.
	const float ScreenFactor = static_cast<float>(DevicePtr->Width * DevicePtr->Height) * FovFactor * (EPS_S + LodBias);
	const float InvScreenFactor = 1.f / ScreenFactor; // Переводит пиксельные пороги в нормализованные значения SSA.
	SsaDiscardThreshold = _sqr(SsaDiscard) * InvScreenFactor;
	
	GlodSsaStartThreshold = _sqr(GlodSsaStart / 3.f) * InvScreenFactor;
	GlodSsaEndThreshold = _sqr(GlodSsaEnd / 3.f) * InvScreenFactor;
	RenderGraph.SetSsaThresholds(SsaDiscardThreshold, GlodSsaStartThreshold, GlodSsaEndThreshold);
	
	PortalFadeSsaStartThreshold = _sqr(PortalFadeSsaStart / 3.f) * InvScreenFactor;
	PortalFadeSsaEndThreshold = _sqr(PortalFadeSsaEnd / 3.f) * InvScreenFactor;
	
	ViewBase.CreateFromMatrix(DevicePtr->mFullTransform, FRUSTUM_P_LRTB | FRUSTUM_P_FAR);
	{
		TiramisuLegacySceneSector* NewCurrentSector = GetSector(DevicePtr->vCameraPosition);
		if (NewCurrentSector && (NewCurrentSector != LastSector))
			g_pGamePersistent->OnSectorChanged(0);

		if (NewCurrentSector)
		{
			LastSector = NewCurrentSector;
		}
	}

	if (PortalsCollisionModel)
	{
		Fvector BoxRaidus;		
		BoxRaidus.set(EPS_L * 2, EPS_L * 2, EPS_L * 2);
		PortalsCollider.box_options(CDB::OPT_FULL_TEST);
		PortalsCollider.box_query(PortalsCollisionModel, DevicePtr->vCameraPosition, BoxRaidus);
		for (s32 i = 0; i < PortalsCollider.r_count(); i++)
		{
			Portals[PortalsCollisionModel->get_tris()[PortalsCollider.r_begin()[i].id].dummy]->bDualRender = true;
		}
	}
	//

	if (LastSector)
	{
		// Traverse sector/portal structure
		GPortalTraverser.traverse
		(
			LastSector,
			ViewBase,
			DevicePtr->vCameraPosition,
			DevicePtr->mFullTransform,
			CPortalTraverser::VQ_HOM + CPortalTraverser::VQ_SSA + CPortalTraverser::VQ_FADE
		);

		// Determine visibility for static geometry hierrarhy
		if (psDeviceFlags.test(rsDrawStatic))
		{
			for (u32 s_it = 0; s_it < GPortalTraverser.r_sectors.size(); s_it++)
			{
				TiramisuLegacySceneSector* Sector = GPortalTraverser.r_sectors[s_it];
				CDS0_RenderVisual* RootVisual = Sector->root();
				RenderGraph.ClearViews();
				for (u32 v_it = 0; v_it < Sector->r_frustums.size(); v_it++)
				{
					RenderGraph.PushView(&(Sector->r_frustums[v_it]));
				}
				RenderGraph.AddStatic(RootVisual);
			}
		}
	}
	
	xr_vector<FMeshBatch> MeshBatches;
	for (const auto &RenderItem:RenderGraph.RenderList)
	{
		FMeshBatch& OutMeshBatch = MeshBatches.emplace_back();
		OutMeshBatch.VertexBuffer.Count = RenderItem.SceneVertexBuffer.Count;
		OutMeshBatch.VertexBuffer.Offset = RenderItem.SceneVertexBuffer.Offset;
		OutMeshBatch.VertexBuffer.Size = RenderItem.SceneVertexBuffer.Size;
		OutMeshBatch.VertexBuffer.Stride = RenderItem.SceneVertexBuffer.Stride;
		OutMeshBatch.VertexType = RenderItem.SceneVertexBuffer.VertexType;

		OutMeshBatch.IndexBuffer.Count = RenderItem.SceneIndexBuffer.Count;
		OutMeshBatch.IndexBuffer.Offset = RenderItem.SceneIndexBuffer.Offset;
		OutMeshBatch.IndexBuffer.Size = RenderItem.SceneIndexBuffer.Size;
		OutMeshBatch.IndexBuffer.IndexType = nri::IndexType::UINT16;
		OutMeshBatch.Material = RenderItem.Material->MaterialRenderProxy;

		FMeshBatchElement& Element = OutMeshBatch.Elements.emplace_back();
		Element.CountIndex = RenderItem.CountIndex;
		Element.CountVertex = RenderItem.CountVertex;
		Element.OffsetIndex = RenderItem.OffsetIndex;
		Element.OffsetVertex = RenderItem.OffsetVertex;
		
	}
	ENQUEUE_RENDER_COMMAND(TiramisuLegacyScene::Calculate)([InSceneRenderProxy = SceneRenderProxy, InMeshBatches = std::move(MeshBatches)]
	{
		CheckIsRenderThread();
		InSceneRenderProxy->MeshBatches = InMeshBatches;
	});
}

void TiramisuLegacyScene::LoadBuffers(CStreamReader* Reader)
{
	CheckIsGameThread();
    xr_vector<u8> MegaBuffer;
    MegaBuffer.reserve(1024*1024*100);
	R_ASSERT(Reader->find_chunk(fsL_VB));
    {
        CStreamReader* BuffersStreamReader = Reader->open_chunk(fsL_VB);
        u32 CountBuffers = BuffersStreamReader->r_u32();
        for (u32 i = 0; i < CountBuffers; i++)
        {
            u32 StrideSize = 0;
            
        	FLegacyVisualSceneVertexBuffer& VertexBuffer = VertexBuffers.emplace_back();
        	VertexBuffer.VertexType = GetAndConvertFVF(BuffersStreamReader, StrideSize);
        	
            u32 CountVertex = BuffersStreamReader->r_u32();
            u32 Offset = MegaBuffer.size();
        	
        	VertexBuffer.Offset = Offset;
            VertexBuffer.Size = CountVertex * StrideSize;
            VertexBuffer.Stride = StrideSize;
            VertexBuffer.Count = CountVertex;
            
            Msg("* [Loading VB] %d verts, %d Kb", CountVertex, (CountVertex * StrideSize) / 1024);
	
            MegaBuffer.resize(MegaBuffer.size() + CountVertex * StrideSize);
            BuffersStreamReader->r(MegaBuffer.data() + Offset, CountVertex * StrideSize);
	
        }
        BuffersStreamReader->close();
    }
    R_ASSERT(Reader->find_chunk(fsL_IB));
    {
    	auto* BufferStreamReader = Reader->open_chunk(fsL_IB);
    	u32 CountIndexBuffers = BufferStreamReader->r_u32();
    	for (u32 i = 0; i < CountIndexBuffers; i++)
    	{
    		u32 IndexCount = BufferStreamReader->r_u32();
    		Msg("* [Loading IB] %d indices, %d Kb", IndexCount, (IndexCount * 2) / 1024);
    		u32 Offset = MegaBuffer.size();
    		
    		FLegacyVisualSceneIndexBuffer& IndexBuffer = IndexBuffers.emplace_back();
    		IndexBuffer.Offset = Offset;
    		IndexBuffer.Size = IndexCount * 2;
    		IndexBuffer.Count = IndexCount;
    		
    		
    		MegaBuffer.resize(MegaBuffer.size() + IndexCount * 2);
    		BufferStreamReader->r(MegaBuffer.data() + Offset, IndexCount * 2);
    	}
    	BufferStreamReader->close();
    }
	
	StaticMeshRenderData = new TiramisuStaticMeshRenderData;
	
	ENQUEUE_RENDER_COMMAND(TiramisuLegacyScene::LoadBuffers)([InStaticMeshRenderData = StaticMeshRenderData, InMegaBuffer = std::move(MegaBuffer)]
	{
		CheckIsRenderThread();
		{
			nri::BufferDesc BufferDescription = {};
			BufferDescription.size = InMegaBuffer.size();
			BufferDescription.usage = nri::BufferUsageBits::VERTEX_BUFFER | nri::BufferUsageBits::INDEX_BUFFER;
			NRI_CHECK(GRenderDevice.CoreInterface.CreateCommittedBuffer(*GRenderDevice.Device,nri::MemoryLocation::DEVICE, 0.f, BufferDescription, InStaticMeshRenderData->GeometryBuffer));

			nri::BufferUploadDesc BufferUploadData;
			BufferUploadData.buffer = InStaticMeshRenderData->GeometryBuffer;
			BufferUploadData.data = InMegaBuffer.data();
			BufferUploadData.after = {nri::AccessBits::INDEX_BUFFER | nri::AccessBits::VERTEX_BUFFER};

			NRI_CHECK(GRenderDevice.HelperInterface.UploadData(*GRenderDevice.GraphicsQueue, nullptr, 0, &BufferUploadData, 1));
		}
	});
   
}

void TiramisuLegacyScene::LoadVisuals(IReader* Reader)
{
	CheckIsGameThread();
	u32 ChunkIndex = 0;
	IReader* ReaderChunk;
	while ((ReaderChunk = Reader->open_chunk(ChunkIndex)) != 0)
	{
		ogf_header Header;
		ReaderChunk->r_chunk_safe(OGF_HEADER, &Header, sizeof(Header));
		CDS0_RenderVisual* V = GModelPool->Instance_Create(Header.type);
		V->LegacyOwner = this;
		V->Load(0, ReaderChunk, 0);
		Visuals.push_back(V);
		ReaderChunk->close();
		ChunkIndex++;
	}
}

#pragma pack(push,4)
struct FPortalHeader
{
	u16				sector_front;
	u16				sector_back;
	svector<Fvector, 6>	vertices;
};
#pragma pack(pop)

void TiramisuLegacyScene::LoadSectors(IReader* Reader)
{
	CheckIsGameThread();
	u32 HeaderSize = Reader->find_chunk(fsL_PORTALS);
	R_ASSERT(0 == HeaderSize % sizeof(FPortalHeader));
	u32 HeaderCount = HeaderSize / sizeof(FPortalHeader);
	
	Portals.resize(HeaderCount);
	for (u32 i = 0; i < HeaderCount; i++)
	{
		Portals[i] = xr_make_unique<TiramisuLegacyScenePortal>();
	}

	// load sectors
	IReader* SectorReader = Reader->open_chunk(fsL_SECTORS);
	for (u32 i = 0; ; i++)
	{
		IReader* P = SectorReader->open_chunk(i);
		
		if (!P)
		{
			break;
		}
		
		xr_unique_ptr<TiramisuLegacySceneSector>&NewSector = Sectors.emplace_back(xr_make_unique<TiramisuLegacySceneSector>());
		NewSector->LegacyOwner = this;
		NewSector->load(*P);
		P->close();
	}
	SectorReader->close();

	// load portals
	if (HeaderCount)
	{
		CDB::Collector	Collector;
		Reader->find_chunk(fsL_PORTALS);
		for (u32 i = 0; i < HeaderCount; i++)
		{
			FPortalHeader	P;
			Reader->r(&P, sizeof(P));
			
			Portals[i]->Setup(P.vertices.begin(), P.vertices.size(), GetSector(P.sector_front), GetSector(P.sector_back));
			for (u32 j = 2; j < P.vertices.size(); j++)
			{
				Collector.add_face_packed_D(P.vertices[0], P.vertices[j - 1], P.vertices[j], i);
			}
		}
		if (Collector.getTS() < 2)
		{
			Fvector		v1, v2, v3;
			v1.set(-20000.f, -20000.f, -20000.f);
			v2.set(-20001.f, -20001.f, -20001.f);
			v3.set(-20002.f, -20002.f, -20002.f);
			Collector.add_face_packed_D(v1, v2, v3, 0);
		}

		// build portal model
		PortalsCollisionModel = new CDB::MODEL;
		PortalsCollisionModel->build(Collector.getV(), int(Collector.getVS()), Collector.getT(), int(Collector.getTS()),nullptr, nullptr, nullptr, false,false);
	}
	else 
	{
		PortalsCollisionModel = nullptr;
	}
}

void TiramisuLegacyScene::LoadSWIs(CStreamReader* base_fs)
{
	CheckIsGameThread();
}
