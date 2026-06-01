#include "stdafx.h"
#include "Extensions/NRIDeviceCreation.h"
#include "Visual/XRayModelPool.h"
#include "Light/XRayRenderLight.h"
#include "Light/XRayRenderGlow.h"
#include "Light/XRayObjectSpecific.h"
#include "Resources/XRayRenderResourcesManager.h"
#include "src/xrCore/stream_reader.h"
#include "src/xrEngine/IGame_Persistent.h"
#include "src/xrEngine/xrLevel.h"

CDS0_RenderInterface GRenderInterface;


CDS0_RenderInterface::CDS0_RenderInterface()
{

}

bool CDS0_RenderInterface::is_sun_static()
{
	return false;
}

DWORD CDS0_RenderInterface::get_dx_level()
{
	return 0;
}

void CDS0_RenderInterface::create()
{
	
	DevicePtr->seqFrame.Add(this);
	GModelPool = new CDS0_ModelPool;
	GRenderDevice.Initialize();
	GRenderResourcesManager = new XRayRenderResourcesManager;
	GRenderResourcesManager->Initialize();
	GRender = new XRayRender;
	GRender->Initialize();
}

void CDS0_RenderInterface::destroy()
{
	GRender->Destroy();
	delete GRender;
	GRender = nullptr;
	delete GRenderResourcesManager;
	GRenderResourcesManager = nullptr;
	DevicePtr->seqFrame.Remove(this);
	xr_delete(GModelPool);
}

void CDS0_RenderInterface::reset_begin()
{
}

void CDS0_RenderInterface::reset_end()
{
}

void CDS0_RenderInterface::level_Load(IReader* fs)
{
	R_ASSERT(g_pGamePersistent);
	IReader* chunk;
	{
		chunk = fs->open_chunk(fsL_SHADERS);
		R_ASSERT2(chunk, "Level doesn't builded correctly.");
		u32 count = chunk->r_u32();
		//m_Shader.resize(count);
		for (u32 i = 0; i < count; i++)	// skip first shader as "reserved" one
		{
			string512				n_sh, n_tlist;
			LPCSTR			n = LPCSTR(chunk->pointer());
			chunk->skip_stringZ();
			if (0 == n[0])			continue;
			xr_strcpy(n_sh, n);
			LPSTR			delim = strchr(n_sh, '/');
			*delim = 0;
			xr_strcpy(n_tlist, delim + 1);
			Msg("Level shader:%s", n_sh);
			//GResourcesManager->CompileBlender(m_Shader[i], n_sh, n_tlist);
		}
		chunk->close();
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
		chunk = fs->open_chunk(fsL_VISUALS);
		LoadVisuals(chunk);
		chunk->close();
	}
	LoadSectors(fs);

	// HOM.Load();
	// GRenderTarget->LoadLevel();
}

void CDS0_RenderInterface::level_Unload()
{
}

HRESULT CDS0_RenderInterface::shader_compile(LPCSTR name, DWORD const* pSrcData, UINT SrcDataLen, LPCSTR pFunctionName, LPCSTR pTarget, DWORD Flags, void*& result)
{
	return E_NOTIMPL;
}

LPCSTR CDS0_RenderInterface::getShaderPath()
{
	return LPCSTR();
}

IRender_Sector* CDS0_RenderInterface::getSector(int id)
{
	return nullptr;
}

IRenderVisual* CDS0_RenderInterface::getVisual(int id)
{
	return nullptr;
}

IRender_Sector* CDS0_RenderInterface::detectSector(const Fvector& P)
{
	return nullptr;
}

IRender_Target* CDS0_RenderInterface::getTarget()
{
	return &Target;
}

void CDS0_RenderInterface::set_Transform(Fmatrix* M)
{
}

void CDS0_RenderInterface::set_HUD(bool V)
{
}

void CDS0_RenderInterface::set_UI(bool V)
{
}

bool CDS0_RenderInterface::get_HUD()
{
	return 0;
}

void CDS0_RenderInterface::set_Invisible(bool V)
{
}

void CDS0_RenderInterface::set_Object(IRenderable* O)
{
}

void CDS0_RenderInterface::add_Occluder(Fbox2& bb_screenspace)
{
}

void CDS0_RenderInterface::add_Visual(IRenderVisual* V)
{
	if (IKinematics* IK = V->dcast_PKinematics())
	{
		IK->CalculateBones();
		KinematicPool.push_back(IK);
	}
}

void CDS0_RenderInterface::add_Geometry(IRenderVisual* V)
{
}

void CDS0_RenderInterface::add_StaticWallmark(const wm_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V)
{
}

void CDS0_RenderInterface::add_StaticWallmark(IWallMarkArray* pArray, const Fvector& P, float s, CDB::TRI* T, Fvector* V)
{
}

void CDS0_RenderInterface::add_SkeletonWallmark(const Fmatrix* xf, IKinematics* obj, IWallMarkArray* pArray, const Fvector& start, const Fvector& dir, float size)
{
}

StaticWallmarkHandle::WallmarkHandlePtr CDS0_RenderInterface::add_DynamicWallmark(const wm_shader& S, const Fvector& P,
	float w, float h, float r, CDB::TRI* T, Fvector* V)
{
	return nullptr;
}

void CDS0_RenderInterface::clear_static_wallmarks()
{
}

void CDS0_RenderInterface::flush()
{
}

IRender_ObjectSpecific* CDS0_RenderInterface::ros_create(IRenderable* parent)
{
	return new CDS0_RenderObjectSpecific;
}

void CDS0_RenderInterface::ros_destroy(IRender_ObjectSpecific*&)
{
}

IRender_Light* CDS0_RenderInterface::light_create()
{
	return new CDS0_RenderLight;
}

IRender_Glow* CDS0_RenderInterface::glow_create()
{
	return new CDS0_RenderGlow;
}

IRenderVisual* CDS0_RenderInterface::model_CreateParticles(LPCSTR name)
{
	return nullptr;
}
IRenderVisual* CDS0_RenderInterface::model_Create(LPCSTR name, IReader* data)
{
	CDS0_RenderVisual* VisualPtr = GModelPool->Create(name, data);
	return VisualPtr;
}

IRenderVisual* CDS0_RenderInterface::model_CreateChild(LPCSTR name, IReader* data)
{
	return  GModelPool->CreateChild(name, data);
}

IRenderVisual* CDS0_RenderInterface::model_Duplicate(IRenderVisual* V)
{
	return GModelPool->Instance_Duplicate((CDS0_RenderVisual*)V);
}

void CDS0_RenderInterface::model_Delete(IRenderVisual*& V, bool bDiscard)
{
	if (V == nullptr)
		return;

	CDS0_RenderVisual* pVisual = (CDS0_RenderVisual*)V;

	GModelPool->Delete(pVisual, bDiscard);
	V = nullptr;
}

void CDS0_RenderInterface::models_Prefetch()
{
	GModelPool->Prefetch();
}

void CDS0_RenderInterface::models_Clear(bool b_complete)
{
}

bool CDS0_RenderInterface::occ_visible(vis_data& V)
{
	return 0;
}

bool CDS0_RenderInterface::occ_visible(Fbox& B)
{
	return 0;
}

bool CDS0_RenderInterface::occ_visible(sPoly& P)
{
	return 0;
}

void CDS0_RenderInterface::Screenshot(ScreenshotMode mode, LPCSTR name)
{
}

void CDS0_RenderInterface::Screenshot(ScreenshotMode mode, CMemoryWriter& memory_writer)
{
}

void CDS0_RenderInterface::ScreenshotAsyncBegin()
{
}

void CDS0_RenderInterface::ScreenshotAsyncEnd(CMemoryWriter& memory_writer)
{
}

void CDS0_RenderInterface::rmNear()
{
}

void CDS0_RenderInterface::rmFar()
{
}

void CDS0_RenderInterface::rmNormal()
{
}

u32 CDS0_RenderInterface::memory_usage()
{
	return u32();
}

void CDS0_RenderInterface::BeforeWorldRender()
{
}

void CDS0_RenderInterface::AfterWorldRender()
{
}

void CDS0_RenderInterface::ChangeMark(LPCSTR mark)
{
}

u32 CDS0_RenderInterface::active_phase()
{
	return u32();
}

void CDS0_RenderInterface::Render()
{
	GModelPool->Render();
	{
		GRender->Render();
		// RenderViewport.BeginRender();
		
		// RenderViewport.EndRender();
	}
}

void CDS0_RenderInterface::RenderUI(bool)
{
}

void CDS0_RenderInterface::OnFrame()
{
	GModelPool->DeleteQueue();

	for (IKinematics* IK : KinematicPool)
	{
		//IK->CalculateBones_Invalidate();
		IK->CalculateBones();
	}
	KinematicPool.clear();
}

void CDS0_RenderInterface::Calculate()
{
}

void CDS0_RenderInterface::ScreenshotImpl(ScreenshotMode mode, LPCSTR name, CMemoryWriter* memory_writer)
{
}

void CDS0_RenderInterface::LoadBuffers(CStreamReader* base_fs)
{
	// if (base_fs->find_chunk(fsL_VB))
	// {
	// 	// Use DX9-style declarators
	// 	auto* fs = base_fs->open_chunk(fsL_VB);
	// 	u32 count = fs->r_u32();
	// 	m_VertexState.resize(count);
	// 	m_VertexBuffer.resize(count);
	// 	for (u32 i = 0; i < count; i++)
	// 	{
	// 		u32					buffer_size = (64 + 1) * sizeof(D3DVERTEXELEMEN_D3D9);
	// 		D3DVERTEXELEMEN_D3D9* dcl = (D3DVERTEXELEMEN_D3D9*)_alloca(buffer_size);
	// 		fs->r(dcl, buffer_size);
	// 		fs->advance(-(int)buffer_size);
	// 		fs->advance(GetSize(dcl));
	//
	// 		m_VertexState[i] = ConvertFVF(dcl);
	// 		u32 vCount = fs->r_u32();
	// 		u32 vSize = GResourcesManager->GetStride(m_VertexState[i]);
	// 		Msg("* [Loading VB] %d verts, %d Kb", vCount, (vCount * vSize) / 1024);
	//
	// 		u8* pData = xr_alloc<u8>(vCount * vSize);
	// 		fs->r(pData, vCount * vSize);
	// 		m_VertexBuffer[i] = BearRenderInterface::CreateVertexBuffer();
	// 		m_VertexBuffer[i]->Create(vSize, vCount, false, pData);
	// 		xr_free(pData);
	//
	// 	}
	// 	fs->close();
	// }
	// else {
	// 	FATAL("DX7-style FVFs unsupported");
	// }
	// if (base_fs->find_chunk(fsL_IB))
	// {
	// 	auto* fs = base_fs->open_chunk(fsL_IB);
	// 	u32 count = fs->r_u32();
	// 	m_IndexBuffers.resize(count);
	// 	for (u32 i = 0; i < count; i++)
	// 	{
	// 		u32 iCount = fs->r_u32();
	// 		Msg("* [Loading IB] %d indices, %d Kb", iCount, (iCount * 2) / 1024);
	//
	// 		u32* pData = xr_alloc<u32>(iCount);
	// 		fs->r(pData, iCount * 2);
	// 		u16* srcData = (u16*)pData;
	//
	// 		for (u32 a = iCount; a != 0; a--)
	// 		{
	// 			pData[a - 1] = srcData[a - 1];
	// 		}
	// 		m_IndexBuffers[i] = BearRenderInterface::CreateIndexBuffer();
	// 		m_IndexBuffers[i]->Create(iCount, true, pData);
	// 		xr_free(pData);
	// 	}
	// 	fs->close();
	// }
}

void CDS0_RenderInterface::LoadVisuals(IReader* fs)
{
}

void CDS0_RenderInterface::LoadSectors(IReader* fs)
{
}

void CDS0_RenderInterface::LoadSWIs(CStreamReader* base_fs)
{
}
