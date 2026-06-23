#include "stdafx.h"
#include "Visual/XRayModelPool.h"
#include "Light/XRayRenderLight.h"
#include "Light/XRayRenderGlow.h"
#include "Light/XRayObjectSpecific.h"

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
	Device.seqFrame.Add(this);
	GModelPool = new CDS0_ModelPool;
}

void CDS0_RenderInterface::destroy()
{
	Device.seqFrame.Remove(this);
	xr_delete(GModelPool);
}

void CDS0_RenderInterface::reset_begin()
{
}

void CDS0_RenderInterface::reset_end()
{
}

void CDS0_RenderInterface::level_Load(IReader*)
{
}

void CDS0_RenderInterface::level_Unload()
{
}

HRESULT CDS0_RenderInterface::shader_compile(str_c name, DWORD const* pSrcData, UINT SrcDataLen, str_c pFunctionName, str_c pTarget, DWORD Flags, void*& result)
{
	return E_NOTIMPL;
}

str_c CDS0_RenderInterface::getShaderPath()
{
	return nullptr;
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

void CDS0_RenderInterface::set_Transform(Fmatrix& M)
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

void CDS0_RenderInterface::add_Visual(IRenderVisual* V, bool IgnoreOptimize, bool Force)
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

void CDS0_RenderInterface::add_StaticWallmark(const wm_shader& S, const Fvector& P, float s, const CDB::TRI& T, Fvector* V)
{
}

void CDS0_RenderInterface::add_StaticWallmark(IWallMarkArray* pArray, const Fvector& P, float s, const CDB::TRI& T, Fvector* V, bool UseCameraDirection)
{
}

void CDS0_RenderInterface::add_SkeletonWallmark(const Fmatrix* xf, IKinematics* obj, IWallMarkArray* pArray, const Fvector& start, const Fvector& dir, float size)
{
}

StaticWallmarkHandle::WallmarkHandlePtr CDS0_RenderInterface::add_DynamicWallmark(const wm_shader& S, const Fvector& P,
	float w, float h, float r, const CDB::TRI& T, Fvector* V)
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

IRenderVisual* CDS0_RenderInterface::model_CreateParticles(str_c name)
{
	return nullptr;
}
IRenderVisual* CDS0_RenderInterface::model_Create(str_c name, IReader* data)
{
	CDS0_RenderVisual* VisualPtr = GModelPool->Create(name, data);
	return VisualPtr;
}

IRenderVisual* CDS0_RenderInterface::model_CreateChild(str_c name, IReader* data)
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

void CDS0_RenderInterface::Screenshot(ScreenshotMode mode, str_c name)
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

void CDS0_RenderInterface::ChangeMark(str_c mark)
{
}

u32 CDS0_RenderInterface::active_phase()
{
	return u32();
}

void CDS0_RenderInterface::Render()
{
	GModelPool->Render();
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

void CDS0_RenderInterface::ScreenshotImpl(ScreenshotMode mode, str_c name, CMemoryWriter* memory_writer)
{
}

GeomData& CDS0_RenderInterface::GetMUSlot(shared_str Name)
{
	static GeomData dummy;
	return dummy;
}

void CDS0_RenderInterface::ReadVBChunk(xr_vector<IRHIBuffer*>& OutBuffer, xr_vector<VertexDeclarator>& DeclBuffer, u32 Count, IReaderBase& fs)
{
}

void CDS0_RenderInterface::ReadIBChunk(xr_vector<IRHIBuffer*>& OutBuffer, IReaderBase& fs)
{
}

void CDS0_RenderInterface::ReadSWIsChunk(xr_vector<FSlideWindowItem>& SWIs, IReaderBase& fs)
{
}
