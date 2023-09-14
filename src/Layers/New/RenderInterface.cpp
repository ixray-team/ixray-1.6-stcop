#include "pch.h"

using namespace Rendering;

RenderInterface RenderEngineInterface;

RenderInterface::RenderInterface()
{
}

RenderInterface::~RenderInterface()
{
}

IRender_interface::GenerationLevel 
RenderInterface::get_generation()
{
	return GenerationLevel();
}

bool 
RenderInterface::is_sun_static()
{
	return false;
}

DWORD 
RenderInterface::get_dx_level()
{
	return 0;
}

void
RenderInterface::create()
{
}

void
RenderInterface::destroy()
{
}

void
RenderInterface::reset_begin()
{
}

void
RenderInterface::reset_end()
{
}

void 
RenderInterface::level_Load(IReader*)
{
}

void
RenderInterface::level_Unload()
{
}

HRESULT
RenderInterface::shader_compile(LPCSTR name, DWORD const* pSrcData, UINT SrcDataLen, LPCSTR pFunctionName, LPCSTR pTarget, DWORD Flags, void*& result)
{
	return E_NOTIMPL;
}

LPCSTR
RenderInterface::getShaderPath()
{
	return LPCSTR();
}

IRender_Sector*
RenderInterface::getSector(int id)
{
	return nullptr;
}

IRenderVisual* 
RenderInterface::getVisual(int id)
{
	return nullptr;
}

IRender_Sector*
RenderInterface::detectSector(const Fvector& P)
{
	return nullptr;
}

IRender_Target*
RenderInterface::getTarget()
{
	return nullptr;
}

void
RenderInterface::set_Transform(Fmatrix* M)
{
}

void 
RenderInterface::set_HUD(BOOL V)
{
}

BOOL 
RenderInterface::get_HUD()
{
	return 0;
}

void
RenderInterface::set_Invisible(BOOL V)
{
}

void 
RenderInterface::flush()
{
}

void
RenderInterface::set_Object(IRenderable* O)
{
}

void
RenderInterface::add_Occluder(Fbox2& bb_screenspace)
{
}

void
RenderInterface::add_Visual(IRenderVisual* V)
{
}

void
RenderInterface::add_Geometry(IRenderVisual* V)
{
}

void 
RenderInterface::add_StaticWallmark(const wm_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V)
{
}

void 
RenderInterface::add_StaticWallmark(IWallMarkArray* pArray, const Fvector& P, float s, CDB::TRI* T, Fvector* V)
{
}

void 
RenderInterface::clear_static_wallmarks()
{
}

void 
RenderInterface::add_SkeletonWallmark(
	const Fmatrix* xf,
	IKinematics* obj, 
	IWallMarkArray* pArray, 
	const Fvector& start, 
	const Fvector& dir, 
	float size
)
{
}

IRender_ObjectSpecific*
RenderInterface::ros_create(IRenderable* parent)
{
	return nullptr;
}

void
RenderInterface::ros_destroy(IRender_ObjectSpecific*&)
{
}

IRender_Light*
RenderInterface::light_create()
{
	return nullptr;
}

void
RenderInterface::light_destroy(IRender_Light* p_)
{
}

IRender_Glow* 
RenderInterface::glow_create()
{
	return nullptr;
}

void
RenderInterface::glow_destroy(IRender_Glow* p_)
{
}

IRenderVisual*
RenderInterface::model_CreateParticles(LPCSTR name)
{
	return nullptr;
}

IRenderVisual*
RenderInterface::model_Create(LPCSTR name, IReader* data)
{
	return nullptr;
}

IRenderVisual* 
RenderInterface::model_CreateChild(LPCSTR name, IReader* data)
{
	return nullptr;
}

IRenderVisual*
RenderInterface::model_Duplicate(IRenderVisual* V)
{
	return nullptr;
}

void
RenderInterface::model_Delete(IRenderVisual*& V, BOOL bDiscard)
{
}

void 
RenderInterface::model_Logging(BOOL bEnable)
{
}

void
RenderInterface::models_Prefetch()
{
}

void
RenderInterface::models_Clear(BOOL b_complete)
{
}

BOOL 
RenderInterface::occ_visible(vis_data& V)
{
	return 0;
}

BOOL 
RenderInterface::occ_visible(Fbox& B)
{
	return 0;
}

BOOL 
RenderInterface::occ_visible(sPoly& P)
{
	return 0;
}

void 
RenderInterface::Calculate()
{
}

void 
RenderInterface::Render()
{
}

void 
RenderInterface::Screenshot(ScreenshotMode mode, LPCSTR name)
{
}

void
RenderInterface::Screenshot(ScreenshotMode mode, CMemoryWriter& memory_writer)
{
}

void 
RenderInterface::ScreenshotAsyncBegin()
{
}

void 
RenderInterface::ScreenshotAsyncEnd(CMemoryWriter& memory_writer)
{
}

void
RenderInterface::rmNear()
{
}

void
RenderInterface::rmFar()
{
}

void
RenderInterface::rmNormal()
{
}

u32 
RenderInterface::memory_usage()
{
	return u32();
}

void 
RenderInterface::ScreenshotImpl(ScreenshotMode mode, LPCSTR name, CMemoryWriter* memory_writer)
{
}
