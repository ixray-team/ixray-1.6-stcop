#pragma once
class CDS0_RenderInterface :
	public IRender_interface,
	public pureFrame
{
	IRender_Target Target;
	xr_vector<IKinematics*> KinematicPool;

public:
	CDS0_RenderInterface();

	virtual bool is_sun_static() override;
	virtual DWORD get_dx_level() override;

	virtual void create() override;
	virtual void destroy() override;
	virtual void reset_begin() override;
	virtual void reset_end() override;

	virtual void level_Load(IReader*) override;
	virtual void level_Unload() override;

	virtual HRESULT shader_compile(str_c name, DWORD const* pSrcData, UINT SrcDataLen, str_c pFunctionName,str_c pTarget, DWORD Flags, void*& result) override;

	virtual void Statistics(CGameFont* F) override {};
	virtual str_c getShaderPath() override;
	virtual IRender_Sector* getSector(int id) override;
	virtual IRenderVisual* getVisual(int id) override;
	virtual IRender_Sector* detectSector(const Fvector& P) override;
	virtual IRender_Target* getTarget() override;

	virtual void set_Transform(Fmatrix& M) override;
	virtual void set_HUD(bool V) override;
	virtual void set_UI(bool V) override;
	virtual bool get_HUD() override;
	virtual void set_Invisible(bool V) override;
	virtual void set_Object(IRenderable* O) override;
	virtual	GenerationLevel get_generation() override { return GenerationLevel::GENERATION_R1; }

	virtual void add_Occluder(Fbox2& bb_screenspace) override; 
	virtual void add_Visual(IRenderVisual* V, bool IgnoreOptimize = false, bool Force = false) override; 
	virtual void add_Geometry(IRenderVisual* V) override;
	virtual void add_StaticWallmark(const wm_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V) override;
	virtual void add_StaticWallmark(IWallMarkArray* pArray, const Fvector& P, float s, CDB::TRI* T, Fvector* V);
	virtual void add_SkeletonWallmark(const Fmatrix* xf, IKinematics* obj, IWallMarkArray* pArray, const Fvector& start, const Fvector& dir, float size) override;
	virtual StaticWallmarkHandle::WallmarkHandlePtr add_DynamicWallmark(const wm_shader& S, const Fvector& P, float w, float h, float r, CDB::TRI* T, Fvector* V) override;

	virtual void clear_static_wallmarks() override;

	virtual void flush() override;

	virtual IRender_ObjectSpecific* ros_create(IRenderable* parent) override;
	virtual void ros_destroy(IRender_ObjectSpecific*&) override;

	// Lighting/glowing
	virtual IRender_Light* light_create() override;
	virtual IRender_Glow* glow_create() override;

	// Models
	virtual IRenderVisual* model_CreateParticles(str_c name) override;
	virtual IRenderVisual* model_Create(str_c name, IReader* data = nullptr) override;
	virtual IRenderVisual* model_CreateChild(str_c name, IReader* data) override;

	virtual IRenderVisual* model_Duplicate(IRenderVisual* V) override;

	virtual void model_Delete(IRenderVisual*& V, bool bDiscard = false) override;
	virtual void models_Prefetch() override;
	virtual void models_Clear(bool b_complete) override;

	virtual bool occ_visible(vis_data& V) override;
	virtual bool occ_visible(Fbox& B) override;
	virtual bool occ_visible(sPoly& P) override;

	virtual void Screenshot(ScreenshotMode mode = SM_NORMAL, str_c name = nullptr) override;
	virtual void Screenshot(ScreenshotMode mode, CMemoryWriter& memory_writer) override;
	virtual void ScreenshotAsyncBegin() override;
	virtual void ScreenshotAsyncEnd(CMemoryWriter& memory_writer) override;

	virtual void rmNear() override;
	virtual void rmFar() override;
	virtual void rmNormal() override;
	virtual u32 memory_usage() override;

	virtual void BeforeWorldRender() ; // Перед рендерингом мира
	virtual void AfterWorldRender() ; // После рендеринга мира (до UI)

	virtual void ChangeMark(str_c mark) ; // Каждый кадр проверяем не поменялась ли текстура
	virtual u32 active_phase();

	virtual void Render() override;
	virtual void OnFrame() override;
	virtual void Calculate() override;
	virtual void RenderUI(bool = false) override;


	IRHISurface* load_texture(str_c fname, u32& msize, bool bStaging = false) override { return nullptr; };
	bool get_texture_metadata(str_c absolute_path, RHITextureMetadata* p_data) override { return false; }
	virtual void ScreenshotImpl(ScreenshotMode mode, str_c name, CMemoryWriter* memory_writer) override;

	
};
 extern CDS0_RenderInterface GRenderInterface;