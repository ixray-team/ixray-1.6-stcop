#pragma once
class CDS0_GraphManager final : public IDSGraphManager
{
	xr_vector<IKinematics*>& KinematicPool;

public:
	explicit CDS0_GraphManager(xr_vector<IKinematics*>& Pool) : KinematicPool(Pool) {}

	void add_Static(IRenderVisual* Visual, CFrustum& Frustum, u32 Planes) override {}
	void add_Dynamic(IRenderVisual* Visual, Fmatrix* Xform, bool IgnoreOptimize = false, bool Force = false) override;
	void set_Object(IRenderable* O = nullptr) override {}
	IRenderable* get_Object() const override { return nullptr; }
	void set_HUD(bool V = false) override {}
	bool get_HUD() const override { return false; }
	void set_Invisible(bool V = false) override {}
	void set_UI(bool V = false) override {}
};

class CDS0_RenderInterface :
	public IRender_interface,
	public pureFrame
{
	IRender_Target Target;
	xr_vector<IKinematics*> KinematicPool;
	CDS0_GraphManager MainGraph{ KinematicPool };

public:
	CDS0_RenderInterface();

	virtual bool is_sun_static() override;
	virtual u32 get_dx_level() override;

	virtual void create() override;
	virtual void destroy() override;
	virtual void reset_begin() override;
	virtual void reset_end() override;

	virtual void level_Load(IReader*) override;
	virtual void level_Unload() override;

	virtual void Statistics(CGameFont* F) override {};
	virtual const char* getShaderPath() override;
	virtual IRender_Sector* getSector(int id) override;
	virtual IRenderVisual* getVisual(int id) override;
	virtual IRender_Sector* detectSector(const Fvector& P) override;
	virtual IRender_Target* getTarget() override;

	virtual GenerationLevel			get_generation() override { return GenerationLevel::GENERATION_R1; }

	virtual void add_Occluder(Fbox2& bb_screenspace) override;
	virtual IDSGraphManager* GetMainGraph() override { return &MainGraph; }
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
	virtual IRenderVisual* model_CreateParticles(const char* name) override;
	virtual IRenderVisual* model_Create(const char* name, IReader* data = 0) override;
	virtual IRenderVisual* model_CreateChild(const char* name, IReader* data) override;

	virtual IRenderVisual* model_Duplicate(IRenderVisual* V) override;

	virtual void model_Delete(IRenderVisual*& V, bool bDiscard = false) override;
	virtual void models_Prefetch() override;
	virtual void models_Clear(bool b_complete) override;

	virtual bool occ_visible(vis_data& V) override;
	virtual bool occ_visible(Fbox& B) override;
	virtual bool occ_visible(sPoly& P) override;

	virtual void Screenshot(ScreenshotMode mode = SM_NORMAL, const char* name = 0) override;
	virtual void Screenshot(ScreenshotMode mode, CMemoryWriter& memory_writer) override;
	virtual void ScreenshotAsyncBegin() override;
	virtual void ScreenshotAsyncEnd(CMemoryWriter& memory_writer) override;

	virtual void rmNear() override;
	virtual void rmFar() override;
	virtual void rmNormal() override;
	virtual u32 memory_usage() override;

	virtual void                    BeforeWorldRender() ; // Перед рендерингом мира
	virtual void                    AfterWorldRender() ; // После рендеринга мира (до UI)

	virtual void ChangeMark(const char* mark); // Каждый кадр проверяем не поменялась ли текстура
	virtual u32                     active_phase();

	virtual void Render() override;
	virtual void OnFrame() override;
	virtual void Calculate() override;
	virtual void RenderUI(Fcolor* = nullptr) override;


	virtual IRHISurface* load_texture(const char* fname, u32& msize, bool bStaging = false) override { return nullptr; };
	virtual bool get_texture_metadata(const char* absolute_path, RHITextureMetadata* p_data) override { return false; }
	virtual void ScreenshotImpl(ScreenshotMode mode, const char* name, CMemoryWriter* memory_writer) override;

	
};
 extern CDS0_RenderInterface GRenderInterface;
