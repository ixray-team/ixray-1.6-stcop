#pragma once
class XRayRenderInterface :public IRender_interface,public pureFrame
{
public:
	XRayRenderInterface();

	virtual bool is_sun_static();
	virtual DWORD get_dx_level();

	virtual void create();
	virtual void destroy();
	virtual void reset_begin();
	virtual void reset_end();

	virtual void level_Load(IReader*);
	virtual void level_Unload();

	virtual HRESULT shader_compile(LPCSTR name,	DWORD const* pSrcData,	UINT SrcDataLen,	LPCSTR pFunctionName,LPCSTR pTarget,	DWORD Flags,	void*& result);

	virtual void Statistics(CGameFont* F) {};
	virtual LPCSTR getShaderPath();
	virtual IRender_Sector* getSector(int id);
	virtual IRenderVisual* getVisual(int id);
	virtual IRender_Sector* detectSector(const Fvector& P);
	virtual IRender_Target* getTarget();

	virtual void set_Transform(Fmatrix* M);
	virtual void set_HUD(BOOL V);
	virtual BOOL get_HUD();
	virtual void set_Invisible(BOOL V);
	virtual void set_Object(IRenderable* O);
	virtual	GenerationLevel			get_generation() { return GenerationLevel::GENERATION_R1; }

	virtual void add_Occluder(Fbox2& bb_screenspace); 
	virtual void add_Visual(IRenderVisual* V); 
	virtual void add_Geometry(IRenderVisual* V);
	virtual void add_StaticWallmark(const wm_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V);
	virtual void add_StaticWallmark(IWallMarkArray* pArray, const Fvector& P, float s, CDB::TRI* T, Fvector* V);
	virtual void add_SkeletonWallmark(const Fmatrix* xf, IKinematics* obj, IWallMarkArray* pArray, const Fvector& start, const Fvector& dir, float size);

	virtual void clear_static_wallmarks();

	virtual void flush();

	virtual IRender_ObjectSpecific* ros_create(IRenderable* parent);
	virtual void ros_destroy(IRender_ObjectSpecific*&);

	// Lighting/glowing
	virtual IRender_Light* light_create();
	virtual IRender_Glow* glow_create();

	// Models
	virtual IRenderVisual* model_CreateParticles(LPCSTR name);
	virtual IRenderVisual* model_Create(LPCSTR name, IReader* data = 0);
	virtual IRenderVisual* model_CreateChild(LPCSTR name, IReader* data);

	virtual IRenderVisual* model_Duplicate(IRenderVisual* V);

	virtual void model_Delete(IRenderVisual*& V, BOOL bDiscard = FALSE);
	virtual void model_Logging(BOOL bEnable);
	virtual void models_Prefetch();
	virtual void models_Clear(BOOL b_complete);

	virtual BOOL occ_visible(vis_data& V);
	virtual BOOL occ_visible(Fbox& B);
	virtual BOOL occ_visible(sPoly& P);

	virtual void Screenshot(ScreenshotMode mode = SM_NORMAL, LPCSTR name = 0);
	virtual void Screenshot(ScreenshotMode mode, CMemoryWriter& memory_writer);
	virtual void ScreenshotAsyncBegin();
	virtual void ScreenshotAsyncEnd(CMemoryWriter& memory_writer);

	virtual void rmNear();
	virtual void rmFar();
	virtual void rmNormal();
	virtual u32 memory_usage();

	virtual void                    BeforeWorldRender() ; // Перед рендерингом мира
	virtual void                    AfterWorldRender() ; // После рендеринга мира (до UI)

	virtual void					ChangeMark(LPCSTR mark) ; // Каждый кадр проверяем не поменялась ли текстура
	virtual u32                     active_phase();

	virtual void Render();
	virtual void OnFrame();
	virtual void Calculate();


	virtual void ScreenshotImpl(ScreenshotMode mode, LPCSTR name, CMemoryWriter* memory_writer);

	
};
 extern XRayRenderInterface GRenderInterface;