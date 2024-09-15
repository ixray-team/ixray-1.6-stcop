#ifndef _RENDER_H_
#define _RENDER_H_

#include "../xrCDB/Frustum.h"
#include "vis_common.h"
//#include "IRenderDetailModel.h"

#include "../xrCore/API/xrAPI.h"
#include "../Include/xrRender/FactoryPtr.h"
class IUIShader;
typedef FactoryPtr<IUIShader> wm_shader;

// refs
class ENGINE_API	IRenderable;
struct ENGINE_API	FSlideWindowItem;

//	Igor
class IRenderVisual;
class IKinematics;
class CGameFont;

#ifndef _EDITOR
extern const	float		fLightSmoothFactor;
#else
const	float		fLightSmoothFactor = 4.f;
#endif
//////////////////////////////////////////////////////////////////////////
// definition (Dynamic Light)
class	ENGINE_API	IRender_Light	: public xr_resource									{
public:
	enum LT
	{
		DIRECT		= 0,
		POINT		= 1,
		SPOT		= 2,
		OMNIPART	= 3,
		REFLECTED	= 4,
	};
public:
	virtual void					set_type			(LT type)							= 0;
	virtual void					set_active			(bool)								= 0;
	virtual bool					get_active			()									= 0;
	virtual void					set_shadow			(bool)								= 0;
	virtual void					set_volumetric		(bool)								= 0;
	virtual void					set_volumetric_quality(float)							= 0;
	virtual void					set_volumetric_intensity(float)							= 0;
	virtual void					set_volumetric_distance(float)							= 0;
	virtual void					set_indirect		(bool)								{};
	virtual void					set_position		(const Fvector& P)					= 0;
	virtual void					set_rotation		(const Fvector& D, const Fvector& R)= 0;
	virtual void					set_cone			(float angle)						= 0;
	virtual void					set_range			(float R)							= 0;
	virtual void					set_virtual_size	(float R)							= 0;
	virtual void					set_texture			(LPCSTR name)						= 0;
	virtual void					set_color			(const Fcolor& C)					= 0;
	virtual void					set_color			(float r, float g, float b)			= 0;
	virtual void					set_hud_mode		(bool b)							= 0;
	virtual bool					get_hud_mode		()									= 0;
	virtual vis_data&				get_homdata			()									= 0;

	virtual void	set_occq_mode						(bool b)							= 0;
	virtual bool	get_occq_mode						()									= 0;

	virtual void	set_ignore_object					(CObject* O)						= 0;
	virtual CObject* get_ignore_object					()									= 0;

	virtual void	set_decor_object					(CObject* O, int index = 0)			= 0;
	virtual CObject* get_decor_object					(int index = 0)						= 0;

	virtual ~IRender_Light()		;
};
struct ENGINE_API		resptrcode_light	: public resptr_base<IRender_Light>
{
	void				destroy			()				{ _set(nullptr);						}
};
typedef	resptr_core<IRender_Light,resptrcode_light >	ref_light;

//////////////////////////////////////////////////////////////////////////
// definition (Dynamic Glow)
class	ENGINE_API		IRender_Glow	: public xr_resource									{
public:
	virtual void					set_active			(bool)								= 0;
	virtual bool					get_active			()									= 0;
	virtual void					set_position		(const Fvector& P)					= 0;
	virtual void					set_direction		(const Fvector& P)					= 0;
	virtual void					set_radius			(float R)							= 0;
	virtual void					set_texture			(LPCSTR name)						= 0;
	virtual void					set_color			(const Fcolor& C)					= 0;
	virtual void					set_color			(float r, float g, float b)			= 0;
	virtual ~IRender_Glow()			;
};
struct ENGINE_API		resptrcode_glow	: public resptr_base<IRender_Glow>
{
	void				destroy			()					{ _set(nullptr);					}
};
typedef	resptr_core<IRender_Glow,resptrcode_glow >		ref_glow;

//////////////////////////////////////////////////////////////////////////
// definition (Per-object render-specific data)
class	ENGINE_API	IRender_ObjectSpecific		{
public:
	enum mode
	{
		TRACE_LIGHTS	= (1<<0),
		TRACE_SUN		= (1<<1),
		TRACE_HEMI		= (1<<2),
		TRACE_ALL		= (TRACE_LIGHTS|TRACE_SUN|TRACE_HEMI),
	};
public:
	virtual	void						force_mode			(u32 mode)							= 0;
	virtual float						get_luminocity		()									= 0;
	virtual float						get_luminocity_hemi	()									= 0;
	virtual float*						get_luminocity_hemi_cube		()									= 0;

	virtual ~IRender_ObjectSpecific()	{};
};

//////////////////////////////////////////////////////////////////////////
// definition (Portal)
class	ENGINE_API	IRender_Portal				{
public:
	virtual ~IRender_Portal()			{};
};

//////////////////////////////////////////////////////////////////////////
// definition (Sector)
class	ENGINE_API	IRender_Sector				{
public:
	virtual ~IRender_Sector()			{};
};

//////////////////////////////////////////////////////////////////////////
// definition (Target)
class	ENGINE_API	IRender_Target				{
public:
	virtual	void					set_blur			(float	f)							{};
	virtual	void					set_gray			(float	f)							{};
	virtual void					set_duality_h		(float	f)							{};
	virtual void					set_duality_v		(float	f)							{};
	virtual void					set_noise			(float	f)							{};
	virtual void					set_noise_scale		(float	f)							{};
	virtual void					set_noise_fps		(float	f)							{};
	virtual void					set_color_base		(u32	f)							{};
	virtual void					set_color_gray		(u32	f)							{};
	virtual void					set_color_add		(const Fvector	&f)					{};

	virtual u32						get_width			()									{ return 0;};
	virtual u32						get_height			()									{ return 0;};
	virtual u32						get_target_width	()									{ return 0;};
	virtual u32						get_target_height	()									{ return 0;};
	virtual u32						get_core_width		()									{ return 0;};
	virtual u32						get_core_height		()									{ return 0;};

	virtual void					set_cm_imfluence	(float	f)							{};
	virtual void					set_cm_interpolate	(float	f)							{};
	virtual void					set_cm_textures		(const shared_str &tex0, const shared_str &tex1) {};
	virtual ~IRender_Target()		{};
};

//////////////////////////////////////////////////////////////////////////
// definition (Renderer)
class	ENGINE_API	IRender_interface
{
public:
	enum GenerationLevel
	{
		GENERATION_R1				= 81,
		GENERATION_DX81				= 81,
		GENERATION_R2				= 90,
		GENERATION_DX90				= 90,
		GENERATION_forcedword		= u32(-1)
	};
	enum ScreenshotMode
	{
		SM_NORMAL					= 0,		// jpeg,	name ignored
		SM_FOR_CUBEMAP				= 1,		// tga,		name used as postfix
		SM_FOR_GAMESAVE				= 2,		// dds/dxt1,name used as full-path
		SM_FOR_LEVELMAP				= 3,		// tga,		name used as postfix (level_name)
		SM_FOR_MPSENDING			= 4,
		SM_forcedword				= u32(-1)
	};
public:
	// options
	s32								m_skinning;

	// data
	CFrustum						ViewBase;
	CFrustum*						View;
public:
	// feature level
	virtual	GenerationLevel			get_generation			()											= 0;

	virtual bool					is_sun_static			() { return false; };
	virtual DWORD					get_dx_level			() { return 0;};

	// Loading / Unloading
	virtual	void					create					()											{};
	virtual	void					destroy					()											{};
	virtual	void					reset_begin				()											{};
	virtual	void					reset_end				()											{};

	virtual	void					level_Load				(IReader*)									{};
	virtual void					level_Unload			()											{};

	virtual BOOL					InIndoor				()											{ return false; }
	virtual size_t					SectorsCount			()											{ return size_t(0); }

			void					shader_option_skinning	(s32 mode)									{ m_skinning=mode;	}
	virtual HRESULT					shader_compile			(
		LPCSTR							name,
		DWORD const*                    pSrcData,
		UINT                            SrcDataLen,
		LPCSTR                          pFunctionName,
		LPCSTR                          pTarget,
		DWORD                           Flags,
		void*&							result
	)																									{return S_OK;};

	// Information
	virtual	void					Statistics				(CGameFont* F	)							{};

	virtual LPCSTR					getShaderPath			()											{return 0;};
	virtual IRender_Sector*			getSector				(int id)									{return 0;};
	virtual IRenderVisual*			getVisual				(int id)									{return 0;};
	virtual IRender_Sector*			detectSector			(const Fvector& P)							{return 0;};
	virtual IRender_Target*			getTarget				()											{return 0;};

	struct SurfaceParams
	{
		float w = 0.0f;
		float h = 0.0f;
		void* Surface = nullptr;
	};

	virtual SurfaceParams			getSurface(const char* nameTexture) { R_ASSERT(!"Method is not overridden"); return SurfaceParams(); };

	// Main 
	IC		void					set_Frustum				(CFrustum*	O	)							{ VERIFY(O);	View = O;			}
	virtual void					set_Transform			(Fmatrix*	M	)							{};
	virtual void					set_HUD					(BOOL 		V	)							{};
	virtual BOOL					get_HUD					()											{ return 0; };
	virtual void					set_Invisible			(BOOL 		V	)							{};
	virtual void					flush					()											{};	
	virtual void					set_Object				(IRenderable*		O	)					{};
	virtual	void					add_Occluder			(Fbox2&	bb_screenspace	)					{};	// mask screen region as oclluded (-1..1, -1..1)
	virtual void					add_Visual				(IRenderVisual*	V, bool ignore_opt = false)	{};	// add visual leaf	(no culling performed at all)
	virtual void					add_Geometry			(IRenderVisual*	V	)					{};	// add visual(s)	(all culling performed)
	virtual void					add_StaticWallmark		(const wm_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V) {};

	//	Prefer this function when possible
	virtual void					add_StaticWallmark		(IWallMarkArray *pArray, const Fvector& P, float s, CDB::TRI* T, Fvector* V) {};
	virtual void					clear_static_wallmarks	() {};

	//	Prefer this function when possible
	virtual void					add_SkeletonWallmark	(const Fmatrix* xf, IKinematics* obj, IWallMarkArray *pArray, const Fvector& start, const Fvector& dir, float size) {};

	virtual IRender_ObjectSpecific*	ros_create				(IRenderable* parent)						{ return 0; };
	virtual void					ros_destroy				(IRender_ObjectSpecific* &)					{};

	// Lighting/glowing
	virtual IRender_Light*			light_create			()											{ return 0; };
	virtual void					light_destroy			(IRender_Light* p_)							{ };
	virtual IRender_Glow*			glow_create				()											{ return 0; };
	virtual void					glow_destroy			(IRender_Glow* p_)							{ };

	// Models
	virtual IRenderVisual*			model_CreateParticles	(LPCSTR name)								{return 0;};
	virtual IRenderVisual*			model_Create			(LPCSTR name, IReader*	data=0)				{return 0;};
	virtual IRenderVisual*			model_CreateChild		(LPCSTR name, IReader*	data)				{return 0;};
	virtual IRenderVisual*			model_Duplicate			(IRenderVisual*	V)							{return 0;};
	virtual void					model_Delete			(IRenderVisual* &	V, BOOL bDiscard=FALSE)	{};
	virtual void					model_Logging			(BOOL bEnable)								{};
	virtual void					models_Prefetch			()											{};
	virtual void					models_Clear			(BOOL b_complete)							{};

	// Occlusion culling
	virtual BOOL					occ_visible				(vis_data&	V)								{return false;};
	virtual BOOL					occ_visible				(Fbox&		B)								{return false;};
	virtual BOOL					occ_visible				(sPoly&		P)								{return false;};

	// Main
	virtual void					Calculate				()											{};
	virtual void					Render					()											{};
	
	virtual void					Screenshot				(ScreenshotMode mode=SM_NORMAL, LPCSTR name = 0) {};
	virtual	void					Screenshot				(ScreenshotMode mode, CMemoryWriter& memory_writer) {};
	virtual void					ScreenshotAsyncBegin	() {};
	virtual void					ScreenshotAsyncEnd		(CMemoryWriter& memory_writer) {};

	// Render mode
	virtual void					rmNear					()											{};
	virtual void					rmFar					()											{};
	virtual void					rmNormal				()											{};
	virtual u32						memory_usage			()											{ return 0;};

	// Constructor/destructor
	virtual ~IRender_interface() = default;
protected:
	virtual	void					ScreenshotImpl			(ScreenshotMode mode, LPCSTR name, CMemoryWriter* memory_writer) {};
};

//extern ENGINE_API	IRender_interface*	Render;

#endif