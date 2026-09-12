// DetailManager.h: interface for the CDetailManager class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

#include <atomic>
#include "../../xrCore/xrPool.h"
#include "DetailFormat.h"
#include "DetailModel.h"

#ifdef _EDITOR
//.	#include	"ESceneClassList.h"
	constexpr int	dm_max_decompress	= 14;
	class CCustomObject;
	typedef u32	ObjClassID;

    typedef xr_list<CCustomObject*> 		ObjectList;
    typedef ObjectList::iterator 			ObjectIt;
    typedef xr_map<ObjClassID,ObjectList> 	ObjectMap;
    typedef ObjectMap::iterator 			ObjectPairIt;

#else
	constexpr int	dm_max_decompress	= 7;
#endif

constexpr int		dm_obj_in_slot = 4;
constexpr int		dm_slide_window_count = 4;
constexpr int		dm_cache_count = 16;
constexpr float		dm_slot_size = DETAIL_SLOT_SIZE;

extern u32		dm_size;
extern u32 		dm_slide_window_line;
extern u32		dm_cache_line;
extern u32		dm_cache_size;
extern float	dm_fade;
extern u32		dm_current_size;
extern u32 		dm_current_slide_window_line;
extern u32		dm_current_cache_line;
extern u32		dm_current_cache_size;
extern float	dm_current_fade;
extern float	ps_current_detail_density;

class light;

class CDetailManager
{
public:
	poolSS<CDetail::SlotItem, 4096> items_pool;

	struct SlotPart
	{
		u32 id; // ID модельки
		xr_vector<CDetail::SlotItem*> items[3]; // список кустиков

		ICF SlotPart() : id(DetailSlot::ID_Empty){}
	};
	enum SlotType
	{
		stReady = 0, // Ready to use
		stPending, // Pending for unpacking
		stFORCEDWORD = 0xffffffff
	};
	struct Slot
	{
		struct
		{
			u32 empty :1;
			u32 type :1;
		};
		DetailSlot* DS;
		vis_data vis;
		SlotPart G[dm_obj_in_slot];

		ICF Slot() : empty(1), type(stReady), DS(nullptr){ vis.clear(); }
	};
    struct SlideSlot
	{
		u32 empty;
    	vis_data vis;
        Slot** slots[dm_cache_count];
		ICF SlideSlot() : empty(1) { vis.clear(); }
    };
	// swing values
	struct SSwingValue
	{
		float rot1;
		float rot2;
		float amp1;
		float amp2;
		float speed;
		ICF void lerp(const SSwingValue& A, const SSwingValue& B, float f)
		{
			float fi = 1.f - f;
			amp1 = fi * A.amp1 + f * B.amp1;
			amp2 = fi * A.amp2 + f * B.amp2;
			rot1 = fi * A.rot1 + f * B.rot1;
			rot2 = fi * A.rot2 + f * B.rot2;
			speed = fi * A.speed + f * B.speed;
		}
	} swing_desc[2], swing_current;
	float m_time_rot_1 = 0.f;
	float m_time_rot_2 = 0.f;
	float m_time_pos = 0.f, m_time_pos_old = 0.f;
	float m_global_time_old;
	u32 m_frame_render;
	Fvector4 wave_dir1, wave_dir2, wave_dir1_old, wave_dir2_old;

	int dither[16][16];

	IReader* dtFS = nullptr;
	DetailHeader dtH;
	DetailSlot* dtSlots = nullptr;		// note: pointer into VFS
	DetailSlot DS_empty;

	xr_atomic_bool task_finished = true;
	int render_key = 1, calc_key = 0;
	u32 alt_models_start = 0;
	u32 alt_models_count = 0;
	u32 vanilla_grass_indices[64] = {};
	u32 vanilla_grass_count = 0;
#ifndef _EDITOR    
	xr_vector<CDetail> objects;
	CDB::COLLIDER xrc;
#else
	using DetailIt = xr_vector<CDetail*>::iterator;
	xr_vector<CDetail*> objects;
#endif

    xr_vector<xr_vector<SlideSlot>> slide_window;
    xr_vector<xr_vector<Slot*>>		cache;
    xr_vector<Slot*>				unpacked_slots;
    xr_vector<Slot>					cache_pool;

	int								cache_cx;
	int								cache_cz;

	// Precomputed cluster-noise field indexed by detail-DB slot (world cell = dm_slot_size).
	// One byte per DB slot (value in [0, 255] used as deterministic 0..1 sample).
	// cluster_field stores the resolved asset index (fast O(1) at unpack; if the raw
	// value was 255 no mix is used -> index 0 / native). cluster_rnd_field stores the
	// raw deterministic pick hash01(h) in [0,255] so a bake can be re-resolved against
	// a different asset count/mode without recomputing the per-slot noise.
	xr_vector<u8>					cluster_field;
	xr_vector<u8>					cluster_rnd_field;

	// Precomputed combined FMB height factor t (max over enabled layers of
	// mapped*power), one float per detail-DB slot CORNER (grid of
	// (size_x+1)*(size_z+1)), bilinearly sampled at instance position so
	// decompress does no per-instance noise calls.
	// Negative values are reserved for user "clear grass" strokes (t<0 => no grass).
	xr_vector<float>				fmb_field;
	xr_vector<float>				fmb_field_base;		// snapshot before user masks overlay
	xr_vector<u8>					cluster_field_base;
	xr_vector<u8>					cluster_rnd_field_base;

	void							BuildClusterField	();
	u32								SampleClusterField	(float world_x, float world_z, u32 asset_count) const;
	void							BuildFMBField		();
	float							SampleFMBField		(float world_x, float world_z) const;

#ifndef _EDITOR
	// Persist precomputed fields to $level$\detail_layers\ so a reload with unchanged
	// detail settings skips the noise rebuild. All I/O is best-effort: a read-only
	// level folder simply falls back to regenerating.
	bool							DetailLayers_LoadFromBake();	// true => fields came from disk
	void							DetailLayers_SaveToBake();
	void							DetailLayers_ApplySettingsFromBake();	// restore r__detail_* to baked values
	bool							m_detail_layers_baking = false;	// true during level Load() only

	// User paint masks: sparse per-cell overrides layered ON TOP of the baked/generated
	// fields. Files live next to the bakes ($level$\detail_layers\) with a user_mask_
	// prefix and are re-applied on every cache_ReInitialize, so the generated bake stays
	// clean and the user strokes simply overwrite it where drawn.
	struct FMBMaskEntry { u32 idx; float value; };
	struct CLUMaskEntry { u32 idx; u8 value; };
	xr_vector<FMBMaskEntry>			user_fmb_mask;
	xr_vector<CLUMaskEntry>			user_clu_mask;

	void							DetailLayers_LoadUserMasks();	// read user_mask_*.bin, replace current
	void							DetailLayers_SaveUserMasks();	// write user_mask_*.bin
	void							DetailLayers_ClearUserMasks();	// wipe masks, restore base fields
	void							DetailLayers_CommitBaseFields();	// snapshot working fields (pre-mask)
	void							DetailLayers_ApplyUserMasks();	// overlay sparse masks onto fields
	// Stroke helpers. strength in [0,1] is the shared brush intensity for both paint and
	// erase; the stroke blends from the current cell value toward the target (paint) or
	// toward the generated base (erase). A cell that lands back on the base value drops
	// its sparse entry entirely, so full erases leave no stale overrides.
	bool							DetailLayers_PaintFMB(float world_x, float world_z, float value, float strength);
	bool							DetailLayers_EraseFMB(float world_x, float world_z, float strength);
	bool							DetailLayers_PaintCluster(float world_x, float world_z, u8 value, float strength);
	bool							DetailLayers_EraseCluster(float world_x, float world_z, float strength);
#endif

	// Re-unpack ONLY the visible cache slots touched by the brush circle. Called per
	// stroke (throttled by the tool) so edits appear in real time without a full
	// cache_ReInitialize stall. Pending slots are left to the streaming pipeline.
	// Never call this directly from the render thread: cache_Update (and thus the pool
	// / unpacked_slots bookkeeping) runs on the DetailsTask worker, so the rebuild is
	// deferred through DetailLayers_RequestRebuildAround and consumed at the top of
	// cache_Update on that same worker. Direct cross-thread UnpackSlot/UnpackSlotItems
	// corrupted the items pool freelist and crashed.
	void							DetailLayers_RebuildSlotsAround(const Fvector& center, float radius);
	void							DetailLayers_RequestRebuildAround(const Fvector& center, float radius);

	// Brush-rebuild request published by the editor (render thread), consumed by the
	// DetailsTask worker. Fields are written before the release-store of the flag and
	// read after its acquire-load, so the plain members never race.
	std::atomic<bool>				m_brush_rebuild_around{ false };
	Fvector							m_brush_rebuild_center{ 0.f, 0.f, 0.f };
	float							m_brush_rebuild_radius = 0.f;

#ifdef _EDITOR
	virtual ObjectList* 			GetSnapList		()=0;
#else
	// Deferred rebuild: console changes set a flag; cache_Update applies it once per frame
	// so a cfg_load (series of r__detail_* commands) rebuilds all slots a single time.
	void RequestCacheRebuild();
	bool ConsumeCacheRebuildRequest();
#endif

	void							hw_Load			();
	void							hw_Unload		();
	void							hw_Render		(light*L=NULL);

#ifdef USE_DX11
	xr_map<u32, std::pair<IRHIBuffer*, IRHIShaderResourceView*>> DetailInstanceBuffers;

	template<typename T>
	void							hw_Render_dump	(const Fvector4 &wave, const Fvector4 &wind, const Fvector4& wave_old, const Fvector4& wind_old, u32 var_id, u32 lod_id, light*L=NULL);
#else //USE_DX11
	ref_geom						hw_Geom;
	IRHIBuffer*						hw_VB;
	IRHIBuffer*						hw_IB;
	ref_constant					hwc_consts;
	ref_constant					hwc_wave;
	ref_constant					hwc_wind;
	ref_constant					hwc_array;
	ref_constant					hwc_s_consts;
	ref_constant					hwc_s_xform;
	ref_constant					hwc_s_array;
	u32								hw_BatchSize;
	template<typename T, bool light_phase = false>
	void							hw_Render_dump	(ref_constant array, u32 var_id, u32 lod_id, light*L=NULL);
#endif

	// get unpacked slot
	DetailSlot&						QueryDB			(int sx, int sz);

	// Cheap slot-grid LOS factor for AI vision (no instance decompress).
	float							TraceVisibility	(
		Fvector const& eye,
		Fvector const& target,
		float minHeight,
		float opaqueDistance,
		float sampleStep);
    
	void							cache_ReInitialize();
	void							cache_Update	(const Fvector& view);
	void							UnpackSlot		(int gx, int gz, Slot* D);
	void							UnpackSlotItems	(Slot* D);

    // cache grid to world
	ICF int							cg2w_X			(int x)			const { return cache_cx-dm_size+x;					}
	ICF int							cg2w_Z			(int z)			const { return cache_cz-dm_size+(dm_cache_line-1-z);	}
    // world to cache grid 
	ICF int							w2cg_X			(int x)			const { return x-cache_cx+dm_size;					}
	ICF int							w2cg_Z			(int z)			const { return cache_cz-dm_size+(dm_cache_line-1-z);	}

	void							Load			();
	void							Unload			();
	void							Render			();

	void							cache_Alloc();
	void							cache_Free();

	virtual ~CDetailManager() {};
};