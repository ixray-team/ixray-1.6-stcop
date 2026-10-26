// DetailManager.h: interface for the CDetailManager class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

#include <atomic>
#include <mutex>
#include "../../xrCore/xrPool.h"
#include "../../xrCore/_static_thread.h"
#include "DetailFormat.h"
#include "DetailModel.h"
#include "DetailTramplePresentation.h"
#include "RenderAPI.h"
#include "../../xrCore/Kernel/xrSyncronize.h"

class CCustomObject;
typedef u32 ObjClassID;

typedef xr_list<CCustomObject*> ObjectList;
typedef ObjectList::iterator ObjectIt;
typedef xr_map<ObjClassID, ObjectList> ObjectMap;
typedef ObjectMap::iterator ObjectPairIt;

constexpr int dm_obj_in_slot = 4;
constexpr int dm_slide_window_count = 4;
constexpr int dm_cache_count = 16;
constexpr float dm_slot_size = DETAIL_SLOT_SIZE;

extern RENDER_API u32 dm_size;
extern RENDER_API u32 dm_slide_window_line;
extern RENDER_API u32 dm_cache_line;
extern RENDER_API u32 dm_cache_size;
extern RENDER_API float dm_fade;
extern RENDER_API u32 dm_current_size;
extern RENDER_API u32 dm_current_slide_window_line;
extern RENDER_API u32 dm_current_cache_line;
extern RENDER_API u32 dm_current_cache_size;
extern RENDER_API float dm_current_fade;
extern RENDER_API float ps_current_detail_density;

RENDER_API void bwdithermap(int levels, int magic[16][16]);

class light;
class CFrustum;

class RENDER_API CDetailManager
{
public:
	poolSS<CDetail::SlotItem, 4096> items_pool;

	struct SlotPart
	{
		u32 id;									// ID модельки
		xr_vector<CDetail::SlotItem*> items[3]; // список кустиков

		ICF SlotPart()
			: id(DetailSlot::ID_Empty) {}
	};
	enum SlotType
	{
		stReady = 0, // Ready to use
		stPending,	 // Pending for unpacking
		stFORCEDWORD = 0xffffffff
	};
	struct Slot
	{
		struct
		{
			u32 empty : 1;
			u32 type : 1;
		};
		DetailSlot* DS;
		vis_data vis;
		Fbox WindSampleBounds;
		SlotPart G[dm_obj_in_slot];
		bool has_trample;
		u32 trample_tick;
		float StreamingPriority = 0;
		u32 Generation = 0;

		ICF Slot()
			: empty(1), type(stReady), DS(nullptr), has_trample(false), trample_tick(0)
		{
			vis.clear();
			WindSampleBounds.invalidate();
		}
	};
	struct SlideSlot
	{
		u32 empty;
		vis_data vis;
		Slot** slots[dm_cache_count];
		ICF SlideSlot()
			: empty(1) { vis.clear(); }
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
	u32 WindPublishTime = 0;
	float WindInterpolationMs = 16;
	Fvector4 wave_dir1, wave_dir2, wave_dir1_old, wave_dir2_old;

	int dither[16][16];

	IReader* dtFS = nullptr;
	DetailHeader dtH;
	DetailSlot* dtSlots = nullptr; // note: pointer into VFS
	DetailSlot DS_empty;

	xr_atomic_bool task_finished = true;
	xr_unique_ptr<XRayWorkerThread> ProducerThread;
	XRayWorkerThread::Callback ProducerWork;
	xrSRWLock GeneratedFieldsAccess;
	enum class FieldEditKind
	{
		Height,
		Species,
		EraseHeight,
		EraseSpecies
	};
	struct FieldEdit
	{
		FieldEditKind Kind;
		float X, Z, Value, Strength;
	};
	xrCriticalSection FieldEditsAccess;
	xr_vector<FieldEdit> PendingFieldEdits;
	xr_vector<FieldEdit> ProcessingFieldEdits;
	xr_atomic_bool FieldEditsQueued = false;
	void QueueFieldEdit(FieldEditKind Kind, float X, float Z, float Value, float Strength);
	void ApplyFieldEdits();
	void WaitForDetails();
	void StopProducer();
	struct WindSlotRequest
	{
		Slot* Cell;
		u32 Generation;
		bool operator==(const WindSlotRequest&) const = default;
	};
	xr_vector<WindSlotRequest> WindUpdateSlots;
	xr_vector<WindSlotRequest> PreviousPreparedSlots;
	Fvector PreviousPreparedCamera = {};
	float PreviousPreparedFarDistance = -1;
	xr_vector<Fbox> WindSamplingBounds;
	u32 VisibilityFrame = u32(-1);
	u32 CompletedWindTick = 0;
	u32 RunningWindTick = 0;
	bool PreparedPacketValid = false;
	bool PreviousFarHadTrails = false;
	Fvector4 PreparedWindConstants[4] = {};
	Fvector4 PublishedWindConstants[4] = {};
	void PrepareWindInstances(bool SimulateWind, const Fvector& Camera, const CFrustum& View, bool EditorMode, float GeometryDistance, float WindDistance);
	void PublishDetailInstances();
	u32 alt_models_start = 0;
	u32 alt_models_count = 0;
	struct AlternativeModel
	{
		xr_string Path;
		u32 Species = 0;
	};
	struct AlternativeSpecies
	{
		xr_string Name;
		xr_vector<u8> Models;
	};
	xr_vector<AlternativeModel> AlternativeModels;
	xr_vector<AlternativeSpecies> AlternativeSpeciesGroups;
	u32 AlternativeCatalogCRC = 0;
	u32 GetClusterSpeciesCount() const;
	u32 GetClusterSpecies(u32 Model) const;

	u32 vanilla_grass_indices[64] = {};
	u32 vanilla_grass_count = 0;
	using DetailIt = xr_vector<CDetail*>::iterator;
	xr_vector<CDetail*> objects;
	CDB::COLLIDER xrc;

	xr_vector<xr_vector<SlideSlot>> slide_window;
	xr_vector<xr_vector<Slot*>> cache;
	xr_vector<Slot*> unpacked_slots;
	xr_vector<Slot> cache_pool;

	int cache_cx;
	int cache_cz;
	int CacheSlotRadius = 0;
	bool StreamingInitialized = false;
	Fvector StreamingPreviousView{};
	u32 StreamingPreviousTime = 0;

	// Baked categorical nodes on the native 2 m grid. Four shared nodes supply
	// each slot's continuous species mixture (0..254 = asset, 255 = unsuitable).
	// The auxiliary byte stores a repeatable blend sample / unpack seed; 255 protects a coverage support node.
	xr_vector<u8> cluster_field;
	xr_vector<u8> cluster_rnd_field;

	// Precomputed combined FMB height factor t (max over enabled layers of
	// mapped*power), one float per detail-DB slot CORNER (grid of
	// (size_x+1)*(size_z+1)), bilinearly sampled at instance position so
	// decompress does no per-instance noise calls.
	// Negative values are reserved for user "clear grass" strokes (t<0 => no grass).
	xr_vector<float> fmb_field;
	xr_vector<float> fmb_field_base; // snapshot before user masks overlay
	xr_vector<u8> cluster_field_base;
	xr_vector<u8> cluster_rnd_field_base;

	void BuildClusterField();
	void BuildClusterCoverage();
	void BuildClusterTransitions(float Spacing);
	u32 GetClusterFieldKey() const;
	u32 GetFMBFieldKey() const;
	u32 ClusterFieldKey = 0, FMBFieldKey = 0;
	bool ClusterFieldValid = false, FMBFieldValid = false;
	void BuildFMBField();
	float SampleFMBField(float world_x, float world_z) const;

	// Persist precomputed fields to $level$\detail_layers\ so a reload with unchanged
	// detail settings skips the noise rebuild. All I/O is best-effort: a read-only
	// level folder simply falls back to regenerating.
	bool DetailLayers_LoadFromBake(); // true => fields came from disk
	void DetailLayers_SaveToBake();
	void DetailLayers_ApplySettingsFromBake(); // restore r__detail_* to baked values
	bool m_detail_layers_baking = false;	   // true during level Load() only

	// User paint masks: sparse per-cell overrides layered ON TOP of the baked/generated
	// fields. Files live next to the bakes ($level$\detail_layers\) with a user_mask_
	// prefix and are re-applied on every cache_ReInitialize, so the generated bake stays
	// clean and the user strokes simply overwrite it where drawn.
	struct FMBMaskEntry
	{
		u32 idx;
		float value;
	};
	struct CLUMaskEntry
	{
		u32 idx;
		u8 value;
	};
	xr_vector<FMBMaskEntry> user_fmb_mask;
	xr_vector<CLUMaskEntry> user_clu_mask;

	void DetailLayers_LoadUserMasks();	  // read user_mask_*.bin, replace current
	void DetailLayers_SaveUserMasks();	  // write user_mask_*.bin
	void DetailLayers_ClearUserMasks();	  // wipe masks, restore base fields
	void DetailLayers_CommitBaseFields(); // snapshot working fields (pre-mask)
	void DetailLayers_ApplyUserMasks();	  // overlay sparse masks onto fields
	// Stroke helpers. strength in [0,1] is the shared brush intensity for both paint and
	// erase; the stroke blends from the current cell value toward the target (paint) or
	// toward the generated base (erase). A cell that lands back on the base value drops
	// its sparse entry entirely, so full erases leave no stale overrides.
	bool DetailLayers_PaintFMB(float world_x, float world_z, float value, float strength);
	bool DetailLayers_EraseFMB(float world_x, float world_z, float strength);
	bool DetailLayers_PaintCluster(float world_x, float world_z, u8 value, float strength);
	bool DetailLayers_EraseCluster(float world_x, float world_z, float strength);

	// Re-unpack ONLY the visible cache slots touched by the brush circle. Called per
	// stroke (throttled by the tool) so edits appear in real time without a full
	// cache_ReInitialize stall. Pending slots are left to the streaming pipeline.
	// Never call this directly from the render thread: cache_Update (and thus the pool
	// / unpacked_slots bookkeeping) runs on the vegetation producer, so the rebuild is
	// deferred through DetailLayers_RequestRebuildAround and consumed at the top of
	// cache_Update on that same worker. Direct cross-thread UnpackSlot/UnpackSlotItems
	// corrupted the items pool freelist and crashed.
	void DetailLayers_RebuildSlotsAround(const Fvector& center, float radius);
	void DetailLayers_RequestRebuildAround(const Fvector& center, float radius);

	// Brush-rebuild request published by the editor (render thread), consumed by the
	// vegetation producer. Fields are written before the release-store of the flag and
	// read after its acquire-load, so the plain members never race.
	std::atomic<bool> m_brush_rebuild_around{false};
	Fvector m_brush_rebuild_center{0.f, 0.f, 0.f};
	float m_brush_rebuild_radius = 0.f;

	// Recompute the slide window visibility on the next cache_Update even if the camera
	// stays in the same slot (editor edits that make slots empty or non-empty).
	std::atomic<bool> m_slide_window_dirty{false};
	void RequestSlideWindowUpdate() { m_slide_window_dirty.store(true, std::memory_order_release); }

	// Editor snap geometry (Level Editor overrides these; the game has no snap objects).
	virtual ObjectList* GetSnapList() { return nullptr; }
	// World-space triangles (three vertices each) of the snap objects intersecting Box.
	virtual void PickSnapTriangles(const Fbox& Box, xr_vector<Fvector>& Verts) {}

	// Deferred rebuild: console changes set a flag; cache_Update applies it once per frame
	// so a cfg_load (series of r__detail_* commands) rebuilds all slots a single time.
	void RequestCacheRebuild();
	bool ConsumeCacheRebuildRequest();

	void hw_Load();
	void hw_Unload();
	void hw_Render(light* L = NULL);


	Fvector TrampleLastCamPos;
	float TramplePlayerRadius = 1.5f;

	struct STramplePendingMark
	{
		float x, y, z, radius, weight;
	};
	xr_vector<STramplePendingMark> TramplePendingMarks;
	std::mutex TramplePendingLock;

	void TrampleMark(float x, float y, float z, float radius, float weight, bool isActor = false);
	void TrampleProcessMarks();
	void TrampleAnimateItems();
	xr_vector<u32> TrampleActiveSlots;

	struct TramplePresentationState
	{
		float Visual = 0;
		float Progress = 0;
		u32 Frame = u32(-1);
	};
	struct TramplePresentationEntry
	{
		Fvector4 Identity;
		Fvector4 State;
	};
	xr_map<DetailTramplePresentation::Key, TramplePresentationState> TramplePresentationStates;
	xr_vector<const CDetail::StaticItem*> TramplePresentationSources;
	xr_vector<TramplePresentationEntry> TramplePresentationTable;
	IRHIBuffer* TramplePresentationBuffer = nullptr;
	IRHIShaderResourceView* TramplePresentationView = nullptr;
	u32 TramplePresentationCapacity = 0;
	u32 TramplePresentationFrame = u32(-1);
	IRHIBuffer* PreparedTrampleBuffer = nullptr;
	IRHIShaderResourceView* PreparedTrampleView = nullptr;
	u32 PreparedTrampleCapacity = 0;
	void UpdateTramplePresentation();
	bool PrepareDetailBuffers(bool ReuseStatic);
	void ResetTramplePresentation();


	void hw_Render_dump(const Fvector4& wave, const Fvector4& wind, const Fvector4& wave_old, const Fvector4& wind_old, u32 var_id, u32 lod_id, light* L = NULL);

	// get unpacked slot
	DetailSlot& QueryDB(int sx, int sz);

	void cache_ReInitialize(u32 Size = dm_current_size, float Fade = dm_current_fade, float Density = ps_current_detail_density);
	void cache_Update(const Fvector& View, const Fvector& Direction, u32 FrameTime);
	void UnpackSlot(int gx, int gz, Slot* D);
	void UnpackSlotItems(Slot* D);

	// cache grid to world
	ICF int cg2w_X(int x) const { return cache_cx - CacheSlotRadius + x; }
	ICF int cg2w_Z(int z) const { return cache_cz - CacheSlotRadius + (dm_cache_line - 1 - z); }
	// world to cache grid
	ICF int w2cg_X(int x) const { return x - cache_cx + CacheSlotRadius; }
	ICF int w2cg_Z(int z) const { return cache_cz - CacheSlotRadius + (dm_cache_line - 1 - z); }

	void Load();
	void Unload();
	void Render();

	void cache_Alloc(u32 Size, float Fade, float Density);
	void cache_Free();

	virtual ~CDetailManager();
};
