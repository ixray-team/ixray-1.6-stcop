#pragma once
#include "xr_collide_defs.h"
#include "../FormatParsers/LevelCForm/CFormFormat.h"
#include "../FormatParsers/LevelCForm/CFormIO.h"
#include "../Kernel/xrSyncronize.h"

class ISpatial_DB;
XRCORE_API extern ISpatial_DB* g_SpatialSpace;
XRCORE_API extern ISpatial_DB* g_SpatialSpacePhysic;

class XRCORE_API CSpatialContextGuard
{
private:
	xrCriticalSectionGuard Lock;
	ISpatial_DB* PreviousSpatial;
	ISpatial_DB* PreviousPhysic;

public:
	CSpatialContextGuard(xrCriticalSection& mutex, ISpatial_DB* spatial, ISpatial_DB* physic);
	~CSpatialContextGuard();
};

// refs
class ISpatial;
using ISpatialShared = xr_shared_ptr<ISpatial>;

class 	ICollisionForm;
class 	CObject;

#include "../../Include/xrRender/FactoryPtr.h"
#include "../../Include/xrRender/ObjectSpaceRender.h"
#include "xrCDB.h"

//-----------------------------------------------------------------------------------------------------------
//Space Area
//-----------------------------------------------------------------------------------------------------------
//struct hdrCFORM;
class	XRCORE_API						CObjectSpace
{
private:
	// Debug
	CDB::MODEL							Static;
	Fbox								m_BoundingVolume;
	xr_unique_ptr<XRay::CForm::IFormat> StreamingFormat;
	CDB::build_callback* StreamingBuildCallback = nullptr;
	xr_hash_map<u64, xr_shared_ptr<CDB::MODEL>> StreamedTileModels;
	xr_hash_map<u32, xr_shared_ptr<CDB::MODEL>> StreamedSectorModels;
	mutable xrCriticalSection StreamingCS;
	u64 StreamingRevision = 0;
	static u64 EncodeTileKey(s32 X, s32 Z);
	ISpatial_DB* Spatial = nullptr;
	ISpatial_DB* SpatialPhysic = nullptr;
	ICF ISpatial_DB* SpatialDatabase() const { return Spatial != nullptr ? Spatial : g_SpatialSpace; }
	ICF ISpatial_DB* PhysicDatabase() const { return SpatialPhysic != nullptr ? SpatialPhysic : g_SpatialSpacePhysic; }
public:

#ifdef DEBUG
	FactoryPtr<IObjectSpaceRender>		*m_pRender;
#endif

public:
	void SetSpatialDatabases(ISpatial_DB* spatial, ISpatial_DB* physic) { Spatial = spatial; SpatialPhysic = physic; }
										CObjectSpace		( );
										~CObjectSpace		( );

	bool								Load				(  CDB::build_callback build_callback  ) { return Load("$level$", "level", build_callback); }
	bool								Load				(  const char* initial, const char* fname, CDB::build_callback build_callback, bool NotFromLevel = false, bool InitializeSpatialDB = true);
	// Initializes the spatial databases without any collision geometry.
	// Used when the level is loaded without collision data (missing file or masked out).
	void								CreateEmpty			(bool InitializeSpatialDB = true);
	//void								Load				(  IReader* R, CDB::build_callback build_callback  );
	//void								Create				(  Fvector*	verts, CDB::TRI* tris, const hdrCFORM &H, CDB::build_callback build_callback, void* pRW, bool RWMode);
	bool								Create				(  const XRay::CForm::IFormat& Data, CDB::build_callback build_callback, void* pRW, bool RWMode, bool InitializeSpatialDB = true);
	void								SwapStaticGeometry	(CObjectSpace& other);
	void								ClearStaticGeometry	();
	
	// Occluded/No
	bool								RayTest				( const Fvector &start, const Fvector &dir, float range, collide::rq_target tgt, collide::ray_cache* cache, CObject* ignore_object);

	// Game raypick (nearest) - returns object and addititional params
	bool								RayPick				( const Fvector &start, const Fvector &dir, float range, collide::rq_target tgt, collide::rq_result& R, CObject* ignore_object );

	// General collision query
	bool								RayQuery			( collide::rq_results& dest, const collide::ray_defs& rq, collide::rq_callback* cb, void* user_data, collide::test_callback* tb, CObject* ignore_object);
	bool								RayQuery			( collide::rq_results& dest, ICollisionForm* target, const collide::ray_defs& rq);

	ICF xr_vector<CDB::TRI>&			GetStaticTris		() { return Static.get_tris();	}
	ICF xr_vector<Fvector>&				GetStaticVerts		() { return Static.get_verts(); }
	ICF CDB::MODEL*						GetStaticModel		() { return &Static;			}

	// Change residency at frame boundaries, never from a collision callback. Snapshots
	// retain ownership while physics, sound or debug rendering traverse the models.
	bool IsStreamingEnabled() const { return StreamingFormat && StreamingFormat->IsStreamed(); }
	float GetStreamTileSize() const { return StreamingFormat ? StreamingFormat->GetTileSize() : 0.f; }
	void GetStreamedSectorIDs(xr_vector<u32>& OutIDs) const;
	xr_vector<xr_shared_ptr<CDB::MODEL>> GetActiveStreamedModels() const;
	xr_vector<xr_shared_ptr<CDB::MODEL>> GetStaticModels();
	xr_shared_ptr<CDB::MODEL> GetStaticStreamedTileModel(const Fvector& Location) const;
	u64 GetStreamingRevision() const;
	bool IsStreamedTileLoaded(s32 X, s32 Z) const;
	bool IsStreamedSectorLoaded(u32 SectorID) const;
	bool LoadStreamedTile(s32 X, s32 Z);
	bool UnloadStreamedTile(s32 X, s32 Z);
	bool LoadStreamedSector(u32 SectorID);
	bool UnloadStreamedSector(u32 SectorID);
	void UnloadAllStreamedTiles();
	class StreamingUpdate
	{
		friend class CObjectSpace;
		const CObjectSpace* Owner = nullptr;
		u64 Revision = 0;
		xr_hash_map<u64, xr_shared_ptr<CDB::MODEL>> TileModels;
	};
	// Preparation may run on a worker; publication stays at the simulation boundary.
	// The space and its immutable format must outlive preparation.
	xr_unique_ptr<StreamingUpdate> PrepareStreamingUpdate(const Fvector& ViewerPosition, float LoadRadius, float UnloadRadius) const;
	bool ApplyStreamingUpdate(xr_unique_ptr<StreamingUpdate> Update);
	void UpdateStreaming(const Fvector& ViewerPosition, float LoadRadius, float UnloadRadius);

	ICF const Fbox&						GetBoundingVolume	() { return m_BoundingVolume;}

	// Debugging
#ifdef DEBUG
	void								dbgRender			();
#endif
};
