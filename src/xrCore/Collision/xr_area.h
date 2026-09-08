#pragma once
#include "xr_collide_defs.h"
#include "../FormatParsers/LevelCForm/CFormIO.h"

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

	// Streaming (see XRay::CForm::CFormatStreamed / CFormatStreamedInstanced). Kept alive
	// after Load() only when the on-disk cform is a streamed format, so LoadStreamedTile()/
	// LoadStreamedSector() can be called on-demand at runtime.
	xr_unique_ptr<XRay::CForm::IFormat>	StreamingFormat;
	CDB::build_callback*				StreamingBuildCallback = nullptr;
	void*								StreamingBuildCallbackParams = nullptr;

	xr_hash_map<u64, CDB::MODEL*>		StreamedTileModels;
	xr_hash_map<u32, CDB::MODEL*>		StreamedSectorModels;
	xr_vector<CDB::MODEL*>				ActiveStreamedModels;	// flat cache of the two maps above, kept in sync

	static u64							EncodeTileKey		( s32 TileX, s32 TileZ );
	void								RebuildActiveStreamedModelsCache();

public:

#ifdef DEBUG
	FactoryPtr<IObjectSpaceRender>		*m_pRender;
#endif

public:
										CObjectSpace		( );
										~CObjectSpace		( );

	void								Load				(  CDB::build_callback build_callback  );
	void								Load				(  const char* initial, const char* fname, CDB::build_callback build_callback, bool NotFromLevel = false);
	//void								Load				(  IReader* R, CDB::build_callback build_callback  );
	//void								Create				(  Fvector*	verts, CDB::TRI* tris, const hdrCFORM &H, CDB::build_callback build_callback, void* pRW, bool RWMode);
	void								Create				(  const XRay::CForm::IFormat& Data, CDB::build_callback build_callback);
	
	// Occluded/No
	bool								RayTest				( const Fvector &start, const Fvector &dir, float range, collide::rq_target tgt, collide::ray_cache* cache, CObject* ignore_object);

	// Game raypick (nearest) - returns object and addititional params
	bool								RayPick				( const Fvector &start, const Fvector &dir, float range, collide::rq_target tgt, collide::rq_result& R, CObject* ignore_object );

	// General collision query
	bool								RayQuery			( collide::rq_results& dest, const collide::ray_defs& rq, collide::rq_callback* cb, LPVOID user_data, collide::test_callback* tb, CObject* ignore_object);
	bool								RayQuery			( collide::rq_results& dest, ICollisionForm* target, const collide::ray_defs& rq);

	//ICF xr_vector<CDB::TRI>&			GetStaticTris		() { return Static.get_tris();	}
	//ICF xr_vector<Fvector>&				GetStaticVerts		() { return Static.get_verts(); }
	ICF CDB::MODEL*						GetStaticModel		() { return &Static;			}
	CDB::MODEL* GetStaticStreamedTileModel(const Fvector& Location);

	ICF const Fbox&						GetBoundingVolume	() { return m_BoundingVolume;}

	//--------------------------------------------------------------------------------
	// Streaming collision API (low-level, must be called manually unless UpdateStreaming()
	// is used). See CDB::TRI::StreamedSectorID for the sector 0 (automatic) vs sector != 0
	// (manual, e.g. underground levels) distinction.
	//--------------------------------------------------------------------------------
	ICF bool							IsStreamingEnabled	() const { return StreamingFormat && StreamingFormat->IsStreamed(); }
	ICF float							GetStreamTileSize	() const { return StreamingFormat ? StreamingFormat->GetTileSize() : 0.f; }
	void								GetStreamedSectorIDs( xr_vector<u32>& OutIDs ) const;

	ICF const xr_vector<CDB::MODEL*>&	GetActiveStreamedModels() const { return ActiveStreamedModels; }

	bool								IsStreamedTileLoaded( s32 TileX, s32 TileZ ) const;
	bool								IsStreamedSectorLoaded( u32 SectorID ) const;

	// Loads/unloads a single N*N meter automatic-streaming (sector 0) tile. Returns false if
	// streaming isn't enabled for this level, the tile has no geometry, or it is already
	// loaded/unloaded.
	bool								LoadStreamedTile	( s32 TileX, s32 TileZ );
	bool								UnloadStreamedTile	( s32 TileX, s32 TileZ );

	// Loads/unloads a whole manually-streamed (sector != 0) chunk of collision geometry.
	bool								LoadStreamedSector	( u32 SectorID );
	bool								UnloadStreamedSector( u32 SectorID );

	void								UnloadAllStreamedTiles();

	// Simple automatic distance-based streaming hook for sector 0 tiles: loads every tile
	// within LoadRadius of ViewerPosition and unloads every currently-loaded tile further
	// than UnloadRadius away. Manually-streamed sectors (!=0) are never touched by this call.
	void								UpdateStreaming		( const Fvector& ViewerPosition, float LoadRadius, float UnloadRadius );

	// Debugging
#ifdef DEBUG
	void								dbgRender			();
#endif
};