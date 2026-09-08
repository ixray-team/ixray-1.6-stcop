#include "stdafx.h"

#include "xr_area.h"
#define ENGINE_API
#include "../xrEngine/xr_object.h"
//#include "../xrEngine/xrLevel.h"
#include "../xrEngine/xr_collide_form.h"
#include "FormatParsers/LevelGeom/GeomIO.h"
#include "override/Model.h"

using namespace	collide;

//----------------------------------------------------------------------
// Class	: CObjectSpace
// Purpose	: stores space slots
//----------------------------------------------------------------------
CObjectSpace::CObjectSpace()
#ifdef DEBUG
	: m_pRender(nullptr)
#endif
{
#ifdef DEBUG
	if (RenderFactory)
		m_pRender = new FactoryPtr<IObjectSpaceRender>();
#endif
	m_BoundingVolume.invalidate();
}

//----------------------------------------------------------------------
CObjectSpace::~CObjectSpace()
{
	for (auto& [Key, Model] : StreamedTileModels)
	{
		xr_delete(Model);
	}
	for (auto& [SectorID, Model] : StreamedSectorModels)
	{
		xr_delete(Model);
	}

#ifdef DEBUG
	xr_delete(m_pRender);
#endif
}
//----------------------------------------------------------------------

//----------------------------------------------------------------------
void CObjectSpace::Load(CDB::build_callback build_callback)
{
	
	Load("$level$", "level", build_callback);
}

void CObjectSpace::Load(const char* initial, const char* fname, CDB::build_callback build_callback, bool NotFromLevel)
{
	xr_string Filename;
	xr_unique_ptr<XRay::CForm::IFormat> CFormData;
	{
		PROF_EVENT("CObjectSpace::Load");
		CFormData = XRay::CForm::Read(initial, fname);
		if (!I_ASSERT(CFormData))
		{
			FATAL("Attempt to load level with invalid collision data!");
			return;
		}
	}
		
	PROF_EVENT("CObjectSpace::Create");
	bool IsStreamedFormat = CFormData->IsStreamed();
	Create(*CFormData, build_callback);

	// Streamed formats don't have their static mesh eagerly loaded via ReadData() - the
	// format instance needs to stay alive so LoadStreamedTile()/LoadStreamedSector() can
	// keep pulling geometry from it on demand.
	if (IsStreamedFormat)
	{
		StreamingFormat = std::move(CFormData);
		StreamingBuildCallback = build_callback;
		StreamingBuildCallbackParams = nullptr;
	}
}

void CObjectSpace::Create(const XRay::CForm::IFormat& Data, CDB::build_callback build_callback)
{
	if (!Data.IsStreamed())
	{
		PROF_EVENT("CObjectSpace::Create::Data.ReadData");
		Data.ReadData(Static, build_callback, nullptr);
	}
	{
		PROF_EVENT("CObjectSpace::Create::m_BoundingVolume");
		m_BoundingVolume.set(Data.GetHeader().aabb);
	}
	{
		PROF_EVENT("CObjectSpace::Create::g_SpatialSpace");
		g_SpatialSpace->initialize(m_BoundingVolume);
	}
	{
		PROF_EVENT("CObjectSpace::Create::g_SpatialSpacePhysic");
		g_SpatialSpacePhysic->initialize(m_BoundingVolume);
	}
}

//----------------------------------------------------------------------
// Streaming collision API
//----------------------------------------------------------------------
u64 CObjectSpace::EncodeTileKey(s32 TileX, s32 TileZ)
{
	return (u64(u32(TileX)) << 32) | u64(u32(TileZ));
}

void CObjectSpace::RebuildActiveStreamedModelsCache()
{
	PROF_EVENT("CObjectSpace::RebuildActiveStreamedModelsCache");
	ActiveStreamedModels.clear();
	ActiveStreamedModels.reserve(StreamedTileModels.size() + StreamedSectorModels.size());
	for (auto& [Key, Model] : StreamedTileModels)
	{
		ActiveStreamedModels.push_back(Model);
	}
	for (auto& [SectorID, Model] : StreamedSectorModels)
	{
		ActiveStreamedModels.push_back(Model);
	}
}

void CObjectSpace::GetStreamedSectorIDs(xr_vector<u32>& OutIDs) const
{
	OutIDs.clear();
	if (StreamingFormat)
	{
		StreamingFormat->GetStreamedSectorIDs(OutIDs);
	}
}

bool CObjectSpace::IsStreamedTileLoaded(s32 TileX, s32 TileZ) const
{
	return StreamedTileModels.find(EncodeTileKey(TileX, TileZ)) != StreamedTileModels.end();
}

bool CObjectSpace::IsStreamedSectorLoaded(u32 SectorID) const
{
	return StreamedSectorModels.find(SectorID) != StreamedSectorModels.end();
}

bool CObjectSpace::LoadStreamedTile(s32 TileX, s32 TileZ)
{
	if (!IVERIFY(IsStreamingEnabled()))
	{
		return false;
	}

	u64 Key = EncodeTileKey(TileX, TileZ);
	if (StreamedTileModels.find(Key) != StreamedTileModels.end())
	{
		return false;
	}

	auto Model = new CDB::MODEL();
	if (!StreamingFormat->LoadStreamedTile(TileX, TileZ, *Model, StreamingBuildCallback, StreamingBuildCallbackParams))
	{
		xr_delete(Model);
		return false;
	}

	StreamedTileModels.emplace(Key, Model);
	RebuildActiveStreamedModelsCache();
	return true;
}

bool CObjectSpace::UnloadStreamedTile(s32 TileX, s32 TileZ)
{
	u64 Key = EncodeTileKey(TileX, TileZ);
	auto It = StreamedTileModels.find(Key);
	if (It == StreamedTileModels.end())
	{
		return false;
	}

	xr_delete(It->second);
	StreamedTileModels.erase(It);
	RebuildActiveStreamedModelsCache();
	return true;
}

bool CObjectSpace::LoadStreamedSector(u32 SectorID)
{
	if (!IVERIFY(IsStreamingEnabled()) || !IVERIFY(SectorID != 0))
	{
		return false;
	}

	if (StreamedSectorModels.find(SectorID) != StreamedSectorModels.end())
	{
		return false;
	}

	auto Model = new CDB::MODEL();
	if (!StreamingFormat->LoadStreamedSector(SectorID, *Model, StreamingBuildCallback, StreamingBuildCallbackParams))
	{
		xr_delete(Model);
		return false;
	}

	StreamedSectorModels.emplace(SectorID, Model);
	RebuildActiveStreamedModelsCache();
	return true;
}

bool CObjectSpace::UnloadStreamedSector(u32 SectorID)
{
	auto It = StreamedSectorModels.find(SectorID);
	if (It == StreamedSectorModels.end())
	{
		return false;
	}

	xr_delete(It->second);
	StreamedSectorModels.erase(It);
	RebuildActiveStreamedModelsCache();
	return true;
}

void CObjectSpace::UnloadAllStreamedTiles()
{
	for (auto& [Key, Model] : StreamedTileModels)
	{
		xr_delete(Model);
	}
	StreamedTileModels.clear();
	RebuildActiveStreamedModelsCache();
}

void CObjectSpace::UpdateStreaming(const Fvector& ViewerPosition, float LoadRadius, float UnloadRadius)
{
	if (!IsStreamingEnabled())
	{
		return;
	}

	float TileSize = GetStreamTileSize();
	if (!IVERIFY(TileSize > 0.f))
	{
		return;
	}

	s32 CenterX = iFloor(ViewerPosition.x / TileSize);
	s32 CenterZ = iFloor(ViewerPosition.z / TileSize);
	s32 TileRadius = iCeil(LoadRadius / TileSize) + 1;

	for (s32 dx = -TileRadius; dx <= TileRadius; ++dx)
	{
		for (s32 dz = -TileRadius; dz <= TileRadius; ++dz)
		{
			s32 TileX = CenterX + dx;
			s32 TileZ = CenterZ + dz;

			Fvector TileCenter(
				(float(TileX) + 0.5f) * TileSize,
				ViewerPosition.y,
				(float(TileZ) + 0.5f) * TileSize
			);

			if (TileCenter.distance_to_xz(ViewerPosition) <= LoadRadius)
			{
				PROF_EVENT("CObjectSpace::UpdateStreaming::Load");
				LoadStreamedTile(TileX, TileZ);
			}
		}
	}

	for (auto It = StreamedTileModels.begin(); It != StreamedTileModels.end();)
	{
		s32 TileX = s32(u32(It->first >> 32));
		s32 TileZ = s32(u32(It->first & 0xFFFFFFFFu));

		Fvector TileCenter(
			(float(TileX) + 0.5f) * TileSize,
			ViewerPosition.y,
			(float(TileZ) + 0.5f) * TileSize
		);

		if (TileCenter.distance_to_xz(ViewerPosition) > UnloadRadius)
		{
			PROF_EVENT("CObjectSpace::UpdateStreaming::Free");
			xr_delete(It->second);
			It = StreamedTileModels.erase(It);
		}
		else
		{
			++It;
		}
	}

	RebuildActiveStreamedModelsCache();
}

CDB::MODEL* CObjectSpace::GetStaticStreamedTileModel(const Fvector& Location)
{
	float TileSize = GetStreamTileSize();
	if (!IVERIFY(TileSize > 0.f))
	{
		return nullptr;
	}

	s32 CenterX = iFloor(Location.x / TileSize);
	s32 CenterZ = iFloor(Location.z / TileSize);
	
	if (!IsStreamedTileLoaded(CenterX, CenterZ))
	{
		return nullptr;
	}
	
	return StreamedTileModels.find(EncodeTileKey(CenterX, CenterZ))->second;
}

//----------------------------------------------------------------------
#ifdef DEBUG
void CObjectSpace::dbgRender()
{
	(*m_pRender)->dbgRender();
}
/*
void CObjectSpace::dbgRender()
{
	R_ASSERT(bDebug);

	RCache.set_Shader(sh_debug);
	for (u32 i=0; i<q_debug.boxes.size(); i++)
	{
		Fobb&		obb		= q_debug.boxes[i];
		Fmatrix		X,S,R;
		obb.xform_get(X);
		RCache.dbg_DrawOBB(X,obb.m_halfsize,color_xrgb(255,0,0));
		S.scale		(obb.m_halfsize);
		R.mul		(X,S);
		RCache.dbg_DrawEllipse(R,color_xrgb(0,0,255));
	}
	q_debug.boxes.clear();

	for (i=0; i<dbg_S.size(); i++)
	{
		std::pair<Fsphere,u32>& P = dbg_S[i];
		Fsphere&	S = P.first;
		Fmatrix		M;
		M.scale		(S.R,S.R,S.R);
		M.translate_over(S.P);
		RCache.dbg_DrawEllipse(M,P.second);
	}
	dbg_S.clear();
}
*/
#endif
