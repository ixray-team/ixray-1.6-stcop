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
#ifdef DEBUG
CObjectSpace::~CObjectSpace()
{
	xr_delete(m_pRender);
}
#else
CObjectSpace::~CObjectSpace() = default;
#endif
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
	Create(*CFormData, build_callback);
	if (CFormData->IsStreamed())
	{
		StreamingFormat = std::move(CFormData);
		StreamingBuildCallback = build_callback;
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

u64 CObjectSpace::EncodeTileKey(s32 X, s32 Z)
{
	return (u64(u32(X)) << 32) | u64(u32(Z));
}

void CObjectSpace::GetStreamedSectorIDs(xr_vector<u32>& OutIDs) const
{
	OutIDs.clear();
	if (StreamingFormat)
	{
		StreamingFormat->GetStreamedSectorIDs(OutIDs);
	}
}

xr_vector<xr_shared_ptr<CDB::MODEL>> CObjectSpace::GetActiveStreamedModels() const
{
	xrCriticalSectionGuard Guard(StreamingCS);
	xr_vector<xr_shared_ptr<CDB::MODEL>> Models;
	Models.reserve(StreamedTileModels.size() + StreamedSectorModels.size());
	for (const auto& [Key, Model] : StreamedTileModels)
	{
		Models.push_back(Model);
	}
	for (const auto& [Sector, Model] : StreamedSectorModels)
	{
		Models.push_back(Model);
	}
	return Models;
}

xr_vector<xr_shared_ptr<CDB::MODEL>> CObjectSpace::GetStaticModels()
{
	if (IsStreamingEnabled())
	{
		return GetActiveStreamedModels();
	}
	return {xr_shared_ptr<CDB::MODEL>(&Static, [](CDB::MODEL*) {})};
}

xr_shared_ptr<CDB::MODEL> CObjectSpace::GetStaticStreamedTileModel(const Fvector& Location) const
{
	const float TileSize = GetStreamTileSize();
	if (TileSize <= 0.f)
	{
		return {};
	}
	xrCriticalSectionGuard Guard(StreamingCS);
	const auto It = StreamedTileModels.find(EncodeTileKey(iFloor(Location.x / TileSize), iFloor(Location.z / TileSize)));
	return It == StreamedTileModels.end() ? xr_shared_ptr<CDB::MODEL>{} : It->second;
}

u64 CObjectSpace::GetStreamingRevision() const
{
	xrCriticalSectionGuard Guard(StreamingCS);
	return StreamingRevision;
}

bool CObjectSpace::IsStreamedTileLoaded(s32 X, s32 Z) const
{
	xrCriticalSectionGuard Guard(StreamingCS);
	return StreamedTileModels.contains(EncodeTileKey(X, Z));
}

bool CObjectSpace::IsStreamedSectorLoaded(u32 SectorID) const
{
	xrCriticalSectionGuard Guard(StreamingCS);
	return StreamedSectorModels.contains(SectorID);
}

bool CObjectSpace::LoadStreamedTile(s32 X, s32 Z)
{
	if (!IsStreamingEnabled() || !StreamingFormat->HasStreamedTile(X, Z) || IsStreamedTileLoaded(X, Z))
	{
		return false;
	}
	xr_shared_ptr<CDB::MODEL> Model(new CDB::MODEL());
	if (!StreamingFormat->LoadStreamedTile(X, Z, *Model, StreamingBuildCallback, nullptr))
	{
		return false;
	}
	xrCriticalSectionGuard Guard(StreamingCS);
	if (!StreamedTileModels.emplace(EncodeTileKey(X, Z), std::move(Model)).second)
	{
		return false;
	}
	++StreamingRevision;
	return true;
}

bool CObjectSpace::UnloadStreamedTile(s32 X, s32 Z)
{
	xrCriticalSectionGuard Guard(StreamingCS);
	if (!StreamedTileModels.erase(EncodeTileKey(X, Z)))
	{
		return false;
	}
	++StreamingRevision;
	return true;
}

bool CObjectSpace::LoadStreamedSector(u32 SectorID)
{
	if (!IsStreamingEnabled() || SectorID == 0 || IsStreamedSectorLoaded(SectorID))
	{
		return false;
	}
	xr_shared_ptr<CDB::MODEL> Model(new CDB::MODEL());
	if (!StreamingFormat->LoadStreamedSector(SectorID, *Model, StreamingBuildCallback, nullptr))
	{
		return false;
	}
	xrCriticalSectionGuard Guard(StreamingCS);
	if (!StreamedSectorModels.emplace(SectorID, std::move(Model)).second)
	{
		return false;
	}
	++StreamingRevision;
	return true;
}

bool CObjectSpace::UnloadStreamedSector(u32 SectorID)
{
	xrCriticalSectionGuard Guard(StreamingCS);
	if (!StreamedSectorModels.erase(SectorID))
	{
		return false;
	}
	++StreamingRevision;
	return true;
}

void CObjectSpace::UnloadAllStreamedTiles()
{
	xrCriticalSectionGuard Guard(StreamingCS);
	if (!StreamedTileModels.empty())
	{
		StreamedTileModels.clear();
		++StreamingRevision;
	}
}

xr_unique_ptr<CObjectSpace::StreamingUpdate> CObjectSpace::PrepareStreamingUpdate(const Fvector& ViewerPosition, float LoadRadius, float UnloadRadius) const
{
	if (!IsStreamingEnabled() || !std::isfinite(ViewerPosition.x) || !std::isfinite(ViewerPosition.z) || !std::isfinite(LoadRadius) || !std::isfinite(UnloadRadius) || LoadRadius < 0.f)
	{
		return {};
	}
	PROF_EVENT("CObjectSpace::PrepareStreamingUpdate");
	xr_unique_ptr<StreamingUpdate> Update(new StreamingUpdate());
	Update->Owner = this;
	{
		xrCriticalSectionGuard Guard(StreamingCS);
		Update->Revision = StreamingRevision;
		Update->TileModels = StreamedTileModels;
	}
	UnloadRadius = std::max(LoadRadius, UnloadRadius);
	const float TileSize = GetStreamTileSize();
	const s32 CenterX = iFloor(ViewerPosition.x / TileSize);
	const s32 CenterZ = iFloor(ViewerPosition.z / TileSize);
	const s32 Radius = iCeil(LoadRadius / TileSize) + 1;
	auto DistanceSquared = [&](s32 X, s32 Z)
	{
		// Distance to the tile rectangle, not its center: the current tile must
		// always be resident, including when the camera stands on a grid boundary.
		const float DX = std::max(std::max(X * TileSize - ViewerPosition.x, 0.f), ViewerPosition.x - (X + 1.f) * TileSize);
		const float DZ = std::max(std::max(Z * TileSize - ViewerPosition.z, 0.f), ViewerPosition.z - (Z + 1.f) * TileSize);
		return DX * DX + DZ * DZ;
	};
	for (s32 X = CenterX - Radius; X <= CenterX + Radius; ++X)
	{
		for (s32 Z = CenterZ - Radius; Z <= CenterZ + Radius; ++Z)
		{
			if (DistanceSquared(X, Z) <= LoadRadius * LoadRadius)
			{
				const u64 Key = EncodeTileKey(X, Z);
				if (!Update->TileModels.contains(Key) && StreamingFormat->HasStreamedTile(X, Z))
				{
					xr_shared_ptr<CDB::MODEL> Model(new CDB::MODEL());
					if (StreamingFormat->LoadStreamedTile(X, Z, *Model, StreamingBuildCallback, nullptr))
					{
						Update->TileModels.emplace(Key, std::move(Model));
					}
				}
			}
		}
	}
	for (auto It = Update->TileModels.begin(); It != Update->TileModels.end();)
	{
		if (DistanceSquared(s32(u32(It->first >> 32)), s32(u32(It->first))) > UnloadRadius * UnloadRadius)
		{
			It = Update->TileModels.erase(It);
		}
		else
		{
			++It;
		}
	}
	return Update;
}

bool CObjectSpace::ApplyStreamingUpdate(xr_unique_ptr<StreamingUpdate> Update)
{
	if (!Update || Update->Owner != this)
	{
		return false;
	}
	PROF_EVENT("CObjectSpace::ApplyStreamingUpdate");
	xrCriticalSectionGuard Guard(StreamingCS);
	// A manual residency change made while loading must not be undone by an old task.
	if (Update->Revision != StreamingRevision)
	{
		return false;
	}
	if (StreamedTileModels != Update->TileModels)
	{
		StreamedTileModels.swap(Update->TileModels);
		++StreamingRevision;
	}
	return true;
}

void CObjectSpace::UpdateStreaming(const Fvector& ViewerPosition, float LoadRadius, float UnloadRadius)
{
	ApplyStreamingUpdate(PrepareStreamingUpdate(ViewerPosition, LoadRadius, UnloadRadius));
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
