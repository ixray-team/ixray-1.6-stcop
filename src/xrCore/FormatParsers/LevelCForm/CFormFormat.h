#pragma once

#include "CFormTypes.h"
#include "../../Collision/xrCDB.h"

namespace XRay::CForm
{
	using ChunkHeader = hdrCFORM;

	class XRCORE_API IFormat
	{
	protected:
		ChunkHeader Header = {};
		u32 FileHash = 0;

	public:
		virtual ~IFormat() = default;

		virtual bool Write(xr_string_view FileName) = 0;
		virtual bool Read(xr_string_view FileName) = 0;

		virtual void AddStaticGeom(xr_span<Fvector> Vertices, xr_span<CDB::TRI> Tris) = 0;
		virtual void AddInstanceRef(shared_str Path, const Fmatrix& Transform, const Fbox& AABB, const CDB::MODEL& Collision, u16 Sector, u32 StreamingSector = 0) = 0;
		virtual void GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const = 0;
		virtual void ReadData(CDB::MODEL& Model, CDB::build_callback* Callback = nullptr, void* Params = nullptr) const = 0;
		virtual bool IsStreamed() const { return false; }
		virtual float GetTileSize() const { return 0.f; }
		virtual bool HasStreamedTile(s32 TileX, s32 TileZ) const { return false; }
		virtual bool LoadStreamedTile(s32 TileX, s32 TileZ, CDB::MODEL& OutModel, CDB::build_callback* Callback = nullptr, void* Params = nullptr) const { return false; }
		virtual bool LoadStreamedSector(u32 SectorID, CDB::MODEL& OutModel, CDB::build_callback* Callback = nullptr, void* Params = nullptr) const { return false; }
		virtual void GetStreamedSectorIDs(xr_vector<u32>& OutIDs) const { OutIDs.clear(); }

		ChunkHeader& GetHeader();
		const ChunkHeader& GetHeader() const;

		u32 GetFileHash() const;
	};
}
