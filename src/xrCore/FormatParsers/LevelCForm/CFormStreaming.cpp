#include "stdafx.h"
#include "CFormIO.h"

using namespace XRay;

namespace
{
u64 EncodeTileKey(s32 X, s32 Z)
{
	return (u64(u32(X)) << 32) | u64(u32(Z));
}

xr_string TileFileName(xr_string_view Base, s32 X, s32 Z)
{
	string64 Suffix;
	xr_sprintf(Suffix, "_tile_%d_%d", X, Z);
	return xr_string(Base.data()) + Suffix;
}

xr_string SectorFileName(xr_string_view Base, u32 Sector)
{
	string64 Suffix;
	xr_sprintf(Suffix, "_sector_%u", Sector);
	return xr_string(Base.data()) + Suffix;
}
} // namespace

CForm::CFormatStreamed::CFormatStreamed()
{
	Header.version = CFormVersions::Streamed;
	Header.aabb.invalidate();
}

CForm::CFormatStreamed::CFormatStreamed(float TileSize)
	: CFormatStreamed()
{
	SetTileSize(TileSize);
}

CForm::CFormatStreamedInstanced::CFormatStreamedInstanced()
{
	Header.version = CFormVersions::StreamedInstanced;
}

CForm::CFormatStreamedInstanced::CFormatStreamedInstanced(float TileSize)
	: CFormatStreamedInstanced()
{
	SetTileSize(TileSize);
}

void CForm::CFormatStreamed::SetTileSize(float TileSize)
{
	if (std::isfinite(TileSize) && TileSize > 0.f)
	{
		TileGridSize = TileSize;
	}
}

void CForm::CFormatStreamed::AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris)
{
	TileGeomMap.clear();
	SectorGeomMap.clear();
	Header.vertcount = u32(Verts.size());
	Header.facecount = u32(Tris.size());
	Header.aabb.invalidate();
	for (const auto& Vertex : Verts)
	{
		Header.aabb.modify(Vertex);
	}

	// A triangle belongs to every grid cell covered by its XZ bounding box, not just
	// the cells containing its vertices. This also handles large boundary triangles.
	for (const auto& Tri : Tris)
	{
		Fbox Bounds;
		Bounds.invalidate();
		for (u32 Index : Tri.verts)
		{
			Bounds.modify(Verts[Index]);
		}
		auto AddTriangle = [&](TileGeom& Bucket)
		{
			CDB::TRI Local = Tri;
			for (u32 Index = 0; Index < 3; ++Index)
			{
				const auto [It, Inserted] = Bucket.VertRemap.try_emplace(Tri.verts[Index], u32(Bucket.Verts.size()));
				if (Inserted)
				{
					Bucket.Verts.push_back(Verts[Tri.verts[Index]]);
				}
				Local.verts[Index] = It->second;
			}
			Bucket.Tris.push_back(Local);
		};
		if (Tri.StreamedSectorID != 0)
		{
			AddTriangle(SectorGeomMap[Tri.StreamedSectorID]);
		}
		else
		{
			for (s32 X = iFloor(Bounds.min.x / TileGridSize); X <= iFloor(Bounds.max.x / TileGridSize); ++X)
			{
				for (s32 Z = iFloor(Bounds.min.z / TileGridSize); Z <= iFloor(Bounds.max.z / TileGridSize); ++Z)
				{
					AddTriangle(TileGeomMap[EncodeTileKey(X, Z)]);
				}
			}
		}
	}
}

void CForm::CFormatStreamed::AddInstanceRef(shared_str Path, const Fmatrix& Transform, const Fbox& AABB, const CDB::MODEL& Model, u16 RenderSector, u32 StreamingSector)
{
	if (Header.version != CFormVersions::StreamedInstanced)
	{
		R_ASSERT(false);
		return;
	}
	InstanceData Instance{Path, Transform, AABB, &Model, RenderSector, StreamingSector};
	Header.aabb.merge(AABB);
	if (StreamingSector != 0)
	{
		SectorGeomMap[StreamingSector].Instances.push_back(Instance);
	}
	else
	{
		for (s32 X = iFloor(AABB.min.x / TileGridSize); X <= iFloor(AABB.max.x / TileGridSize); ++X)
		{
			for (s32 Z = iFloor(AABB.min.z / TileGridSize); Z <= iFloor(AABB.max.z / TileGridSize); ++Z)
			{
				TileGeomMap[EncodeTileKey(X, Z)].Instances.push_back(Instance);
			}
		}
	}
}

bool CForm::CFormatStreamed::WriteChunk(const xr_string& Path, TileGeom& Geometry) const
{
	xr_unique_ptr<IFormat> Format;
	if (Header.version == CFormVersions::StreamedInstanced)
	{
		Format.reset(new CFormatInstanced());
	}
	else
	{
		Format.reset(new CFormatVanilla());
	}
	Format->AddStaticGeom(Geometry.Verts, Geometry.Tris);
	for (const auto& Instance : Geometry.Instances)
	{
		Format->AddInstanceRef(Instance.Path, Instance.Transform, Instance.AABB, *Instance.Model, Instance.RenderSector, Instance.StreamingSector);
	}
	return Format->Write(Path.c_str());
}

bool CForm::CFormatStreamed::Write(xr_string_view FileName)
{
	BaseFileName = FileName.data();
	Tiles.clear();
	TileKeys.clear();
	Sectors.clear();
	for (auto& [Key, Geometry] : TileGeomMap)
	{
		s32 X = s32(u32(Key >> 32));
		s32 Z = s32(u32(Key));
		if (!WriteChunk(TileFileName(FileName, X, Z), Geometry))
		{
			return false;
		}
		Tiles.push_back({X, Z});
		TileKeys.insert(Key);
	}
	for (auto& [Sector, Geometry] : SectorGeomMap)
	{
		if (!WriteChunk(SectorFileName(FileName, Sector), Geometry))
		{
			return false;
		}
		Sectors.push_back(Sector);
	}
	const xr_string Path = BaseFileName + ".cform";
	auto Writer = FS.wg_open(Path.c_str());
	if (!Writer)
	{
		return false;
	}
	Writer->w(&Header, sizeof(Header));
	Writer->w_float(TileGridSize);
	Writer->w_u32(u32(Tiles.size()));
	for (const auto& Tile : Tiles)
	{
		Writer->w_s32(Tile.X);
		Writer->w_s32(Tile.Z);
	}
	Writer->w_u32(u32(Sectors.size()));
	for (u32 Sector : Sectors)
	{
		Writer->w_u32(Sector);
	}
	return true;
}

bool CForm::CFormatStreamed::Read(xr_string_view FileName)
{
	BaseFileName = FileName.data();
	const xr_string Path = BaseFileName + ".cform";
	auto Reader = FS.rg_open(Path.c_str());
	if (!Reader || Reader->length() < sizeof(Header) + sizeof(float) + 2 * sizeof(u32))
	{
		return false;
	}
	FileHash = crc32(Reader->pointer(), Reader->length());
	const auto ExpectedVersion = Header.version;
	Reader->r(&Header, sizeof(Header));
	if (Header.version != ExpectedVersion)
	{
		return false;
	}
	TileGridSize = Reader->r_float();
	if (!std::isfinite(TileGridSize) || TileGridSize <= 0.f)
	{
		return false;
	}
	u32 Count = Reader->r_u32();
	if (Count > (Reader->elapsed() - sizeof(u32)) / (2 * sizeof(s32)))
	{
		return false;
	}
	Tiles.resize(Count);
	TileKeys.clear();
	for (auto& Tile : Tiles)
	{
		Tile.X = Reader->r_s32();
		Tile.Z = Reader->r_s32();
		TileKeys.insert(EncodeTileKey(Tile.X, Tile.Z));
	}
	Count = Reader->r_u32();
	if (Count > Reader->elapsed() / sizeof(u32))
	{
		return false;
	}
	Sectors.resize(Count);
	for (auto& Sector : Sectors)
	{
		Sector = Reader->r_u32();
		if (Sector == 0)
		{
			return false;
		}
	}
	return true;
}

bool CForm::CFormatStreamed::LoadChunk(const xr_string& Path, CDB::MODEL& OutModel, CDB::build_callback* Callback, void* Params) const
{
	xr_string CheckPath = Path + ".cform";
	xr_strlwr(CheckPath.data());
	if (!FS.exist(CheckPath.c_str()))
	{
		Msg("! Missing streamed collision chunk: %s.cform", Path.c_str());
		return false;
	}
	auto Format = CForm::Read(Path.c_str());
	const auto ExpectedVersion = Header.version == CFormVersions::StreamedInstanced ? CFormVersions::Instanced : CFormVersions::Vanilla;
	if (!Format || Format->GetHeader().version != ExpectedVersion)
	{
		return false;
	}
	Format->ReadData(OutModel, Callback, Params);
	// Parsers own their input buffers. Complete all builds before releasing them or
	// publishing the model to collision queries.
	OutModel.load_task.wait();
	return OutModel.IsBuilt;
}

bool CForm::CFormatStreamed::HasStreamedTile(s32 X, s32 Z) const
{
	return TileKeys.contains(EncodeTileKey(X, Z));
}

bool CForm::CFormatStreamed::LoadStreamedTile(s32 X, s32 Z, CDB::MODEL& Model, CDB::build_callback* Callback, void* Params) const
{
	if (!HasStreamedTile(X, Z))
	{
		return false;
	}
	return LoadChunk(TileFileName(BaseFileName.c_str(), X, Z), Model, Callback, Params);
}

bool CForm::CFormatStreamed::LoadStreamedSector(u32 SectorID, CDB::MODEL& Model, CDB::build_callback* Callback, void* Params) const
{
	if (std::find(Sectors.begin(), Sectors.end(), SectorID) == Sectors.end())
	{
		return false;
	}
	return LoadChunk(SectorFileName(BaseFileName.c_str(), SectorID), Model, Callback, Params);
}

void CForm::CFormatStreamed::GetStaticGeom(xr_vector<Fvector>& Verts, xr_vector<CDB::TRI>& Tris) const
{
	// A streamed master deliberately does not contain a whole-level mesh.
	R_ASSERT(false);
	Verts.clear();
	Tris.clear();
}

void CForm::CFormatStreamed::ReadData(CDB::MODEL& Model, CDB::build_callback* Callback, void* Params) const
{
	R_ASSERT(false);
}
