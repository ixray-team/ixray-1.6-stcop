#include "stdafx.h"
#include "CFormIO.h"

#include "API/xrAPI.h"
#include "xrEngine/Render.h"

using namespace XRay;

CForm::ChunkHeader& CForm::IFormat::GetHeader()
{
    return Header;
}

const CForm::ChunkHeader& CForm::IFormat::GetHeader() const
{
    return Header;
}

u32 CForm::IFormat::GetFileHash() const
{
    return FileHash;
}

CForm::CFormatVanilla::CFormatVanilla()
{
    Header.version = CFormVersions::Vanilla;
}

CForm::CFormatVanilla::~CFormatVanilla()
{
}

bool CForm::CFormatVanilla::Write(xr_string_view FileName)
{
    xr_stack_string_path Path = FileName.data();
    Path.append(".cform");
    
    auto Writer = FS.wg_open(Path.c_str());
    if (!I_ASSERT(Writer))
    {
        return false;
    }

    Writer->w(&Header, sizeof(Header));
    Writer->w(VertsPtr, Header.vertcount*sizeof(Fvector));
	for (int i = 0; i < Header.facecount; ++i)
	{
		Writer->w(TrisPtr+i, sizeof(CDB::TRI_Vanilla));
	}
    //Writer->w(TrisPtr, Header.facecount*sizeof(CDB::TRI));
    
    return true;
}

bool CForm::CFormatVanilla::Read(xr_string_view FileName)
{
    xr_stack_string_path Path = FileName.data();
    Path.append(".cform");

    FileReader = FS.rg_open(Path.c_str());
    if (!I_ASSERT_M(FileReader, "Unable to open file [%s]", Path.c_str()))
    {
        return false;
    }

    FileHash = crc32(FileReader->pointer(), FileReader->length());
    
    FileReader->r(&Header, sizeof(Header));
    if (!I_ASSERT(Header.version == CFormVersions::Vanilla || Header.version == CFormVersions::VanillaChunkedData))
    {
        return false;
    }
	VertsPtr = (Fvector*)FileReader->pointer();
	FileReader->advance(Header.vertcount*sizeof(Fvector));
	TrisPtr = (CDB::TRI*)FileReader->pointer();

    return true;
}

void CForm::CFormatVanilla::AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris)
{
    Header.vertcount = Verts.size();
    Header.facecount = Tris.size();
    Header.aabb.invalidate();
    for (auto& elem : Verts)
    {
        Header.aabb.modify(elem);
    }
	VertsPtr = Verts.data();
	TrisPtr = Tris.data();
}

void CForm::CFormatVanilla::GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const
{
    OutVertices.clear();
    OutTris.clear();
    OutVertices.resize(Header.vertcount);
    OutTris.resize(Header.facecount);
    std::memcpy(OutVertices.data(), VertsPtr, sizeof(Fvector) * OutVertices.size());
	for (u32 i = 0; i < Header.facecount; ++i)
	{
		std::memcpy(OutTris.data()+i, ((CDB::TRI_Vanilla*)TrisPtr)+i, sizeof(CDB::TRI_Vanilla));
		OutTris[i].StreamedSectorID = 0;
	}
}

void CForm::CFormatVanilla::ReadData(CDB::MODEL& Model, CDB::build_callback* bc, void* bcp) const
{
	Model.verts.resize(Header.vertcount);
	std::memcpy(Model.verts.data(), VertsPtr, sizeof(Fvector) * Header.vertcount);
	Model.tris.resize(Header.facecount);
	for (u32 i = 0; i < Header.facecount; ++i)
	{
		std::memcpy(Model.tris.data()+i, ((CDB::TRI_Vanilla*)TrisPtr)+i, sizeof(CDB::TRI_Vanilla));
		Model.tris[i].StreamedSectorID = 0;
	}
	//std::memcpy(Model.tris.data(), TrisPtr, sizeof(CDB::TRI) * Header.facecount);
	
	if (bc)
	{
		bc(Model.verts.data(), Header.vertcount, Model.tris.data(), Header.facecount, bcp);
	}

	Model.build_simple();
}

CForm::CFormatVanillaChunked::CFormatVanillaChunked(u32 ChunkNumber)
{
    if (!IVERIFY(ChunkNumber > 0)){
        ChunkNumber = 1;
    }
    Header.version = CFormVersions::VanillaChunked;
    Data.shrink_to_fit();
    Data.resize(ChunkNumber);
    for (auto& elem : Data)
    {
        elem.GetHeader().version = CFormVersions::VanillaChunkedData;
    }
}

bool CForm::CFormatVanillaChunked::Write(xr_string_view FileName)
{
    xr_stack_string_path Path = FileName.data();
    Path.append(".cform");
    auto Writer = FS.wg_open(Path.c_str());
    if (!I_ASSERT(Writer))
    {
        return false;
    }

    Writer->w(&Header, sizeof(Header));
    Writer->w_u32(Data.size());

    for (size_t i = 0; i < Data.size(); i++)
    {
        auto& elem = Data[i];
        xr_stack_string_path Path = FileName.data();
        Path.append("_");
        Path.append(std::to_string(i).c_str());
        if (!I_ASSERT(elem.Write({Path.c_str(), Path.size()})))
        {
            return false;
        }
    }

    return true;
}

bool CForm::CFormatVanillaChunked::Read(xr_string_view FileName)
{
    xr_stack_string_path Path = FileName.data();
    Path.append(".cform");
    auto Reader = FS.rg_open(Path.c_str());
    if (!I_ASSERT(Reader))
    {
        return false;
    }

    FileHash = crc32(Reader->pointer(), Reader->length());
    
    Reader->r(&Header, sizeof(Header));
    if (!I_ASSERT(Header.version == CFormVersions::VanillaChunked))
    {
        return false;
    }

    u32 ChunkNum = Reader->r_u32();
    Data.resize(ChunkNum);
    for (u32 i = 0; i < ChunkNum; i++)
    {
        auto& elem = Data[i];
        xr_stack_string_path Path = FileName.data();
        Path.append("_");
        Path.append(std::to_string(i).c_str());
        if (!I_ASSERT(elem.Read({Path.c_str(), Path.size()})))
        {
            return false;
        }
    }

    return true;
    
}

void CForm::CFormatVanillaChunked::AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris)
{
    auto ChunksNum = Data.size();
    Header.vertcount = Verts.size();
    Header.facecount = Tris.size();
    Header.aabb.invalidate();
    for (auto& elem : Verts)
    {
        Header.aabb.modify(elem);
    }

    auto PerChunkVertsNum = Verts.size()/ChunksNum + Verts.size()%ChunksNum;
    auto PerChunkFaceNum = Tris.size()/ChunksNum + Tris.size()%ChunksNum;
    size_t CurrentPosVerts = 0;
    size_t CurrentPosFace = 0;
    for (size_t i = 0; i < ChunksNum; i++)
    {
        if (!IVERIFY(CurrentPosVerts < Verts.size()) || !IVERIFY(CurrentPosFace < Tris.size()))
        {
            break;
        }
        auto DeltaVerts = std::min(PerChunkVertsNum, Verts.size() - CurrentPosVerts);
        auto DeltaTris = std::min(PerChunkFaceNum, Tris.size() - CurrentPosFace);
        auto& Chunk = Data[i];
        Chunk.AddStaticGeom(
            {Verts.data()+CurrentPosVerts, DeltaVerts},
            {Tris.data()+CurrentPosFace, DeltaTris});
        CurrentPosVerts += DeltaVerts;
        CurrentPosFace += DeltaTris;
    }
}

void CForm::CFormatVanillaChunked::GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const
{
    OutVertices.clear();
    OutTris.clear();
    OutVertices.reserve(Header.vertcount);
    OutTris.reserve(Header.facecount);
    
    for (auto& elem : Data)
    {
        OutVertices.append_range(xr_span<Fvector>{elem.VertsPtr, elem.GetHeader().vertcount});
    	for (u32 i = 0; i < elem.GetHeader().facecount; i++)
    	{
    		auto& Tris = OutTris.emplace_back();
    		Tris.verts[0] = elem.TrisPtr[i].verts[0];
    		Tris.verts[1] = elem.TrisPtr[i].verts[1];
    		Tris.verts[2] = elem.TrisPtr[i].verts[2];
    		Tris.dummy = elem.TrisPtr[i].dummy;
    		Tris.StreamedSectorID = 0;
    		//OutTris.append_range(xr_span<CDB::TRI>{elem.TrisPtr, elem.GetHeader().facecount});
    	}
    }
}

void CForm::CFormatVanillaChunked::ReadData(CDB::MODEL& Model, CDB::build_callback* bc, void* bcp) const
{
	GetStaticGeom(Model.verts, Model.tris);
	
	if (bc)
	{
		bc(Model.verts.data(), Header.vertcount, Model.tris.data(), Header.facecount, bcp);
	}
	
	Model.build_simple();
}

CDB::MODEL* CForm::CFormatInstanced::ReadInstance(shared_str Path, CDB::build_callback* bc, void* bcp) const
{
	xr_stack_string_path FixedPath = "static\\";
	FixedPath.append(Path.c_str());
	return ::Render->model_GetPrototypeCollision(FixedPath.c_str());
}

CForm::CFormatInstanced::CFormatInstanced()
{
	Header.version = CFormVersions::Instanced;
}

CForm::CFormatInstanced::~CFormatInstanced()
{
	if (FileReader)
	{
		xr_delete(FileReader);
	}
}

bool CForm::CFormatInstanced::Write(xr_string_view FileName)
{
	xr_stack_string_path Path = FileName.data();
	Path.append(".cform");
    
	auto Writer = FS.wg_open(Path.c_str());
	if (!I_ASSERT(Writer))
	{
		return false;
	}

	Writer->w(&Header, sizeof(Header));
	Writer->w(VertsPtr, Header.vertcount*sizeof(Fvector));
	Writer->w(TrisPtr, Header.facecount*sizeof(CDB::TRI));
	
	Writer->w_u64(instances.size());
	for (auto& elem : instances)
	{
		Writer->w_stringZ(elem.first);
		Writer->w_u64(elem.second.size());
		Writer->w(elem.second.data(), elem.second.size()*sizeof(decltype(elem.second)::value_type));
	}
	
	CDB::MODEL PreBuild;
	PreBuild.verts.resize(Header.vertcount);
	std::memcpy(PreBuild.verts.data(), VertsPtr, Header.vertcount*sizeof(Fvector));
	PreBuild.tris.resize(Header.facecount);
	std::memcpy(PreBuild.tris.data(), TrisPtr, Header.facecount*sizeof(CDB::TRI));
	
	for (auto& [Name, Vec] : instances)
	{
		auto Index = PreBuild.models.size();
		auto Model = Models[Name];
		PreBuild.models.push_back(Model);
		for (auto& Inst : Vec)
		{
			auto Inv = Inst.xform;
			Inv.invert();
			PreBuild.instances.emplace_back(Inst.xform, Inv, Inst.AABB, Index, Inst.RenderSector);
		}
	}
    
	return true;
}

bool CForm::CFormatInstanced::Read(xr_string_view FileName)
{
	xr_stack_string_path Path = FileName.data();
	Path.append(".cform");

	FileReader = FS.r_open(Path.c_str());
	if (!I_ASSERT_M(FileReader, "Unable to open file [%s]", Path.c_str()))
	{
		return false;
	}

	FileHash = crc32(FileReader->pointer(), FileReader->length());
    
	FileReader->r(&Header, sizeof(Header));
	if (!I_ASSERT(Header.version == CFormVersions::Instanced || Header.version == CFormVersions::InstancedChunkedData))
	{
		return false;
	}
	VertsPtr = (Fvector*)FileReader->pointer();
	FileReader->advance(Header.vertcount*sizeof(Fvector));
	TrisPtr = (CDB::TRI*)FileReader->pointer();
	FileReader->advance(Header.facecount*sizeof(CDB::TRI));
	
	size_t InstancesCount = FileReader->r_u64();
	for (size_t i = 0; i < InstancesCount; ++i)
	{
		shared_str ObjectName;
		FileReader->r_stringZ(ObjectName);
		auto& Slot = instances[ObjectName];
		
		size_t xformCount = FileReader->r_u64();
		Slot.resize(xformCount);
		std::memcpy(Slot.data(), FileReader->pointer(), xformCount * sizeof(decltype(instances)::mapped_type::value_type));
		FileReader->advance(xformCount * sizeof(decltype(instances)::mapped_type::value_type));
	}

	return true;
}

void CForm::CFormatInstanced::AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris)
{
	Header.vertcount = Verts.size();
	Header.facecount = Tris.size();
	Header.aabb.invalidate();
	for (auto& elem : Verts)
	{
		Header.aabb.modify(elem);
	}
	VertsPtr = Verts.data();
	TrisPtr = Tris.data();
}

void CForm::CFormatInstanced::AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, CDB::MODEL& Collsion, u16 RenderSector, u32 StreamingSector)
{
	instances.try_emplace(Path).first->second.emplace_back(xform, AABB, RenderSector);
	Models.try_emplace(Path).first->second = &Collsion;
}

void CForm::CFormatInstanced::GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const
{
	VERIFY(false);
}

void CForm::CFormatInstanced::ReadData(CDB::MODEL& Model, CDB::build_callback* bc, void* bcp) const
{
	xr_task_group LoadTaskGroup;
	for (auto& elem : instances)
	{
		LoadTaskGroup.run([&]()
		{
			PROF_START_THREAD((xr_string("LoadTaskGroup") + std::to_string(std::this_thread::get_id()._Get_underlying_id()).c_str()).c_str())
			PROF_EVENT("CForm::CFormatInstanced::ReadData::Task")
			size_t Index;
			{
				auto InstanceMesh = ReadInstance(elem.first, bc, bcp);
				xrCriticalSectionGuard g(Model.ModelsCS);
				Index = Model.models.size();
				Model.models.emplace_back(InstanceMesh);
			}
			for(auto& trans : elem.second)
			{
				Fmatrix Inv = trans.xform;
				Inv.invert();
				xrCriticalSectionGuard g(Model.InstancesCS);
				Model.instances.emplace_back(trans.xform, Inv, trans.AABB, Index, trans.RenderSector);
			}
			PROF_STOP_THREAD()
		});
		/*auto InstanceMesh = ReadInstance(elem.first, bc, bcp);
		Model.models.emplace_back(InstanceMesh);
		for(auto& trans : elem.second)
		{
			Fmatrix Inv = trans.xform;
			Inv.invert();
			Model.instances.emplace_back(trans.xform, Inv, trans.AABB, Model.models.size()-1, trans.Sector);
		}*/
	}
	LoadTaskGroup.wait();
	
	Model.verts.resize(Header.vertcount);
	std::memcpy(Model.verts.data(), VertsPtr, sizeof(Fvector) * Header.vertcount);
	Model.tris.resize(Header.facecount);
	std::memcpy(Model.tris.data(), TrisPtr, sizeof(CDB::TRI) * Header.facecount);
	
	if (bc)
	{
		bc(Model.verts.data(), Header.vertcount, Model.tris.data(), Header.facecount, bcp);
	}

	Model.build_simple();
}

namespace
{
	// Encodes a signed tile grid coordinate pair into a single hashable key.
	IC u64 EncodeTileKey(s32 X, s32 Z)
	{
		return (u64(u32(X)) << 32) | u64(u32(Z));
	}

	IC void DecodeTileKey(u64 Key, s32& X, s32& Z)
	{
		X = s32(u32(Key >> 32));
		Z = s32(u32(Key & 0xFFFFFFFFu));
	}

	IC s32 TileCoordFromPos(float Pos, float TileGridSize)
	{
		return iFloor(Pos / TileGridSize);
	}
}

xr_string CForm::CFormatStreamed::MakeTileFileName(xr_string_view FileName, s32 X, s32 Z)
{
	xr_stack_string_path Path = FileName.data();
	string64 Suffix;
	xr_sprintf(Suffix, "_tile_%d_%d", X, Z);
	Path.append(Suffix);
	return xr_string(Path.c_str());
}

xr_string CForm::CFormatStreamed::MakeSectorFileName(xr_string_view FileName, u32 SectorID)
{
	xr_stack_string_path Path = FileName.data();
	string64 Suffix;
	xr_sprintf(Suffix, "_sector_%u", SectorID);
	Path.append(Suffix);
	return xr_string(Path.c_str());
}

CForm::CFormatStreamed::CFormatStreamed()
{
	Header.version = CFormVersions::Streamed;
}

CForm::CFormatStreamed::CFormatStreamed(float InTileGridSize) : CFormatStreamed()
{
	TileGridSize = InTileGridSize;
}

void CForm::CFormatStreamed::AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris)
{
	if (!IVERIFY(TileGridSize > 0.f))
	{
		TileGridSize = 100.f;
	}

	Header.aabb.invalidate();
	for (auto& V : Verts)
	{
		Header.aabb.modify(V);
	}
	Header.vertcount = Verts.size();
	Header.facecount = Tris.size();

	auto BucketVertex = [](xr_hash_map<u32, u32>& Remap, xr_vector<Fvector>& OutVerts, xr_span<Fvector> Src, u32 OriginalIndex) -> u32
	{
		auto It = Remap.find(OriginalIndex);
		if (It != Remap.end())
		{
			return It->second;
		}
		u32 NewIndex = (u32)OutVerts.size();
		OutVerts.push_back(Src[OriginalIndex]);
		Remap.emplace(OriginalIndex, NewIndex);
		return NewIndex;
	};

	for (auto& T : Tris)
	{
		TileGeom* Bucket = nullptr;
		if (T.StreamedSectorID != 0)
		{
			Bucket = &SectorGeomMap[T.StreamedSectorID];
			CDB::TRI Local = T;
			Local.verts[0] = BucketVertex(Bucket->VertRemap, Bucket->Verts, Verts, T.verts[0]);
			Local.verts[1] = BucketVertex(Bucket->VertRemap, Bucket->Verts, Verts, T.verts[1]);
			Local.verts[2] = BucketVertex(Bucket->VertRemap, Bucket->Verts, Verts, T.verts[2]);
			Bucket->Tris.push_back(Local);
		}
		else
		{
			for (int i = 0; i < 3; i++)
			{
				s32 TileX = TileCoordFromPos(Verts[T.verts[i]].x, TileGridSize);
				s32 TileZ = TileCoordFromPos(Verts[T.verts[i]].z, TileGridSize);
				Bucket = &TileGeomMap[EncodeTileKey(TileX, TileZ)];
				
				CDB::TRI Local = T;
				Local.verts[0] = BucketVertex(Bucket->VertRemap, Bucket->Verts, Verts, T.verts[0]);
				Local.verts[1] = BucketVertex(Bucket->VertRemap, Bucket->Verts, Verts, T.verts[1]);
				Local.verts[2] = BucketVertex(Bucket->VertRemap, Bucket->Verts, Verts, T.verts[2]);
				Bucket->Tris.push_back(Local);
			}
			/*Fvector Centroid;
			Centroid.add(Verts[T.verts[0]], Verts[T.verts[1]]);
			Centroid.add(Verts[T.verts[2]]);
			Centroid.mul(1.f / 3.f);

			s32 TileX = TileCoordFromPos(Centroid.x, TileGridSize);
			s32 TileZ = TileCoordFromPos(Centroid.z, TileGridSize);
			Bucket = &TileGeomMap[EncodeTileKey(TileX, TileZ)];*/
		}

		/*CDB::TRI Local = T;
		Local.verts[0] = BucketVertex(Bucket->VertRemap, Bucket->Verts, Verts, T.verts[0]);
		Local.verts[1] = BucketVertex(Bucket->VertRemap, Bucket->Verts, Verts, T.verts[1]);
		Local.verts[2] = BucketVertex(Bucket->VertRemap, Bucket->Verts, Verts, T.verts[2]);
		Bucket->Tris.push_back(Local);*/
	}
}

bool CForm::CFormatStreamed::Write(xr_string_view FileName)
{
	BaseFileName = xr_string(FileName.data());

	Tiles.clear();
	Sectors.clear();
	Tiles.reserve(TileGeomMap.size());
	Sectors.reserve(SectorGeomMap.size());

	for (auto& [Key, Bucket] : TileGeomMap)
	{
		s32 X, Z;
		DecodeTileKey(Key, X, Z);
		Tiles.push_back({X, Z});

		CFormatVanilla TileFormat;
		TileFormat.AddStaticGeom(Bucket.Verts, Bucket.Tris);
		if (!I_ASSERT(TileFormat.Write(MakeTileFileName(FileName, X, Z))))
		{
			return false;
		}
	}

	for (auto& [SectorID, Bucket] : SectorGeomMap)
	{
		Sectors.push_back(SectorID);

		CFormatVanilla SectorFormat;
		SectorFormat.AddStaticGeom(Bucket.Verts, Bucket.Tris);
		if (!I_ASSERT(SectorFormat.Write(MakeSectorFileName(FileName, SectorID))))
		{
			return false;
		}
	}

	xr_stack_string_path Path = FileName.data();
	Path.append(".cform");
	auto Writer = FS.wg_open(Path.c_str());
	if (!I_ASSERT(Writer))
	{
		return false;
	}

	Writer->w(&Header, sizeof(Header));
	Writer->w_float(TileGridSize);
	Writer->w_u32((u32)Tiles.size());
	for (auto& Tile : Tiles)
	{
		Writer->w_s32(Tile.X);
		Writer->w_s32(Tile.Z);
	}
	Writer->w_u32((u32)Sectors.size());
	for (auto SectorID : Sectors)
	{
		Writer->w_u32(SectorID);
	}

	return true;
}

bool CForm::CFormatStreamed::Read(xr_string_view FileName)
{
	BaseFileName = xr_string(FileName.data());

	xr_stack_string_path Path = FileName.data();
	Path.append(".cform");
	auto Reader = FS.rg_open(Path.c_str());
	if (!I_ASSERT(Reader))
	{
		return false;
	}

	FileHash = crc32(Reader->pointer(), Reader->length());

	Reader->r(&Header, sizeof(Header));
	if (!I_ASSERT(Header.version == CFormVersions::Streamed))
	{
		return false;
	}

	TileGridSize = Reader->r_float();

	u32 TileCount = Reader->r_u32();
	Tiles.resize(TileCount);
	for (auto& Tile : Tiles)
	{
		Tile.X = Reader->r_s32();
		Tile.Z = Reader->r_s32();
	}

	u32 SectorCount = Reader->r_u32();
	Sectors.resize(SectorCount);
	for (auto& SectorID : Sectors)
	{
		SectorID = Reader->r_u32();
	}

	return true;
}

bool CForm::CFormatStreamed::LoadStreamedTile(s32 TileX, s32 TileZ, CDB::MODEL& OutModel, CDB::build_callback* bc, void* bcp) const
{
	xr_string TilePath = MakeTileFileName(xr_string_view(BaseFileName.c_str(), BaseFileName.size()), TileX, TileZ);
	xr_stack_string_path CheckPath = TilePath.c_str();
	CheckPath.append(".cform");
	if (!FS.exist(CheckPath.c_str()))
	{
		return false;
	}

	CFormatVanilla TileFormat;
	if (!I_ASSERT_M(TileFormat.Read(TilePath.c_str()), "Unable to read streamed tile [%s]", TilePath.c_str()))
	{
		return false;
	}
	TileFormat.ReadData(OutModel, bc, bcp);
	return true;
}

bool CForm::CFormatStreamed::LoadStreamedSector(u32 SectorID, CDB::MODEL& OutModel, CDB::build_callback* bc, void* bcp) const
{
	xr_string SectorPath = MakeSectorFileName(xr_string_view(BaseFileName.c_str(), BaseFileName.size()), SectorID);
	xr_stack_string_path CheckPath = SectorPath.c_str();
	CheckPath.append(".cform");
	if (!FS.exist(CheckPath.c_str()))
	{
		return false;
	}

	CFormatVanilla SectorFormat;
	if (!I_ASSERT_M(SectorFormat.Read(SectorPath.c_str()), "Unable to read streamed sector [%s]", SectorPath.c_str()))
	{
		return false;
	}
	SectorFormat.ReadData(OutModel, bc, bcp);
	return true;
}

xr_string CForm::CFormatStreamedInstanced::MakeTileFileName(xr_string_view FileName, s32 X, s32 Z)
{
	xr_stack_string_path Path = FileName.data();
	string64 Suffix;
	xr_sprintf(Suffix, "_tile_%d_%d", X, Z);
	Path.append(Suffix);
	return xr_string(Path.c_str());
}

xr_string CForm::CFormatStreamedInstanced::MakeSectorFileName(xr_string_view FileName, u32 SectorID)
{
	xr_stack_string_path Path = FileName.data();
	string64 Suffix;
	xr_sprintf(Suffix, "_sector_%u", SectorID);
	Path.append(Suffix);
	return xr_string(Path.c_str());
}

CForm::CFormatStreamedInstanced::CFormatStreamedInstanced()
{
	Header.version = CFormVersions::StreamedInstanced;
}

CForm::CFormatStreamedInstanced::CFormatStreamedInstanced(float InTileGridSize) : CFormatStreamedInstanced()
{
	TileGridSize = InTileGridSize;
}

void CForm::CFormatStreamedInstanced::AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris)
{
	if (!IVERIFY(TileGridSize > 0.f))
	{
		TileGridSize = 100.f;
	}

	Header.aabb.invalidate();
	for (auto& V : Verts)
	{
		Header.aabb.modify(V);
	}
	Header.vertcount = Verts.size();
	Header.facecount = Tris.size();

	auto BucketVertex = [](xr_hash_map<u32, u32>& Remap, xr_vector<Fvector>& OutVerts, xr_span<Fvector> Src, u32 OriginalIndex) -> u32
	{
		auto It = Remap.find(OriginalIndex);
		if (It != Remap.end())
		{
			return It->second;
		}
		u32 NewIndex = (u32)OutVerts.size();
		OutVerts.push_back(Src[OriginalIndex]);
		Remap.emplace(OriginalIndex, NewIndex);
		return NewIndex;
	};

	for (auto& T : Tris)
	{
		if (T.StreamedSectorID != 0)
		{
			auto& Bucket = SectorGeomMap[T.StreamedSectorID];
			CDB::TRI Local = T;
			Local.verts[0] = BucketVertex(Bucket.VertRemap, Bucket.Verts, Verts, T.verts[0]);
			Local.verts[1] = BucketVertex(Bucket.VertRemap, Bucket.Verts, Verts, T.verts[1]);
			Local.verts[2] = BucketVertex(Bucket.VertRemap, Bucket.Verts, Verts, T.verts[2]);
			Bucket.Tris.push_back(Local);
		}
		else
		{
			Ivector2 V1T = {
				TileCoordFromPos(Verts[T.verts[0]].x, TileGridSize), 
				TileCoordFromPos(Verts[T.verts[0]].z, TileGridSize)
			};
			Ivector2 V2T = {
				TileCoordFromPos(Verts[T.verts[1]].x, TileGridSize), 
				TileCoordFromPos(Verts[T.verts[1]].z, TileGridSize)
			};
			Ivector2 V3T = {
				TileCoordFromPos(Verts[T.verts[2]].x, TileGridSize), 
				TileCoordFromPos(Verts[T.verts[2]].z, TileGridSize)
			};
			for (s32 i = std::min(V1T.x, std::min(V2T.x, V3T.x)); i <= std::max(V1T.x, std::max(V2T.x, V3T.x)); i++)
			{
				for (s32 j = std::min(V1T.y, std::min(V2T.y, V3T.y)); j <= std::max(V1T.y, std::max(V2T.y, V3T.y)); j++)
				{
					auto& Bucket = TileGeomMap[EncodeTileKey(i, j)];
					CDB::TRI Local = T;
					Local.verts[0] = BucketVertex(Bucket.VertRemap, Bucket.Verts, Verts, T.verts[0]);
					Local.verts[1] = BucketVertex(Bucket.VertRemap, Bucket.Verts, Verts, T.verts[1]);
					Local.verts[2] = BucketVertex(Bucket.VertRemap, Bucket.Verts, Verts, T.verts[2]);
					Bucket.Tris.push_back(Local);
				}
			}
		}
	}
}

void CForm::CFormatStreamedInstanced::AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, CDB::MODEL& Collsion, u16 RenderSector, u32 StreamingSector)
{
	Models.try_emplace(Path).first->second = &Collsion;

	if (StreamingSector != 0)
	{
		auto& Bucket = SectorGeomMap[StreamingSector];
		Bucket.Instances.try_emplace(Path).first->second.emplace_back(xform, AABB, RenderSector);
	}
	else
	{
		Ivector2 V1T = {
			TileCoordFromPos(AABB.min.x, TileGridSize), 
			TileCoordFromPos(AABB.min.z, TileGridSize)
		};
		Ivector2 V4T = {
			TileCoordFromPos(AABB.max.x, TileGridSize), 
			TileCoordFromPos(AABB.max.z, TileGridSize)
		};
		for (s32 i = V1T.x; i <= V4T.x; i++)
		{
			for (s32 j = V1T.y; j <= V4T.y; j++)
			{
				auto& Bucket = TileGeomMap[EncodeTileKey(i, j)];
				Bucket.Instances.try_emplace(Path).first->second.emplace_back(xform, AABB, RenderSector);
			}
		}
	}

}

bool CForm::CFormatStreamedInstanced::Write(xr_string_view FileName)
{
	BaseFileName = xr_string(FileName.data());

	Tiles.clear();
	Sectors.clear();
	Tiles.reserve(TileGeomMap.size());
	Sectors.reserve(SectorGeomMap.size());

	for (auto& [Key, Bucket] : TileGeomMap)
	{
		s32 X, Z;
		DecodeTileKey(Key, X, Z);
		Tiles.push_back({X, Z});

		CFormatInstanced TileFormat;
		TileFormat.AddStaticGeom(Bucket.Verts, Bucket.Tris);
		for (auto& [Name, Vec] : Bucket.Instances)
		{
			auto ModelIt = Models.find(Name);
			if (!IVERIFY(ModelIt != Models.end()))
			{
				continue;
			}
			for (auto& Inst : Vec)
			{
				TileFormat.AddInstanceRef(Name, Inst.xform, Inst.AABB, *ModelIt->second, Inst.RenderSector, Inst.StreamingSector);
			}
		}
		if (!I_ASSERT(TileFormat.Write(MakeTileFileName(FileName, X, Z))))
		{
			return false;
		}
	}

	for (auto& [SectorID, Bucket] : SectorGeomMap)
	{
		Sectors.push_back(SectorID);

		CFormatInstanced SectorFormat;
		SectorFormat.AddStaticGeom(Bucket.Verts, Bucket.Tris);
		for (auto& [Name, Vec] : Bucket.Instances)
		{
			auto ModelIt = Models.find(Name);
			if (!IVERIFY(ModelIt != Models.end()))
			{
				continue;
			}
			for (auto& Inst : Vec)
			{
				SectorFormat.AddInstanceRef(Name, Inst.xform, Inst.AABB, *ModelIt->second, Inst.RenderSector, Inst.StreamingSector);
			}
		}
		if (!I_ASSERT(SectorFormat.Write(MakeSectorFileName(FileName, SectorID))))
		{
			return false;
		}
	}

	xr_stack_string_path Path = FileName.data();
	Path.append(".cform");
	auto Writer = FS.wg_open(Path.c_str());
	if (!I_ASSERT(Writer))
	{
		return false;
	}

	Writer->w(&Header, sizeof(Header));
	Writer->w_float(TileGridSize);
	Writer->w_u32((u32)Tiles.size());
	for (auto& Tile : Tiles)
	{
		Writer->w_s32(Tile.X);
		Writer->w_s32(Tile.Z);
	}
	Writer->w_u32((u32)Sectors.size());
	for (auto SectorID : Sectors)
	{
		Writer->w_u32(SectorID);
	}

	return true;
}

bool CForm::CFormatStreamedInstanced::Read(xr_string_view FileName)
{
	BaseFileName = xr_string(FileName.data());

	xr_stack_string_path Path = FileName.data();
	Path.append(".cform");
	auto Reader = FS.rg_open(Path.c_str());
	if (!I_ASSERT(Reader))
	{
		return false;
	}

	FileHash = crc32(Reader->pointer(), Reader->length());

	Reader->r(&Header, sizeof(Header));
	if (!I_ASSERT(Header.version == CFormVersions::StreamedInstanced))
	{
		return false;
	}

	TileGridSize = Reader->r_float();

	u32 TileCount = Reader->r_u32();
	Tiles.resize(TileCount);
	for (auto& Tile : Tiles)
	{
		Tile.X = Reader->r_s32();
		Tile.Z = Reader->r_s32();
	}

	u32 SectorCount = Reader->r_u32();
	Sectors.resize(SectorCount);
	for (auto& SectorID : Sectors)
	{
		SectorID = Reader->r_u32();
	}

	return true;
}

bool CForm::CFormatStreamedInstanced::LoadStreamedTile(s32 TileX, s32 TileZ, CDB::MODEL& OutModel, CDB::build_callback* bc, void* bcp) const
{
	xr_string TilePath = MakeTileFileName(xr_string_view(BaseFileName.c_str(), BaseFileName.size()), TileX, TileZ);
	xr_stack_string_path CheckPath = TilePath.c_str();
	CheckPath.append(".cform");
	if (!FS.exist(CheckPath.c_str()))
	{
		return false;
	}

	CFormatInstanced TileFormat;
	if (!I_ASSERT_M(TileFormat.Read(TilePath.c_str()), "Unable to read streamed tile [%s]", TilePath.c_str()))
	{
		return false;
	}
	TileFormat.ReadData(OutModel, bc, bcp);
	return true;
}

bool CForm::CFormatStreamedInstanced::LoadStreamedSector(u32 SectorID, CDB::MODEL& OutModel, CDB::build_callback* bc, void* bcp) const
{
	xr_string SectorPath = MakeSectorFileName(xr_string_view(BaseFileName.c_str(), BaseFileName.size()), SectorID);
	xr_stack_string_path CheckPath = SectorPath.c_str();
	CheckPath.append(".cform");
	if (!FS.exist(CheckPath.c_str()))
	{
		return false;
	}

	CFormatInstanced SectorFormat;
	if (!I_ASSERT_M(SectorFormat.Read(SectorPath.c_str()), "Unable to read streamed sector [%s]", SectorPath.c_str()))
	{
		return false;
	}
	SectorFormat.ReadData(OutModel, bc, bcp);
	return true;
}

XRCORE_API xr_unique_ptr<CForm::IFormat> CForm::Read(const char* Initial, xr_string_view Filename)
{
    ChunkHeader Header;
    xr_stack_string_path Path = Filename.data();
    if (Initial&&Initial[0])
    {
        FS.update_path(Path,Initial,Filename.data());
    }
    {
        xr_stack_string_path TempPath = Path;
        TempPath.append(".cform");
        auto Reader = FS.rg_open(TempPath.c_str());
        if (!I_ASSERT(Reader))
        {
            return nullptr;
        }
        Reader->r(&Header, sizeof(Header));
    }

    switch (Header.version)
    {
    case CFormVersions::Vanilla:
        {
            auto Parsed = new CFormatVanilla();
            if (!I_ASSERT_M(Parsed->Read(Path.c_str()), "Unable to read [%s]", Path.c_str()))
            {
                xr_delete(Parsed);
                return nullptr;
            }
            return xr_unique_ptr<CForm::IFormat>(Parsed);
        }
    case CFormVersions::VanillaChunked:
        {
            auto Parsed = new CFormatVanillaChunked(1);
            if (!I_ASSERT_M(Parsed->Read(Path.c_str()), "Unable to read [%s]", Path.c_str()))
            {
                xr_delete(Parsed);
                return nullptr;
            }
            return xr_unique_ptr<CForm::IFormat>(Parsed);
        }
    case CFormVersions::Instanced:
    	{
    		auto Parsed = new CFormatInstanced();
    		if (!I_ASSERT_M(Parsed->Read(Path.c_str()), "Unable to read [%s]", Path.c_str()))
    		{
    			xr_delete(Parsed);
    			return nullptr;
    		}
    		return xr_unique_ptr<CForm::IFormat>(Parsed);
    	}
    case CFormVersions::Streamed:
    	{
    		auto Parsed = new CFormatStreamed();
    		if (!I_ASSERT_M(Parsed->Read(Path.c_str()), "Unable to read [%s]", Path.c_str()))
    		{
    			xr_delete(Parsed);
    			return nullptr;
    		}
    		return xr_unique_ptr<CForm::IFormat>(Parsed);
    	}
    case CFormVersions::StreamedInstanced:
    	{
    		auto Parsed = new CFormatStreamedInstanced();
    		if (!I_ASSERT_M(Parsed->Read(Path.c_str()), "Unable to read [%s]", Path.c_str()))
    		{
    			xr_delete(Parsed);
    			return nullptr;
    		}
    		return xr_unique_ptr<CForm::IFormat>(Parsed);
    	}
    default:
        {
            I_ASSERT_M(false, "Invalid .cform type in [%s]", Path.c_str());
        }
    }
    
    return nullptr;
}

XRCORE_API xr_unique_ptr<CForm::IFormat> CForm::Read(xr_string_view Filename)
{
    return Read(nullptr, Filename);
}

XRCORE_API void CForm::Write(const char* Initial, xr_string_view Filename, IFormat& Data)
{
    xr_stack_string_path Path = Filename.data();
    if (Initial&&Initial[0])
    {
        FS.update_path(Path,Initial,Filename.data());
    }
    I_ASSERT(Data.Write(Path.c_str()));
}

XRCORE_API void CForm::Write(xr_string_view Filename, IFormat& Data)
{
    Write(nullptr, Filename, Data);
}
