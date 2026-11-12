#include "stdafx.h"
#include "CFormIO.h"

//#include "API/xrAPI.h"
#include "src/xrEngine/Render.h"

using namespace XRay;

namespace
{
	void WriteTriangles(IWriter& Writer, const CDB::TRI* Tris, size_t Count)
	{
		for (size_t Index = 0; Index < Count; ++Index)
		{
			const auto& Tri = Tris[Index];
			CDB::TRI_Vanilla Packed{{Tri.verts[0], Tri.verts[1], Tri.verts[2]}, Tri.dummy};
			Writer.w(&Packed, sizeof(Packed));
		}
	}

	bool ReadTriangles(IReader& Reader, xr_vector<CDB::TRI>& Tris, size_t Count)
	{
		if (Count > Reader.elapsed() / sizeof(CDB::TRI_Vanilla))
		{
			return false;
		}
		Tris.resize(Count);
		for (auto& Tri : Tris)
		{
			CDB::TRI_Vanilla Packed;
			Reader.r(&Packed, sizeof(Packed));
			std::copy(std::begin(Packed.verts), std::end(Packed.verts), Tri.verts);
			Tri.dummy = Packed.dummy;
			Tri.StreamedSectorID = 0;
		}
		return true;
	}
}

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
    WriteTriangles(*Writer, TrisPtr, Header.facecount);
    
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

    if (FileReader->length() < sizeof(Header))
    {
        return false;
    }
    FileHash = crc32(FileReader->pointer(), FileReader->length());
    
    FileReader->r(&Header, sizeof(Header));
    if (!I_ASSERT(Header.version == CFormVersions::Vanilla || Header.version == CFormVersions::VanillaChunkedData))
    {
        return false;
    }
	if (Header.vertcount > FileReader->elapsed() / sizeof(Fvector))
	{
		return false;
	}
	VertsPtr = (Fvector*)FileReader->pointer();
	FileReader->advance(Header.vertcount*sizeof(Fvector));
	if (!ReadTriangles(*FileReader, ReadTris, Header.facecount))
	{
		return false;
	}
	TrisPtr = ReadTris.data();

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
    std::memcpy(OutTris.data(), TrisPtr, sizeof(CDB::TRI) * OutTris.size());
}

void CForm::CFormatVanilla::ReadData(CDB::MODEL& Model, CDB::build_callback* bc, void* bcp) const
{
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
        OutTris.append_range(xr_span<CDB::TRI>{elem.TrisPtr, elem.GetHeader().facecount});
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

CForm::CFormatInstanced::CFormatInstanced()
{
	Header.version = CFormVersions::Instanced;
}

CForm::CFormatInstanced::~CFormatInstanced()
{
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
	WriteTriangles(*Writer, TrisPtr, Header.facecount);
	
	Writer->w_u64(instances.size());
	for (auto& elem : instances)
	{
		Writer->w_stringZ(elem.first);
		
		auto Model = Models[elem.first];
		Writer->w_u64(Model.VertsCount);
		Writer->w(Model.VertsPtr, Model.VertsCount*sizeof(Fvector));
		Writer->w_u64(Model.TrisCount);
		WriteTriangles(*Writer, Model.TrisPtr, Model.TrisCount);
		
		Writer->w_u64(elem.second.size());
		Writer->w(elem.second.data(), elem.second.size()*sizeof(decltype(elem.second)::value_type));
	}
    
	return true;
}

bool CForm::CFormatInstanced::Read(xr_string_view FileName)
{
	xr_stack_string_path Path = FileName.data();
	Path.append(".cform");

	FileReader = FS.rg_open(Path.c_str());
	if (!I_ASSERT_M(FileReader, "Unable to open file [%s]", Path.c_str()))
	{
		return false;
	}

	if (FileReader->length() < sizeof(Header))
	{
		return false;
	}
	FileHash = crc32(FileReader->pointer(), FileReader->length());
    
	FileReader->r(&Header, sizeof(Header));
	if (!I_ASSERT(Header.version == CFormVersions::Instanced || Header.version == CFormVersions::InstancedChunkedData))
	{
		return false;
	}
	if (Header.vertcount > FileReader->elapsed() / sizeof(Fvector))
	{
		return false;
	}
	VertsPtr = (Fvector*)FileReader->pointer();
	FileReader->advance(Header.vertcount*sizeof(Fvector));
	if (!ReadTriangles(*FileReader, ReadTris, Header.facecount))
	{
		return false;
	}
	TrisPtr = ReadTris.data();
	
	size_t InstancesCount = FileReader->r_u64();
	for (size_t i = 0; i < InstancesCount; ++i)
	{
		shared_str ObjectName;
		FileReader->r_stringZ(ObjectName);
		
		auto& Model = Models[ObjectName];
		Model.VertsCount = FileReader->r_u64();
		Model.VertsPtr = (Fvector*)FileReader->pointer();
		FileReader->advance(Model.VertsCount*sizeof(Fvector));
		Model.TrisCount = FileReader->r_u64();
		auto& PrototypeTris = ReadModelTris[ObjectName];
		if (!ReadTriangles(*FileReader, PrototypeTris, Model.TrisCount))
		{
			return false;
		}
		Model.TrisPtr = PrototypeTris.data();
		
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

void CForm::CFormatInstanced::AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, const CDB::MODEL& Collsion, u16 Sector, u32 StreamingSector)
{
	instances.try_emplace(Path).first->second.emplace_back(xform, AABB, Sector);
	auto& Data = Models.try_emplace(Path).first->second;
	Data.VertsPtr = Collsion.get_verts().data();
	Data.TrisPtr = Collsion.get_tris().data();
	Data.VertsCount = Collsion.get_verts().size();
	Data.TrisCount = Collsion.get_tris().size();
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
		LoadTaskGroup.run([&, Entry = &elem]()
		{
			PROF_START_THREAD("CFormPrototypeLoad")
			PROF_EVENT("CForm::CFormatInstanced::ReadData::Task")
			size_t Index;
			CDB::MODEL* Prototype;
			{
				xrCriticalSectionGuard g(Model.ModelsCS);
				Index = Model.models.size();
				Prototype = Model.models.emplace_back(new CDB::MODEL);
			}
			auto& PrototypeData = Models.at(Entry->first);
			Prototype->get_verts().resize(PrototypeData.VertsCount);
			std::memcpy(Prototype->get_verts().data(), PrototypeData.VertsPtr, PrototypeData.VertsCount * sizeof(Fvector));
			Prototype->get_tris().resize(PrototypeData.TrisCount);
			std::memcpy(Prototype->get_tris().data(), PrototypeData.TrisPtr, PrototypeData.TrisCount * sizeof(CDB::TRI));
			if (bc)
			{
				bc(Prototype->get_verts().data(), Prototype->get_verts().size(), Prototype->get_tris().data(), Prototype->get_tris().size(), bcp);
			}
			Prototype->build_simple();
			for(auto& trans : Entry->second)
			{
				Fmatrix Inv = trans.xform;
				Inv.invert();
				xrCriticalSectionGuard g(Model.InstancesCS);
				Model.instances.emplace_back(trans.xform, Inv, trans.AABB, Index, trans.Sector);
			}
			PROF_STOP_THREAD()
		});
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
        if (Reader->length() < sizeof(Header))
        {
            return nullptr;
        }
        Reader->r(&Header, sizeof(Header));
    }

    switch (Header.version)
    {
    case CFormVersions::Streamed:
    case CFormVersions::StreamedInstanced:
        {
            xr_unique_ptr<CForm::IFormat> Parsed;
            if (Header.version == CFormVersions::Streamed)
            {
                Parsed.reset(new CFormatStreamed());
            }
            else
            {
                Parsed.reset(new CFormatStreamedInstanced());
            }
            if (!Parsed->Read(Path.c_str()))
            {
                return nullptr;
            }
            return Parsed;
        }
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
