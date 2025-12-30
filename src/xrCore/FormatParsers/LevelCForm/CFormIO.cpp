#include "stdafx.h"
#include "CFormIO.h"

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
    Writer->w(Data.Verts.data(), Data.Verts.size()*sizeof(Fvector));
    Writer->w(Data.Tris.data(), Data.Tris.size()*sizeof(CDB::TRI));
    
    return true;
}

bool CForm::CFormatVanilla::Read(xr_string_view FileName)
{
    xr_stack_string_path Path = FileName.data();
    Path.append(".cform");

    auto Reader = FS.rg_open(Path.c_str());
    if (!I_ASSERT_M(Reader, "Unable to open file [%s]", Path.c_str()))
    {
        return false;
    }

    FileHash = crc32(Reader->pointer(), Reader->length());
    
    Reader->r(&Header, sizeof(Header));
    if (!I_ASSERT(Header.version == CFormVersions::Vanilla || Header.version == CFormVersions::VanillaChunkedData))
    {
        return false;
    }
    Data.Verts.resize(Header.vertcount);
    Data.Tris.resize(Header.facecount);
    Reader->r(Data.Verts.data(), Data.Verts.size()*sizeof(Fvector));
    Reader->r(Data.Tris.data(), Data.Tris.size()*sizeof(CDB::TRI));

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
    Data.Tris.resize(Tris.size());
    std::memcpy(Data.Tris.data(), Tris.data(), sizeof(CDB::TRI) * Tris.size());
    Data.Verts.resize(Verts.size());
    std::memcpy(Data.Verts.data(), Verts.data(), sizeof(Fvector) * Verts.size());
}

void CForm::CFormatVanilla::GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const
{
    OutVertices.clear();
    OutTris.clear();
    OutVertices.resize(Header.vertcount);
    OutTris.resize(Header.facecount);
    std::memcpy(OutVertices.data(), Data.Verts.data(), sizeof(Fvector) * OutVertices.size());
    std::memcpy(OutTris.data(), Data.Tris.data(), sizeof(CDB::TRI) * OutTris.size());
}

CForm::CFormatVanillaChunked::CFormatVanillaChunked(u32 ChunkNumber)
{
    if (!IVERIFY(ChunkNumber > 0)){
        ChunkNumber = 1;
    }
    Header.version = CFormVersions::VanillaChunkedMain;
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
    if (!I_ASSERT(Header.version == CFormVersions::VanillaChunkedMain))
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
        if (!IVERIFY(CurrentPosVerts > Verts.size()) || !IVERIFY(CurrentPosFace > Tris.size()))
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
    OutVertices.resize(Header.vertcount);
    OutTris.resize(Header.facecount);
    for (auto& elem : Data)
    {
        OutVertices.append_range(elem.Data.Verts);
        OutTris.append_range(elem.Data.Tris);
    }
}

XRCORE_API xr_unique_ptr<CForm::IFormat> CForm::Read(LPCSTR Initial, xr_string_view Filename)
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
    case CFormVersions::VanillaChunkedMain:
        {
            auto Parsed = new CFormatVanillaChunked(1);
            if (!I_ASSERT_M(Parsed->Read(Filename), "Unable to read [%s]", Filename.data()))
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

XRCORE_API void CForm::Write(LPCSTR Initial, xr_string_view Filename, IFormat& Data)
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
