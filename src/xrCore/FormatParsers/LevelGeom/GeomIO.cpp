#include "stdafx.h"
#include "GeomIO.h"

using namespace XRay::Geom;

IFormat::IFormat()
{
    Header.XRLC_version = XRCL_PRODUCTION_VERSION;
}

CGeomVanillaFormat::~CGeomVanillaFormat()
{
    if (FileReader)
    {
        if (std::holds_alternative<IReader*>(VB))
        {
            auto Reader = std::get<IReader*>(VB);
            FS.r_close(Reader);
        }
        if (std::holds_alternative<IReader*>(IB))
        {
            auto Reader = std::get<IReader*>(IB);
            FS.r_close(Reader);
        }
        if (std::holds_alternative<IReader*>(SWI))
        {
            auto Reader = std::get<IReader*>(SWI);
            FS.r_close(Reader);
        }
        FS.r_close(FileReader);
    }
}

bool CGeomVanillaFormat::Write(xr_string_view FileName, xr_string_view Extension)
{
    xr_stack_string_path path = FileName;
    path.append(Extension);

    auto file = FS.wg_open(path.c_str());
    if (!I_ASSERT(file))
    {
        return false;
    }
    
    file->w_chunk(fsL_HEADER, &Header, sizeof(Header));

    file->open_chunk(fsL_TYPECHUNKS);
    file->w_enum(GeomVanillaType::Vanilla);
    file->close_chunk();

    if (std::holds_alternative<VBCPTR>(VB))
    {
        file->open_chunk(fsL_VB);
        std::get<VBCPTR>(VB)->Save(*file);
        file->close_chunk();
    } else if (std::holds_alternative<BuffPtr>(VB))
    {
        file->open_chunk(fsL_VB);
        file->w_buff(*std::get<BuffPtr>(VB));
        file->close_chunk();
    }

    if (std::holds_alternative<IBCPTR>(IB))
    {
        file->open_chunk(fsL_IB);
        std::get<IBCPTR>(IB)->Save(*file);
        file->close_chunk();
    } else if (std::holds_alternative<BuffPtr>(IB))
    {
        file->open_chunk(fsL_IB);
        file->w_buff(*std::get<BuffPtr>(IB));
        file->close_chunk();
    }

    if (std::holds_alternative<SWICPTR>(SWI))
    {
        file->open_chunk(fsL_SWIS);
        std::get<SWICPTR>(SWI)->Save(*file);
        file->close_chunk();
    } else if (std::holds_alternative<BuffPtr>(SWI))
    {
        file->open_chunk(fsL_SWIS);
        file->w_buff(*std::get<BuffPtr>(SWI));
        file->close_chunk();
    }

    return true;
}

bool CGeomVanillaFormat::Read(xr_string_view FileName, xr_string_view Extension)
{
    xr_stack_string_path path = FileName;
    path.append(Extension);

    VERIFY(!FileReader);
    FileReader = FS.r_open(path.c_str());
    if (!I_ASSERT(FileReader))
    {
        return false;
    }

    if (!FileReader->r_chunk_safe(fsL_HEADER, &Header, sizeof(Header)))
    {
        return false;
    }

    if (auto TypeChunk = FileReader->open_chunk(fsL_TYPECHUNKS); TypeChunk && !IVERIFY(TypeChunk->r_enum<GeomVanillaType>() == GeomVanillaType::Vanilla))
    {
        return false;
    }

    if (auto VBChunk = FileReader->open_chunk(fsL_VB))
    {
        VERIFY(std::holds_alternative<std::monostate>(VB));
        VB = VBChunk;
    }

    if (auto IBChunk = FileReader->open_chunk(fsL_IB))
    {
        VERIFY(std::holds_alternative<std::monostate>(IB));
        IB = IBChunk;
    }

    if (auto SWIChunk = FileReader->open_chunk(fsL_SWIS))
    {
        VERIFY(std::holds_alternative<std::monostate>(SWI));
        SWI = SWIChunk;
    }

    return true;
}

void CGeomVanillaFormat::AddVBData(const VBContainerBase& data)
{
    VERIFY(std::holds_alternative<std::monostate>(VB));
    VB = &data;
}

void CGeomVanillaFormat::AddIBData(const IBContainerBase& data)
{
    
    VERIFY(std::holds_alternative<std::monostate>(IB));
    IB = &data;
}

void CGeomVanillaFormat::AddSWIData(const SWIContainerBase& data)
{
    
    VERIFY(std::holds_alternative<std::monostate>(SWI));
    SWI = &data;
}

void CGeomVanillaFormat::AddVBData(const xr_vector<u8>& data)
{
    VERIFY(std::holds_alternative<std::monostate>(VB));
    VB = &data;
}

void CGeomVanillaFormat::AddIBData(const xr_vector<u8>& data)
{
    
    VERIFY(std::holds_alternative<std::monostate>(IB));
    IB = &data;
}

void CGeomVanillaFormat::AddSWIData(const xr_vector<u8>& data)
{
    
    VERIFY(std::holds_alternative<std::monostate>(SWI));
    SWI = &data;
}

IReaderBase& CGeomVanillaFormat::GetVBData() const
{
    VERIFY(std::holds_alternative<IReader*>(VB));
    return *std::get<IReader*>(VB);
}

IReaderBase& CGeomVanillaFormat::GetIBData() const
{
    VERIFY(std::holds_alternative<IReader*>(IB));
    return *std::get<IReader*>(IB);
}

IReaderBase& CGeomVanillaFormat::GetSWIData() const
{
    VERIFY(std::holds_alternative<IReader*>(SWI));
    return *std::get<IReader*>(SWI);
}

bool CGeomVanillaFormat::HasVBData() const
{
    return std::holds_alternative<IReader*>(VB);
}

bool CGeomVanillaFormat::HasIBData() const
{
    return std::holds_alternative<IReader*>(IB);
}

bool CGeomVanillaFormat::HasSWIData() const
{
    return std::holds_alternative<IReader*>(SWI);
}

CGeomVanillaChunkedFormat::~CGeomVanillaChunkedFormat()
{
    if (std::holds_alternative<CMultiReader*>(VB))
    {
        xr_delete(std::get<CMultiReader*>(VB));
    }
    if (std::holds_alternative<CMultiReader*>(IB))
    {
        xr_delete(std::get<CMultiReader*>(IB));
    }
    if (std::holds_alternative<CMultiReader*>(SWI))
    {
        xr_delete(std::get<CMultiReader*>(SWI));
    }
    for (auto& elem : FileReader)
    {
        elem->close();
    }
}

bool CGeomVanillaChunkedFormat::Write(xr_string_view FileName, xr_string_view Extension)
{
    xr_stack_string_path path = FileName;
    path.append(Extension);

    auto MasterChunk = FS.wg_open(path.c_str());
    if (!I_ASSERT(MasterChunk))
    {
        return false;
    }
    
    std::variant<std::monostate, VBCPTR, BuffPtr> VBPtr = {};
    std::variant<std::monostate, IBCPTR, BuffPtr> IBPtr = {};
    std::variant<std::monostate, SWICPTR, BuffPtr> SWIPtr = {};
    
    if (std::holds_alternative<VBCPTR>(VB))
    {
        VBPtr = std::get<VBCPTR>(VB);
    } else if (std::holds_alternative<BuffPtr>(VB))
    {
        VBPtr = std::get<BuffPtr>(VB);
    }
    if (std::holds_alternative<IBCPTR>(IB))
    {
        IBPtr = std::get<IBCPTR>(IB);
    } else if (std::holds_alternative<BuffPtr>(IB))
    {
        IBPtr = std::get<BuffPtr>(IB);
    }
    if (std::holds_alternative<SWICPTR>(SWI))
    {
        SWIPtr = std::get<SWICPTR>(SWI);
    } else if (std::holds_alternative<BuffPtr>(SWI))
    {
        SWIPtr = std::get<BuffPtr>(SWI);
    }

    MasterChunk->w_chunk(fsL_HEADER, &Header, sizeof(Header));
    MasterChunk->open_chunk(fsL_TYPECHUNKS);
    MasterChunk->w_enum(GeomVanillaType::Chunked);
    MasterChunk->w_u32(ChunksNum);
    bool HasVB = true; u32 VBSize = 0;
    bool HasIB = true; u32 IBSize = 0;
    bool HasSWI = true; u32 SWISize = 0;
    {
        HasVB = !std::holds_alternative<std::monostate>(VBPtr);
        if (HasVB)
        {
            if (std::holds_alternative<VBCPTR>(VBPtr))
            {
                auto Ptr = std::get<VBCPTR>(VBPtr);
                VBSize = Ptr->size();
                HasVB = VBSize > sizeof(u32);
            } else if (std::holds_alternative<BuffPtr>(VBPtr))
            {
                auto& data = *std::get<BuffPtr>(VBPtr);
                HasVB = !data.empty();
                VBSize = data.size();
            }
        }
    }
    {
        HasIB = !std::holds_alternative<std::monostate>(IBPtr);
        if (HasIB)
        {
            if (std::holds_alternative<IBCPTR>(IBPtr))
            {
                auto Ptr = std::get<IBCPTR>(IBPtr);
                IBSize = Ptr->size();
                HasIB = IBSize > sizeof(u32);
            } else if (std::holds_alternative<BuffPtr>(IBPtr))
            {
                auto& data = *std::get<BuffPtr>(IBPtr);
                HasIB = !data.empty();
                IBSize = data.size();
            }
        }
    }
    {
        HasSWI = !std::holds_alternative<std::monostate>(SWIPtr);
        if (HasSWI)
        {
            if (std::holds_alternative<SWICPTR>(SWIPtr))
            {
                auto Ptr = std::get<SWICPTR>(SWIPtr);
                SWISize = Ptr->size();
                HasSWI = SWISize > sizeof(u32);
            } else if (std::holds_alternative<BuffPtr>(SWIPtr))
            {
                auto& data = *std::get<BuffPtr>(SWIPtr);
                HasSWI = !data.empty();
                SWISize = data.size();
            }
        }
    }
    MasterChunk->w_u8(HasVB);
    MasterChunk->w_u8(HasIB);
    MasterChunk->w_u8(HasSWI);
    if (HasVB)
    {
        MasterChunk->w_u32(VBSize);
    }
    if (HasIB)
    {
        MasterChunk->w_u32(IBSize);
    }
    if (HasSWI)
    {
        MasterChunk->w_u32(SWISize);
    }
    MasterChunk->close_chunk();

    xr_vector<u8> VBBuffer;
    if (std::holds_alternative<VBCPTR>(VBPtr))
    {
        auto Ptr = std::get<VBCPTR>(VBPtr);
        VBBuffer.resize(Ptr->size());
        CBufferMemoryWriter writer(VBBuffer);
        Ptr->Save(writer);
    } else if (std::holds_alternative<BuffPtr>(VBPtr))
    {
        VBBuffer = *std::get<BuffPtr>(VBPtr);
    }
    VERIFY(!HasVB || !VBBuffer.empty());
    auto VBBufferChunkNum = VBBuffer.size()/ChunksNum + VBBuffer.size()%ChunksNum;
    size_t CurrentPosVBBuffer = 0;
    
    xr_vector<u8> IBBuffer;
    if (std::holds_alternative<IBCPTR>(IBPtr))
    {
        auto Ptr = std::get<IBCPTR>(IBPtr);
        IBBuffer.resize(Ptr->size());
        CBufferMemoryWriter writer(IBBuffer);
        Ptr->Save(writer);
    } else if (std::holds_alternative<BuffPtr>(IBPtr))
    {
        IBBuffer = *std::get<BuffPtr>(IBPtr);
    }
    VERIFY(!HasIB || !IBBuffer.empty());
    auto IBBufferChunkNum = IBBuffer.size()/ChunksNum + IBBuffer.size()%ChunksNum;
    size_t CurrentPosIBBuffer = 0;
    
    xr_vector<u8> SWIBuffer;
    if (std::holds_alternative<SWICPTR>(SWIPtr))
    {
        auto Ptr = std::get<SWICPTR>(SWIPtr);
        SWIBuffer.resize(Ptr->size());
        CBufferMemoryWriter writer(SWIBuffer);
        Ptr->Save(writer);
    } else if (std::holds_alternative<BuffPtr>(SWIPtr))
    {
        SWIBuffer = *std::get<BuffPtr>(SWIPtr);
    }
    VERIFY(!HasSWI || !SWIBuffer.empty());
    auto SWIBufferChunkNum = SWIBuffer.size()/ChunksNum + SWIBuffer.size()%ChunksNum;
    size_t CurrentPosSWIBuffer = 0;

    for (int i = 0; i < ChunksNum; ++i)
    {
        xr_stack_string_path chunk_path = FileName;
        chunk_path.push_back('_');
        chunk_path.append(std::to_string(i).c_str());
        chunk_path.append(Extension);

        auto Chunk = FS.wg_open(chunk_path.c_str());
        if (!I_ASSERT(Chunk))
        {
            return false;
        }

        Chunk->w_chunk(fsL_HEADER, &Header, sizeof(Header));
        Chunk->open_chunk(fsL_TYPECHUNKS);
        Chunk->w_enum(GeomVanillaType::ChunkedData);
        Chunk->close_chunk();

        auto process_func = [&](const xr_vector<u8>& Buffer, size_t ChunkDataSize, size_t& CurrentPos, fsL_Chunks ChunkType) -> bool
        {
            if (!IVERIFY(CurrentPos < Buffer.size()))
            {
                return false;
            }
            auto DeltaData = std::min(ChunkDataSize, Buffer.size() - CurrentPos);
            Chunk->open_chunk(ChunkType);
            Chunk->w(Buffer.data()+CurrentPos, DeltaData);
            Chunk->close_chunk();
            CurrentPos+=DeltaData;
            return true;
        };

        if (HasVB && !IVERIFY(
            process_func(VBBuffer, VBBufferChunkNum, CurrentPosVBBuffer, fsL_VB)))
        {
            return false;
        }

        if (HasIB && !IVERIFY(
            process_func(IBBuffer, IBBufferChunkNum, CurrentPosIBBuffer, fsL_IB)))
        {
            return false;
        }

        if (HasSWI && !IVERIFY(
            process_func(SWIBuffer, SWIBufferChunkNum, CurrentPosSWIBuffer, fsL_SWIS)))
        {
            return false;
        }
    }

    return true;
}

bool CGeomVanillaChunkedFormat::Read(xr_string_view FileName, xr_string_view Extension)
{
    xr_stack_string_path path = FileName;
    path.append(Extension);

    auto MasterChunk = FS.rg_open(path.c_str());
    if (!I_ASSERT(MasterChunk))
    {
        return false;
    }

    
    if (!I_ASSERT(MasterChunk->r_chunk(fsL_HEADER, &Header)))
    {
        return false;
    }
    bool HasVB, HasIB, HasSWI;
    u32 VBSize, IBSize, SWISize;
    {
        auto MasterTypeChunk = MasterChunk->open_chunk(fsL_TYPECHUNKS);
        if (!IVERIFY(MasterTypeChunk))
        {
            return false;    
        }
        if (!IVERIFY(MasterTypeChunk->r_enum<GeomVanillaType>() == GeomVanillaType::Chunked))
        {
            MasterTypeChunk->close();
            return false;
        }
        ChunksNum = MasterTypeChunk->r_u32();
        HasVB = MasterTypeChunk->r_u8();
        HasIB = MasterTypeChunk->r_u8();
        HasSWI = MasterTypeChunk->r_u8();
        if (HasVB)
        {
            VBSize = MasterTypeChunk->r_u32();
        }
        if (HasIB)
        {
            IBSize = MasterTypeChunk->r_u32();
        }
        if (HasSWI)
        {
            SWISize = MasterTypeChunk->r_u32();
        }
        MasterTypeChunk->close();
    }

    CMultiReader* VBPtr = nullptr;
    CMultiReader* IBPtr = nullptr;
    CMultiReader* SWIPtr = nullptr;
    if (HasVB)
    {
        VERIFY(std::holds_alternative<std::monostate>(VB));
        VBPtr = new CMultiReader();
        VB = VBPtr;
    }
    if (HasIB)
    {
        VERIFY(std::holds_alternative<std::monostate>(IB));
        IBPtr = new CMultiReader();
        IB = IBPtr;
    }
    if (HasSWI)
    {
        VERIFY(std::holds_alternative<std::monostate>(SWI));
        SWIPtr = new CMultiReader();
        SWI = SWIPtr;
    }

    for (size_t i = 0; i < ChunksNum; ++i)
    {
        xr_stack_string_path chunk_path = FileName;
        chunk_path.push_back('_');
        chunk_path.append(std::to_string(i).c_str());
        chunk_path.append(Extension);

        auto Chunk = FS.r_open(chunk_path.c_str());
        if (!I_ASSERT(Chunk))
        {
            return false;
        }

        ChunkHeader TempHeader = Header;
        if (!I_ASSERT(Chunk->r_chunk(fsL_HEADER, &TempHeader)))
        {
            Chunk->close();
            return false;
        }
        {
            auto TypeChunk = Chunk->open_chunk(fsL_TYPECHUNKS);
            if (!IVERIFY(TypeChunk))
            {
                return false;
            }
            if (!IVERIFY(TypeChunk->r_enum<GeomVanillaType>() == GeomVanillaType::ChunkedData))
            {
                TypeChunk->close();
                Chunk->close();
                return false;
            }
            TypeChunk->close();
        }

        FileReader.push_back(Chunk);

        auto process_func = [&](CMultiReader& Reader, fsL_Chunks ChunkType) -> bool
        {
            auto DataChunk = Chunk->open_chunk(ChunkType);
            if (!DataChunk)
            {
                return false;
            }
            Reader.AppendReader(*DataChunk);
            return true;
        };

        if (HasVB && !IVERIFY(process_func(*std::get<CMultiReader*>(VB), fsL_VB)))
        {
            return false;
        }
        if (HasIB && !IVERIFY(process_func(*std::get<CMultiReader*>(IB), fsL_IB)))
        {
            return false;
        }
        if (HasSWI && !IVERIFY(process_func(*std::get<CMultiReader*>(SWI), fsL_SWIS)))
        {
            return false;
        }
    }

    if (!IVERIFY(!HasVB || VBPtr->length() == VBSize)
        || !IVERIFY(!HasIB || IBPtr->length() == IBSize)
        || !IVERIFY(!HasSWI || SWIPtr->length() == SWISize)
        )
    {
        return false;
    }

    return true;
}

void CGeomVanillaChunkedFormat::AddVBData(const VBContainerBase& data)
{
    VERIFY(std::holds_alternative<std::monostate>(VB));
    VB = &data;
}

void CGeomVanillaChunkedFormat::AddIBData(const IBContainerBase& data)
{
    
    VERIFY(std::holds_alternative<std::monostate>(IB));
    IB = &data;
}

void CGeomVanillaChunkedFormat::AddSWIData(const SWIContainerBase& data)
{
    
    VERIFY(std::holds_alternative<std::monostate>(SWI));
    SWI = &data;
}

void CGeomVanillaChunkedFormat::AddVBData(const xr_vector<u8>& data)
{
    VERIFY(std::holds_alternative<std::monostate>(VB));
    VB = &data;
}

void CGeomVanillaChunkedFormat::AddIBData(const xr_vector<u8>& data)
{
    
    VERIFY(std::holds_alternative<std::monostate>(IB));
    IB = &data;
}

void CGeomVanillaChunkedFormat::AddSWIData(const xr_vector<u8>& data)
{
    
    VERIFY(std::holds_alternative<std::monostate>(SWI));
    SWI = &data;
}

IReaderBase& CGeomVanillaChunkedFormat::GetVBData() const
{
    VERIFY(std::holds_alternative<CMultiReader*>(VB));
    return *std::get<CMultiReader*>(VB);
}

IReaderBase& CGeomVanillaChunkedFormat::GetIBData() const
{
    VERIFY(std::holds_alternative<CMultiReader*>(IB));
    return *std::get<CMultiReader*>(IB);
}

IReaderBase& CGeomVanillaChunkedFormat::GetSWIData() const
{
    VERIFY(std::holds_alternative<CMultiReader*>(SWI));
    return *std::get<CMultiReader*>(SWI);
}

bool CGeomVanillaChunkedFormat::HasVBData() const
{
    return std::holds_alternative<CMultiReader*>(VB);
}

bool CGeomVanillaChunkedFormat::HasIBData() const
{
    return std::holds_alternative<CMultiReader*>(IB);
}

bool CGeomVanillaChunkedFormat::HasSWIData() const
{
    return std::holds_alternative<CMultiReader*>(SWI);
}

XRCORE_API xr_unique_ptr<IFormat> XRay::Geom::Read(LPCSTR Initial, xr_string_view Filename, xr_string_view Extension)
{
    xr_stack_string_path Path = Filename.data();
    if (Initial&&Initial[0])
    {
        FS.update_path(Path,Initial,Filename.data());
    }

    GeomVanillaType Type = GeomVanillaType::Vanilla;
    {
        xr_stack_string_path TempPath = Path;
        TempPath.append(Extension);
        auto Reader = FS.rg_open(TempPath.c_str());
        if (!I_ASSERT(Reader))
        {
            return nullptr;
        }
        if (auto Chunk = Reader->open_chunk(fsL_TYPECHUNKS))
        {
            Type = Chunk->r_enum<GeomVanillaType>();
            Chunk->close();
        }
    }

    switch (Type)
    {
    case GeomVanillaType::Vanilla:
        {
            auto Parsed = new CGeomVanillaFormat();
            if (!I_ASSERT_M(Parsed->Read(Path, Extension), "Unable to read [%s%s]", Path.c_str(), Extension.data()))
            {
                xr_delete(Parsed);
                return nullptr;
            }
            return xr_unique_ptr<IFormat>(Parsed);
        }
    case GeomVanillaType::Chunked:
        {
            auto Parsed = new CGeomVanillaChunkedFormat(1);
            if (!I_ASSERT_M(Parsed->Read(Path, Extension), "Unable to read [%s%s]", Path.c_str(), Extension.data()))
            {
                xr_delete(Parsed);
                return nullptr;
            }
            return xr_unique_ptr<IFormat>(Parsed);
        }
    default:
        {
            I_ASSERT_M(false, "Invalid %s type in [%s]", Extension.data(), Path.c_str());
        }
    }
    
    return nullptr;
}

XRCORE_API xr_unique_ptr<IFormat> XRay::Geom::Read(xr_string_view Filename, xr_string_view Extension)
{
    return Read(nullptr, Filename, Extension);
}

XRCORE_API void XRay::Geom::Write(LPCSTR Initial, xr_string_view Filename, xr_string_view Extension, IFormat& Data)
{
    xr_stack_string_path Path = Filename.data();
    if (Initial&&Initial[0])
    {
        FS.update_path(Path,Initial,Filename.data());
    }
    I_ASSERT(Data.Write(Path, Extension));
}

XRCORE_API void XRay::Geom::Write(xr_string_view Filename, xr_string_view Extension, IFormat& Data)
{
    Write(nullptr, Filename, Extension, Data);
}
