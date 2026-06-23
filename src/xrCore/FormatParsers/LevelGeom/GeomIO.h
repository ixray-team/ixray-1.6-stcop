#pragma once

#include <variant>

#include "vbm.h"
#include "xrEngine/xrLevel.h"

struct FSlideWindowItem;

namespace XRay::Geom
{
    using ChunkHeader = hdrLEVEL;
    
    using VBCPTR = const VBContainerBase*;
    using IBCPTR = const IBContainerBase*;
    using SWICPTR = const SWIContainerBase*;
    using BuffPtr =  const xr_vector<u8>*;

    class XRCORE_API IFormat
    {
    protected:        
        ChunkHeader Header = {};
        
    public:
        IFormat();
        virtual ~IFormat() = default;

        IFormat(const IFormat&) = delete;
        IFormat& operator=(const IFormat&) = delete;
        IFormat(IFormat&&) = default;
        IFormat& operator=(IFormat&&) = default;
        
        virtual bool Write(xr_string_view FileName, xr_string_view Extension) = 0;
        virtual bool Read(xr_string_view FileName, xr_string_view Extension) = 0;
        
        virtual void AddVBData(const VBContainerBase& data) = 0;
        virtual void AddIBData(const IBContainerBase& data) = 0;
        virtual void AddSWIData(const SWIContainerBase& data) = 0;
        
        virtual void AddVBData(const xr_vector<u8>& data) = 0;
        virtual void AddIBData(const xr_vector<u8>& data) = 0;
        virtual void AddSWIData(const xr_vector<u8>& data) = 0;

        virtual IReaderBase& GetVBData() const = 0;
        virtual IReaderBase& GetIBData() const = 0;
        virtual IReaderBase& GetSWIData() const = 0;

        virtual bool HasVBData() const = 0;
        virtual bool HasIBData() const = 0;
        virtual bool HasSWIData() const = 0;
    
        ChunkHeader& GetHeader() {return Header;}
        const ChunkHeader& GetHeader() const {return Header;}
    };

    class XRCORE_API CGeomVanillaFormat : public IFormat
    {        
        std::variant<std::monostate, VBCPTR, BuffPtr, IReader*> VB;
        std::variant<std::monostate, IBCPTR, BuffPtr, IReader*> IB;
        std::variant<std::monostate, SWICPTR, BuffPtr, IReader*> SWI;
        IReader* FileReader = nullptr;
        
    public:
        ~CGeomVanillaFormat() override;

        virtual bool Write(xr_string_view FileName, xr_string_view Extension) override;
        virtual bool Read(xr_string_view FileName, xr_string_view Extension) override;
        
        virtual void AddVBData(const VBContainerBase& data) override;
        virtual void AddIBData(const IBContainerBase& data) override;
        virtual void AddSWIData(const SWIContainerBase& data) override;
        
        virtual void AddVBData(const xr_vector<u8>& data) override;
        virtual void AddIBData(const xr_vector<u8>& data) override;
        virtual void AddSWIData(const xr_vector<u8>& data) override;

        virtual IReaderBase& GetVBData() const override;
        virtual IReaderBase& GetIBData() const override;
        virtual IReaderBase& GetSWIData() const override;

        virtual bool HasVBData() const override;
        virtual bool HasIBData() const override;
        virtual bool HasSWIData() const override;
    };

    class XRCORE_API CGeomVanillaChunkedFormat : public IFormat
    {
        std::variant<std::monostate, VBCPTR, BuffPtr, CMultiReader*> VB = {};
        std::variant<std::monostate, IBCPTR, BuffPtr, CMultiReader*> IB = {};
        std::variant<std::monostate, SWICPTR, BuffPtr, CMultiReader*> SWI = {};
        xr_vector<IReader*> FileReader = {};
        size_t ChunksNum = 0;
        
    public:
        CGeomVanillaChunkedFormat(size_t ChunksNum): ChunksNum(ChunksNum) {}
        ~CGeomVanillaChunkedFormat() override;
        
        virtual bool Write(xr_string_view FileName, xr_string_view Extension) override;
        virtual bool Read(xr_string_view FileName, xr_string_view Extension) override;
        
        virtual void AddVBData(const VBContainerBase& data) override;
        virtual void AddIBData(const IBContainerBase& data) override;
        virtual void AddSWIData(const SWIContainerBase& data) override;
        
        virtual void AddVBData(const xr_vector<u8>& data) override;
        virtual void AddIBData(const xr_vector<u8>& data) override;
        virtual void AddSWIData(const xr_vector<u8>& data) override;

        virtual IReaderBase& GetVBData() const override;
        virtual IReaderBase& GetIBData() const override;
        virtual IReaderBase& GetSWIData() const override;

        virtual bool HasVBData() const override;
        virtual bool HasIBData() const override;
        virtual bool HasSWIData() const override;
        
    };

    XRCORE_API xr_unique_ptr<IFormat> Read(const char* Initial, xr_string_view Filename, xr_string_view Extension);
    XRCORE_API xr_unique_ptr<IFormat> Read(xr_string_view Filename, xr_string_view Extension);
    XRCORE_API void Write(const char* Initial, xr_string_view Filename, xr_string_view Extension, IFormat& Data);
    XRCORE_API void Write(xr_string_view Filename, xr_string_view Extension, IFormat& Data);
}
