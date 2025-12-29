#pragma once

#include "xrLevel.h"

namespace XRay
{
    struct CForm
    {
        using ChunkHeader = hdrCFORM;

        struct ChunkChunkNum
        {
            u32 ChunkNumber;
        };

        struct ChunkObjectData
        {
            xr_vector<Fvector> Verts;
            xr_vector<CDB::TRI> Tris;
        };

        class IFormat
        {
        protected:
            friend struct CForm;
            virtual bool Write(xr_string_view FileName) = 0;
            virtual bool Read(xr_string_view FileName) = 0;
            
        public:
            virtual ~IFormat() = default;
            
            virtual void AddStaticGeom(xr_span<Fvector> Vertices, xr_span<CDB::TRI> Tris) = 0;
            // Add other functions for future instanced cform
        };

        class CFormatVanillaChunked;
        class CFormatVanilla : public IFormat
        {
            friend class CFormatVanillaChunked;
            friend struct CForm;
            
            ChunkHeader Header;
            ChunkObjectData Data;
            
            bool Write(xr_string_view FileName) override;
            bool Read(xr_string_view FileName) override;
        public:
            CFormatVanilla();
            
            void AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris) override;
        
            ICF ChunkHeader& GetHeader() { return Header; }
            ICF const ChunkHeader& GetHeader() const { return Header; }
        };

        class CFormatVanillaChunked : public IFormat
        {
            friend struct CForm;
            
            ChunkHeader Header;
            xr_vector<CFormatVanilla> Data;
            
            bool Write(xr_string_view FileName) override;
            bool Read(xr_string_view FileName) override;
        public:
            CFormatVanillaChunked(u32 ChunkNumber);
        
            void AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris) override;
        
            ICF ChunkHeader& GetHeader() { return Header; }
            ICF const ChunkHeader& GetHeader() const { return Header; }
        };

        static xr_unique_ptr<IFormat> Read(xr_string_view Filename);
        static void Write(xr_string_view Filename, IFormat& Data);
    };
}
