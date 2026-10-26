#pragma once

#include "../../../xrEngine/xrLevel.h"
#include "../../Collision/xrCDB.h"

namespace XRay::CForm
{
    using ChunkHeader = hdrCFORM;
    
    struct XRCORE_API ChunkChunkNum
    {
        u32 ChunkNumber;
    };

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
    	virtual void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, CDB::MODEL& Collsion, u16 Sector) = 0;
        virtual void GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const = 0;
    	virtual void ReadData(CDB::MODEL& Model, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const = 0;
        // Add other functions for future instanced cform
    
        ChunkHeader& GetHeader();
        const ChunkHeader& GetHeader() const;

        u32 GetFileHash() const;// {return FileHash;}
    };

    class CFormatVanillaChunked;
    class XRCORE_API CFormatVanilla : public IFormat
    {
        friend class CFormatVanillaChunked;
    	IReader* FileReader = nullptr;
    	Fvector* VertsPtr;
    	CDB::TRI* TrisPtr;
        
    public:
        CFormatVanilla();
    	~CFormatVanilla() override;
        
        bool Write(xr_string_view FileName) override;
        bool Read(xr_string_view FileName) override;
        
        void AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris) override;
    	void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, CDB::MODEL& Collsion, u16 Sector) override {R_ASSERT(false);}
        void GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const override;
    	void ReadData(CDB::MODEL& Model, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const override;
    };

    class XRCORE_API CFormatVanillaChunked : public IFormat
    {        
        xr_vector<CFormatVanilla> Data;
    public:
        CFormatVanillaChunked(u32 ChunkNumber);
        
        bool Write(xr_string_view FileName) override;
        bool Read(xr_string_view FileName) override;
    
        void AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris) override;
    	void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, CDB::MODEL& Collsion, u16 Sector) override {R_ASSERT(false);}
        void GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const override;
    	void ReadData(CDB::MODEL& Model, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const override;
    };

	class XRCORE_API CFormatInstanced : public IFormat
	{
		IReader* FileReader = nullptr;
		Fvector* VertsPtr = nullptr;
		CDB::TRI* TrisPtr = nullptr;
		struct InstanceDataPacked
		{
			Fmatrix xform;
			Fbox AABB;
			u16 Sector;
		};
		xr_hash_map<shared_str, xr_vector<InstanceDataPacked>> instances = {};
		xr_hash_map<shared_str, CDB::MODEL*> Models = {};
		
		CDB::MODEL* ReadInstance(shared_str Path, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const;
	        
	public:
		CFormatInstanced();
		~CFormatInstanced() override;
	        
		bool Write(xr_string_view FileName) override;
		bool Read(xr_string_view FileName) override;
	        
		void AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris) override;
    	void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, CDB::MODEL& Collsion, u16 Sector) override;
		void GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const override;
		void ReadData(CDB::MODEL& Model, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const override;
	};

    XRCORE_API xr_unique_ptr<IFormat> Read(const char* Initial, xr_string_view Filename);
    XRCORE_API xr_unique_ptr<IFormat> Read(xr_string_view Filename);
    XRCORE_API void Write(const char* Initial, xr_string_view Filename, IFormat& Data);
    XRCORE_API void Write(xr_string_view Filename, IFormat& Data);
};

