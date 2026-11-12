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
		virtual void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, const CDB::MODEL& Collsion, u16 Sector, u32 StreamingSector = 0) = 0;
        virtual void GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const = 0;
    	virtual void ReadData(CDB::MODEL& Model, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const = 0;
        virtual bool IsStreamed() const { return false; }
        virtual float GetTileSize() const { return 0.f; }
        virtual bool HasStreamedTile(s32 TileX, s32 TileZ) const { return false; }
        virtual bool LoadStreamedTile(s32 TileX, s32 TileZ, CDB::MODEL& OutModel, CDB::build_callback* Callback = nullptr, void* Params = nullptr) const { return false; }
        virtual bool LoadStreamedSector(u32 SectorID, CDB::MODEL& OutModel, CDB::build_callback* Callback = nullptr, void* Params = nullptr) const { return false; }
        virtual void GetStreamedSectorIDs(xr_vector<u32>& OutIDs) const { OutIDs.clear(); }
    
        ChunkHeader& GetHeader();
        const ChunkHeader& GetHeader() const;

        u32 GetFileHash() const;// {return FileHash;}
    };

    class CFormatVanillaChunked;
    class XRCORE_API CFormatVanilla : public IFormat
    {
        friend class CFormatVanillaChunked;
		CReaderGuarded FileReader = nullptr;
		Fvector* VertsPtr = nullptr;
		CDB::TRI* TrisPtr = nullptr;
        xr_vector<CDB::TRI> ReadTris;
        
    public:
        CFormatVanilla();
    	~CFormatVanilla() override;
        CFormatVanilla(const CFormatVanilla&) = delete;
        CFormatVanilla& operator=(const CFormatVanilla&) = delete;
        CFormatVanilla(CFormatVanilla&&) noexcept = default;
        CFormatVanilla& operator=(CFormatVanilla&&) noexcept = default;
        
        bool Write(xr_string_view FileName) override;
        bool Read(xr_string_view FileName) override;
        
        void AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris) override;
		void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, const CDB::MODEL& Collsion, u16 Sector, u32 StreamingSector = 0) override {R_ASSERT(false);}
        void GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const override;
    	void ReadData(CDB::MODEL& Model, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const override;
    };

    class XRCORE_API CFormatVanillaChunked : public IFormat
    {        
        xr_vector<CFormatVanilla> Data;
    public:
        CFormatVanillaChunked(u32 ChunkNumber);
        CFormatVanillaChunked(const CFormatVanillaChunked&) = delete;
        CFormatVanillaChunked& operator=(const CFormatVanillaChunked&) = delete;
        
        bool Write(xr_string_view FileName) override;
        bool Read(xr_string_view FileName) override;
    
        void AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris) override;
		void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, const CDB::MODEL& Collsion, u16 Sector, u32 StreamingSector = 0) override {R_ASSERT(false);}
        void GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const override;
    	void ReadData(CDB::MODEL& Model, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const override;
    };

	class XRCORE_API CFormatInstanced : public IFormat
	{
		CReaderGuarded FileReader = nullptr;
		Fvector* VertsPtr = nullptr;
		CDB::TRI* TrisPtr = nullptr;
		xr_vector<CDB::TRI> ReadTris;
		xr_hash_map<shared_str, xr_vector<CDB::TRI>> ReadModelTris;
		struct ModelDataPacked
		{
			size_t VertsCount;
			const Fvector* VertsPtr = nullptr;
			size_t TrisCount;
			const CDB::TRI* TrisPtr = nullptr;
		};
		struct InstanceDataPacked
		{
			Fmatrix xform;
			Fbox AABB;
			u16 Sector;
		};
		xr_hash_map<shared_str, xr_vector<InstanceDataPacked>> instances = {};
		xr_hash_map<shared_str, ModelDataPacked> Models = {};
	        
	public:
		CFormatInstanced();
		~CFormatInstanced() override;
		CFormatInstanced(const CFormatInstanced&) = delete;
		CFormatInstanced& operator=(const CFormatInstanced&) = delete;
	        
		bool Write(xr_string_view FileName) override;
		bool Read(xr_string_view FileName) override;
	        
		void AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris) override;
		void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, const CDB::MODEL& Collsion, u16 Sector, u32 StreamingSector = 0) override;
		void GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const override;
		void ReadData(CDB::MODEL& Model, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const override;
	};

	struct StreamedTileID
	{
		s32 X = 0;
		s32 Z = 0;
	};

	// The master stores the grid and manual sector index; subfiles use existing formats.
	class XRCORE_API CFormatStreamed : public IFormat
	{
		struct InstanceData
		{
			shared_str Path;
			Fmatrix Transform;
			Fbox AABB;
			const CDB::MODEL* Model;
			u16 RenderSector;
			u32 StreamingSector;
		};
		struct TileGeom
		{
			xr_vector<Fvector> Verts;
			xr_vector<CDB::TRI> Tris;
			xr_hash_map<u32, u32> VertRemap;
			xr_vector<InstanceData> Instances;
		};
		float TileGridSize = 100.f;
		xr_string BaseFileName;
		xr_vector<StreamedTileID> Tiles;
		xr_hash_set<u64> TileKeys;
		xr_vector<u32> Sectors;
		xr_hash_map<u64, TileGeom> TileGeomMap;
		xr_hash_map<u32, TileGeom> SectorGeomMap;

		bool LoadChunk(const xr_string& Path, CDB::MODEL& OutModel, CDB::build_callback* Callback, void* Params) const;
		bool WriteChunk(const xr_string& Path, TileGeom& Geometry) const;

	public:
		CFormatStreamed();
		explicit CFormatStreamed(float TileSize);
		bool Write(xr_string_view FileName) override;
		bool Read(xr_string_view FileName) override;
		void AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris) override;
		void AddInstanceRef(shared_str Path, const Fmatrix& Transform, const Fbox& AABB, const CDB::MODEL& Model, u16 RenderSector, u32 StreamingSector = 0) override;
		void GetStaticGeom(xr_vector<Fvector>& Verts, xr_vector<CDB::TRI>& Tris) const override;
		void ReadData(CDB::MODEL& Model, CDB::build_callback* Callback = nullptr, void* Params = nullptr) const override;
		bool IsStreamed() const override { return true; }
		float GetTileSize() const override { return TileGridSize; }
		bool HasStreamedTile(s32 X, s32 Z) const override;
		void SetTileSize(float TileSize);
		bool LoadStreamedTile(s32 X, s32 Z, CDB::MODEL& Model, CDB::build_callback* Callback = nullptr, void* Params = nullptr) const override;
		bool LoadStreamedSector(u32 SectorID, CDB::MODEL& Model, CDB::build_callback* Callback = nullptr, void* Params = nullptr) const override;
		void GetStreamedSectorIDs(xr_vector<u32>& OutIDs) const override { OutIDs = Sectors; }
	};

	class XRCORE_API CFormatStreamedInstanced : public CFormatStreamed
	{
	public:
		CFormatStreamedInstanced();
		explicit CFormatStreamedInstanced(float TileSize);
	};

    XRCORE_API xr_unique_ptr<IFormat> Read(const char* Initial, xr_string_view Filename);
    XRCORE_API xr_unique_ptr<IFormat> Read(xr_string_view Filename);
    XRCORE_API void Write(const char* Initial, xr_string_view Filename, IFormat& Data);
    XRCORE_API void Write(xr_string_view Filename, IFormat& Data);
};
