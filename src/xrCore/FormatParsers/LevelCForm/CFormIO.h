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
    	virtual void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, CDB::MODEL& Collsion, u16 RenderSector, u32 StreamingSector) = 0;
        virtual void GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const = 0;
    	virtual void ReadData(CDB::MODEL& Model, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const = 0;
        // Add other functions for future instanced cform

        // Streaming support (see CFormatStreamed / CFormatStreamedInstanced). Non-streamed
        // formats keep the defaults below (whole mesh loaded eagerly via ReadData()).
        virtual bool IsStreamed() const { return false; }
        virtual float GetTileSize() const { return 0.f; }
        // Loads a single N*N meter automatic-streaming tile (StreamedSectorID == 0) on demand.
        virtual bool LoadStreamedTile(s32 TileX, s32 TileZ, CDB::MODEL& OutModel, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const { R_ASSERT(false); return false; }
        // Loads a whole manually-streamed sector (StreamedSectorID != 0) on demand.
        virtual bool LoadStreamedSector(u32 SectorID, CDB::MODEL& OutModel, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const { R_ASSERT(false); return false; }
        // Enumerates the manual (non-zero) streaming sector IDs present in this cform.
        virtual void GetStreamedSectorIDs(xr_vector<u32>& OutIDs) const { }
    
        ChunkHeader& GetHeader();
        const ChunkHeader& GetHeader() const;

        u32 GetFileHash() const;// {return FileHash;}
    };

    class CFormatVanillaChunked;
    class XRCORE_API CFormatVanilla : public IFormat
    {
        friend class CFormatVanillaChunked;
    	CReaderGuarded FileReader = nullptr;
    	Fvector* VertsPtr;
    	CDB::TRI* TrisPtr;
        
    public:
        CFormatVanilla();
    	~CFormatVanilla() override;
    	
    	CFormatVanilla(const CFormatVanilla&) = delete;
    	CFormatVanilla& operator=(const CFormatVanilla&) = delete;
    	
    	CFormatVanilla(CFormatVanilla&& other) noexcept
    	{
    		FileReader = std::move(other.FileReader);
    		other.FileReader = nullptr;
    		VertsPtr = other.VertsPtr;
    		TrisPtr = other.TrisPtr;
    		other.VertsPtr = nullptr;
    		other.TrisPtr = nullptr;
    	}
    	
    	CFormatVanilla& operator=(CFormatVanilla&& other) noexcept
    	{
    		FileReader = std::move(other.FileReader);
    		other.FileReader = nullptr;
    		VertsPtr = other.VertsPtr;
    		TrisPtr = other.TrisPtr;
    		other.VertsPtr = nullptr;
    		other.TrisPtr = nullptr;
    		return *this;
    	}
        
        bool Write(xr_string_view FileName) override;
        bool Read(xr_string_view FileName) override;
        
        void AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris) override;
    	void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, CDB::MODEL& Collsion, u16 RenderSector, u32 StreamingSector) override {R_ASSERT(false);}
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
    	void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, CDB::MODEL& Collsion, u16 RenderSector, u32 StreamingSector) override {R_ASSERT(false);}
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
			u32 StreamingSector;
			u16 RenderSector;
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
    	void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, CDB::MODEL& Collsion, u16 RenderSector, u32 StreamingSector) override;
		void GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const override;
		void ReadData(CDB::MODEL& Model, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const override;
	};

	// A single N*N meter grid cell of an automatic-streaming (StreamedSectorID == 0) sector.
	struct XRCORE_API StreamedTileID
	{
		s32 X = 0;
		s32 Z = 0;

		bool operator==(const StreamedTileID& Other) const { return X == Other.X && Z == Other.Z; }
	};

	// Splits a single level cform mesh into a grid of N*N meter tiles (auto-streamed,
	// StreamedSectorID == 0) plus separate whole-mesh files for manually-streamed sectors
	// (StreamedSectorID != 0, e.g. underground levels). Every tile/sector sub-file has the
	// same on-disk layout as CFormatVanilla. The master file only stores lightweight
	// metadata (tile grid size, list of tiles/sectors present) so it can be read up-front
	// without pulling in any geometry; geometry is pulled in on-demand via
	// LoadStreamedTile()/LoadStreamedSector().
	class XRCORE_API CFormatStreamed : public IFormat
	{
	protected:
		struct TileGeom
		{
			xr_vector<Fvector> Verts;
			xr_vector<CDB::TRI> Tris;
			xr_hash_map<u32, u32> VertRemap;	// original vertex index -> local (per-tile) index
		};

		float TileGridSize = 0.f;
		xr_string BaseFileName;
		xr_vector<StreamedTileID> Tiles;
		xr_vector<u32> Sectors;

		// Populated by AddStaticGeom(), consumed and cleared by Write(). Not used after Read().
		xr_hash_map<u64, TileGeom> TileGeomMap;
		xr_hash_map<u32, TileGeom> SectorGeomMap;

		static xr_string MakeTileFileName(xr_string_view BaseFileName, s32 X, s32 Z);
		static xr_string MakeSectorFileName(xr_string_view BaseFileName, u32 SectorID);

	public:
		CFormatStreamed();
		explicit CFormatStreamed(float InTileGridSize);
		~CFormatStreamed() override = default;

		bool Write(xr_string_view FileName) override;
		bool Read(xr_string_view FileName) override;

		void AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris) override;
		void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, CDB::MODEL& Collsion, u16 RenderSector, u32 StreamingSector) override {R_ASSERT(false);}
		void GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const override { R_ASSERT(false); }
		void ReadData(CDB::MODEL& Model, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const override { R_ASSERT(false); }

		bool IsStreamed() const override { return true; }
		float GetTileSize() const override { return TileGridSize; }
		void SetTileSize(float InTileGridSize) { TileGridSize = InTileGridSize; }
		bool LoadStreamedTile(s32 TileX, s32 TileZ, CDB::MODEL& OutModel, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const override;
		bool LoadStreamedSector(u32 SectorID, CDB::MODEL& OutModel, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const override;
		void GetStreamedSectorIDs(xr_vector<u32>& OutIDs) const override { OutIDs = Sectors; }
	};

	// Same tiling scheme as CFormatStreamed, but every tile/sector sub-file has the same
	// on-disk layout as CFormatInstanced (i.e. can additionally hold references to external
	// MU model collision meshes). An instance is bucketed into a tile by the XZ-center of its
	// world AABB when its Sector == 0, or into the matching manual sector file otherwise
	// (mirrors StreamedSectorID bucketing for the static mesh).
	class XRCORE_API CFormatStreamedInstanced : public IFormat
	{
		struct InstanceDataPacked
		{
			Fmatrix xform;
			Fbox AABB;
			u32 StreamingSector;
			u16 RenderSector;
		};

		struct TileGeom
		{
			xr_vector<Fvector> Verts;
			xr_vector<CDB::TRI> Tris;
			xr_hash_map<u32, u32> VertRemap;
			xr_hash_map<shared_str, xr_vector<InstanceDataPacked>> Instances;
		};

		float TileGridSize = 0.f;
		xr_string BaseFileName;
		xr_vector<StreamedTileID> Tiles;
		xr_vector<u32> Sectors;

		xr_hash_map<u64, TileGeom> TileGeomMap;
		xr_hash_map<u32, TileGeom> SectorGeomMap;
		xr_hash_map<shared_str, CDB::MODEL*> Models = {};

		static xr_string MakeTileFileName(xr_string_view BaseFileName, s32 X, s32 Z);
		static xr_string MakeSectorFileName(xr_string_view BaseFileName, u32 SectorID);

	public:
		CFormatStreamedInstanced();
		explicit CFormatStreamedInstanced(float InTileGridSize);
		~CFormatStreamedInstanced() override = default;

		bool Write(xr_string_view FileName) override;
		bool Read(xr_string_view FileName) override;

		void AddStaticGeom(xr_span<Fvector> Verts, xr_span<CDB::TRI> Tris) override;
		void AddInstanceRef(shared_str Path, const Fmatrix& xform, const Fbox& AABB, CDB::MODEL& Collsion, u16 RenderSector, u32 StreamingSector) override;
		void GetStaticGeom(xr_vector<Fvector>& OutVertices, xr_vector<CDB::TRI>& OutTris) const override { R_ASSERT(false); }
		void ReadData(CDB::MODEL& Model, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const override { R_ASSERT(false); }

		bool IsStreamed() const override { return true; }
		float GetTileSize() const override { return TileGridSize; }
		void SetTileSize(float InTileGridSize) { TileGridSize = InTileGridSize; }
		bool LoadStreamedTile(s32 TileX, s32 TileZ, CDB::MODEL& OutModel, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const override;
		bool LoadStreamedSector(u32 SectorID, CDB::MODEL& OutModel, CDB::build_callback* bc=nullptr, void* bcp=nullptr) const override;
		void GetStreamedSectorIDs(xr_vector<u32>& OutIDs) const override { OutIDs = Sectors; }
	};

    XRCORE_API xr_unique_ptr<IFormat> Read(const char* Initial, xr_string_view Filename);
    XRCORE_API xr_unique_ptr<IFormat> Read(xr_string_view Filename);
    XRCORE_API void Write(const char* Initial, xr_string_view Filename, IFormat& Data);
    XRCORE_API void Write(xr_string_view Filename, IFormat& Data);
};

