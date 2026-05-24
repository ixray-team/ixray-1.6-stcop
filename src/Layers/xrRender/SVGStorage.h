#pragma once

#include "../../Include/xrRender/SVGTypes.h"
#include "../../xrCore/xrstring.h"
#include "../../xrCore/_stl_extensions.h"
#include "TextureAtlas.h"

#include <lunasvg.h>

#include <memory>

enum class eSVGStorageFlags : u32
{
	// new requested atlases can't be created and you get runtime exception (bad_alloc if memory was run out)
	kFeatureSVGStorage_Static_Allocation = 1 << 1,

	// if there's big amount of resources and we can't place on static storage we allocate more space and thus atlases
	kFeatureSVGStorage_Dynamic_Allocation = 1 << 2
};

constexpr const char* _kSVGStorage_DefaultSVGTextureSubPathName = "ui" PLATFORM_SLASH_STR "ui_vector_error.svg";
constexpr const char* _kSVGStorage_DefaultAtlasName = "svgdefaultatlas_";
constexpr u32 _kSVGStorage_DefaultAtlasID = 10;
constexpr unsigned char _kSVGStorage_MaxAtlasPlacement = 4;
constexpr unsigned char _kSVGStorage_MaxElementsPerAtlas = 8;
constexpr u32 _kSVGStorage_DocumentLruCapacity = 16;
constexpr u32 _kSVGStorage_DefaultMaxRasterPixels = 1024;

class ECORE_API CSVGStorage
{
public:
#ifdef DEBUG
	struct SvgDebugCacheTableRow
	{
		xr_string tableKey;
		u32 variantCount{};
		u32 totalRasterPixels{};
		u64 lastAccessSeq{};
	};
#endif

	struct AtlasConnection
	{
		char atlas_ids[_kSVGStorage_MaxAtlasPlacement]{ -1,-1,-1,-1 };
		char elements_per_atlas[_kSVGStorage_MaxElementsPerAtlas * _kSVGStorage_MaxAtlasPlacement]{ -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1 };

		bool isValid(void) const
		{
			for (int i = 0; i < _kSVGStorage_MaxAtlasPlacement; ++i)
			{
				if (atlas_ids[i] != char(-1))
					return true;
			}
			return false;
		}
	};

	CSVGStorage(u32 flags);
	~CSVGStorage();

	void init();
	void uninit();

	constexpr unsigned char get_static_size() const;

	u32 get_size() const;

	u32 init_atlas(u32 w, u32 h, const char* pTextureName, CTextureAtlas& instance, bool generate_id = false);

	CTextureAtlas* get_atlas(u32 id);
	const CTextureAtlas* get_atlas(u32 id) const;
	const std::pmr::vector<CTextureAtlas>& get_atlases(void) const;

	const FactoryPtr<IUIShader>& get_shader(const std::string_view& subpath, float requested_width, float requested_height, SVGTintRGBA tint = {});
	const FactoryPtr<IUIShader>& get_default_shader();

	Frect get_uv(const std::string_view& subpath, float requested_width, float requested_height, SVGTintRGBA tint = {});

	void BeginRasterFrameCache();

	void PrecacheSVG(std::string_view subpath, float width, float height, SVGTintRGBA tint = {});
	void PrecacheSVGList(const xr_vector<xr_string>& paths, float width, float height, SVGTintRGBA tint = {});

	void InvalidateAllSvgDocuments();
	void ReloadSVG(std::string_view subpath);

	void SetRasterUserScale(float scale);
	float GetRasterUserScale() const { return m_rasterUserScale; }
	void SetMaxRasterPixels(u32 pixels);
	u32 GetMaxRasterPixels() const { return m_maxRasterPixels; }

#ifdef DEBUG
	void DebugCollectSvgCacheRows(xr_vector<SvgDebugCacheTableRow>& out) const;
	u64 DebugGetSvgDocCacheHits() const { return m_debugDocCacheHits; }
	u64 DebugGetSvgDocCacheMisses() const { return m_debugDocCacheMisses; }
	u64 DebugGetSvgNewAtlasAllocCount() const { return m_debugNewAtlasAllocCount; }
	u64 DebugGetSvgRenderToBitmapNsAccum() const { return m_debugRenderToBitmapNsAccum; }
	u32 DebugGetSvgRenderToBitmapSamples() const { return m_debugRenderToBitmapSamples; }
	void DebugResetSvgMetrics();
#endif

private:
	struct SvgDocumentLruEntry
	{
		xr_string pathKey;
		xr_string resolvedFsPath;
		std::unique_ptr<lunasvg::Document> doc;
		time_t fileMtimeAtLoad{};
		u32 fileSizeAtLoad{};
		u64 lastAccessSeq{};
	};

	struct SvgAtlasEntryStats
	{
		u32 variantCount{};
		u32 totalRasterPixels{};
		u64 lastAccessSeq{};
	};

	struct SvgRasterFrameCacheEntry
	{
		const FactoryPtr<IUIShader>* pShader{};
		Frect uv{};
	};

	void init_default();
	void init_default_atlas();
	void init_default_shader();
	u32 generate_id();

	AtlasConnection try_allocate(const xr_string& atlasTableKey, const std::string_view& filesystemSubpath, float requested_width, float requested_height, AtlasConnection* p_existed, SVGTintRGBA tint);

	AtlasConnection allocate(const xr_string& atlasTableKey, const std::string_view& filesystemSubpath, float requested_width, float requested_height, SVGTintRGBA tint);

	bool add_data(const xr_string& atlasTableKey, const std::string_view& filesystemSubpath, float requested_width, float requested_height, CTextureAtlas& atlas, AtlasConnection& connection, SVGTintRGBA tint);

	bool try_add_data(const xr_string& atlasTableKey, const std::string_view& filesystemSubpath, float requested_width, float requested_height, const CTextureAtlas::element_lookupid_type atlas_lookup_id, CTextureAtlas& atlas, AtlasConnection& connection, SVGTintRGBA tint);

	ESVGLoadResult get_bitmap(const std::string_view& filesystemSubpath, float requested_width, float requested_height, lunasvg::Bitmap* bmp, SVGTintRGBA tint);

	static xr_string MakeSubpathKey(const std::string_view& subpath);
	xr_string BuildAtlasTableKey(const std::string_view& filesystemSubpath, SVGTintRGBA tint) const;

	void NormalizeRasterRequest(float requestedWidth, float requestedHeight, int& outW, int& outH) const;

	void ResolveRasterDimensions(const xr_string& atlasTableKey, float requestedWidth, float requestedHeight, int& useW, int& useH) const;

	bool ConnectionHasExactPixelSize(const AtlasConnection& connection, int pixelW, int pixelH) const;
	bool FindNearestCachedPixelSize(const AtlasConnection& connection, int pixelW, int pixelH, int& outW, int& outH) const;

	bool TryLookupUvForSize(AtlasConnection& connection, int pixelW, int pixelH, Frect& outUv, bool useNearest);

	void FillDefaultAtlasUvForSize(int useW, int useH, Frect& result) const;

	void TouchAtlasEntryStats(const xr_string& atlasTableKey);
	void RegisterNewRasterVariant(const xr_string& atlasTableKey, int pixelW, int pixelH);

	void ResolveSvgRasterDraw(const std::string_view& filesystemSubpath, float requestedWidth, float requestedHeight, SVGTintRGBA tint, const FactoryPtr<IUIShader>** outShader, Frect* outUv);

	u64 MakeFrameCacheKey(const xr_string& atlasTableKey, int pixelW, int pixelH) const;

	SvgDocumentLruEntry* AccessDocumentLru(const xr_string& pathKey, const char* resolvedPathForValidate);
	void InsertDocumentLru(const xr_string& pathKey, const char* resolvedPath, std::unique_ptr<lunasvg::Document> doc, time_t mtime, u32 fileSize);
	void EraseDocumentLruByPathKey(const xr_string& pathKey);
	void LogSvgLoadFailureOnce(const xr_string& pathKey, ESVGLoadResult code);

#ifdef DEBUG
	void DebugRecordRenderToBitmapTime(u64 deltaTicks);
#endif

#ifdef DEBUG
	bool m_init_was_called;
#endif
	u32 m_atlas_index_generator;
	FactoryPtr<IUIShader>* m_p_default_shader;
	FactoryPtr<IUIShader> m_empty_default_shader;
	CTextureAtlas m_default_atlas;
	unsigned char m_static_storage[calculate_reserve_count(sizeof(CTextureAtlas), static_cast<size_t>(_kRenderBackend_SVGStorageSizeInitial))];
	std::pmr::monotonic_buffer_resource m_ss_wrapper;
	std::pmr::vector<CTextureAtlas> m_storage_atlases;
	xr_hash_map<u32, u32> m_atlasIdToStorageIndex;
	// single-threaded: all SVG atlas and document caches are used only from the render / resource thread unless guarded externally
	xr_hash_map<xr_string, AtlasConnection> m_storage_textures;
	xr_hash_map<xr_string, SvgAtlasEntryStats> m_atlasEntryStats;
	xr_vector<SvgDocumentLruEntry> m_docLruSlots;
	xr_string m_readBuffer;
	xr_hash_map<u64, SvgRasterFrameCacheEntry> m_frameShaderUvCache;
	xr_hash_set<xr_string> m_loggedSvgFailures;
	float m_rasterUserScale{ 1.f };
	u32 m_maxRasterPixels{ _kSVGStorage_DefaultMaxRasterPixels };
	u64 m_docLruSeqCounter{};
	u64 m_globalAccessSeq{};

#ifdef DEBUG
	u64 m_debugDocCacheHits{};
	u64 m_debugDocCacheMisses{};
	u64 m_debugNewAtlasAllocCount{};
	u64 m_debugRenderToBitmapNsAccum{};
	u32 m_debugRenderToBitmapSamples{};
#endif
};
