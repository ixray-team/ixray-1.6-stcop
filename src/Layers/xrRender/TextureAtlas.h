#pragma once
#include "../../xrCore/xr_rtree.h"
#include <memory_resource>

constexpr unsigned char _kRenderBackend_DebugTextureAtlasNameLength = 16;
constexpr unsigned char _kRenderBackend_SVGStorageSizeInitial = 2;
constexpr u32 _kRenderBackend_TextureAtlasInvalidID = u32(-1);
constexpr u32 _kRenderBackend_TextureAtlasPreallocatedItems = 64;
constexpr u32 _kRenderBackend_TextureAtlasPreallocatedDimensions = 8;

inline constexpr size_t calculate_reserve_count(size_t bytes, size_t amount)
{
#ifndef DEBUG
	return std::bit_ceil(bytes * amount);
#else
	return std::bit_ceil(bytes * amount) * 2;
#endif
}

struct smol_atlas_t;
struct smol_atlas_item_t;

class ECORE_API CTextureAtlas
{
public:
	using element_lookupid_type = char;

	struct ECORE_API CTextureAtlasElement
	{
		smol_atlas_item_t* p_placement = nullptr;

		float x() const;
		float y() const;
		float w() const;
		float h() const;


		float u0(u32 atlas_width) const;
		float v0(u32 atlas_height) const;
		float u1(u32 atlas_width) const;
		float v1(u32 atlas_height) const;
	};

	/// @brief CTextureAtlasElement spatial indexing feature using morton codes, contains stable lookup_id for accessing element itself 
	struct ECORE_API CTAESpatialIndex
	{
		element_lookupid_type lookup_id = element_lookupid_type(-1);
	};

	using storage_type = std::pmr::vector<CTextureAtlasElement>;
	using spatial_storage_type = std::pmr::vector<CTAESpatialIndex>;
	using storage_allocator = std::pmr::polymorphic_allocator<storage_type::value_type>;
	using spatial_storage_allocator = std::pmr::polymorphic_allocator<spatial_storage_type::value_type>;

public:
	CTextureAtlas();
	CTextureAtlas(CTextureAtlas&& other) noexcept;
	CTextureAtlas(const CTextureAtlas&) = delete;
	CTextureAtlas& operator=(const CTextureAtlas&) = delete;
	~CTextureAtlas();

	CTextureAtlas& operator=(CTextureAtlas&& other) noexcept;

	void init(int width, int height, const char* pName);
	void uninit();

	bool addRegion(element_lookupid_type& lookup_element_id, const xr_string_view& icon_subpath_name, u32 w, u32 h, const void* pData, u32 pitch = 0);

	// if was successful immediately call addData after that method
	bool tryAddRegion(element_lookupid_type& lookup_element_id, const xr_string_view& icon_subpath_name, u32 w, u32 h);

	bool addData(u32 w, u32 h, const void* pData, u32 pitch);

	void getRegion(const xr_string_view& icon_subpath_name, u32& w, u32& h);

	void* getResource();
	void* getResource() const;
	const char* getTextureName() const;

	u32 getID() const;
	void setID(u32);

	u32 getWidth(void) const;
	u32 getHeight(void) const;

	const storage_type& getElements(void) const;

	CTextureAtlasElement* findNearest(float w, float h);
	const CTextureAtlasElement* findNearest(float w, float h) const;

	bool removeElement(float w, float h);
	bool removeElement(element_lookupid_type lookup_id);

	FactoryPtr<IUIShader>* getShader(void) const;
	void createShader();

private:
	element_lookupid_type findNearestSpatialIndex(float w, float h) const;
	void addRegion(u32 x, u32 y, u32 w, u32 h, const void* pData, u32 pitch);
private:
#ifdef DEBUG
	bool init_was_called;
	bool shader_was_created;
#endif
	mutable bool m_is_storage_dirty;
	u32 m_id;

	// logical layout placement 
	smol_atlas_t* m_p_atlas;

	// returned from resource manager and resource manager stores this texture (because later user will need to SetShader calling and for building we need to compile "blender" for that we need to obtain our texture from resource manager otherwise we can't use original way of rendering svg)
	CTexture* m_p_texture;

	FactoryPtr<IUIShader>* m_p_shader;

	// be very careful, change it only when it is needed by sense 
	// otherwise we can't provide find as const
	std::pmr::monotonic_buffer_resource sais_wrapper;
	std::pmr::monotonic_buffer_resource saissi_wrapper;
	mutable storage_type m_atlas_items;
	mutable spatial_storage_type m_atlas_items_spatial_indexing;
	unsigned char static_atlas_items_storage_spatial_indexing[calculate_reserve_count(sizeof(spatial_storage_type::value_type), _kRenderBackend_TextureAtlasPreallocatedItems)];
	unsigned char static_atlas_items_storage[calculate_reserve_count(sizeof(storage_type::value_type), _kRenderBackend_TextureAtlasPreallocatedItems)];
};
