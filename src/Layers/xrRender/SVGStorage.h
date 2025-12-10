#pragma once

enum eSVGStorageFlags
 {
	// new requested atlases can't be created and you get runtime exception (bad_alloc if memory was run out)
	kFeatureSVGStorage_Static_Allocation = 1 << 1,

	// if there's big amount of resources and we can't place on static storage we allocate more space and thus atlases
	kFeatureSVGStorage_Dynamic_Allocation = 1 << 2
};

#ifdef IXR_WINDOWS
constexpr const char* _kSVGStorge_DefaultSVGTextureSubPathName = "ui\\ui_vector_error.svg";
#else
constexpr const char* _kSVGStorge_DefaultSVGTextureSubPathName = "ui/ui_vector_error.svg";
#endif

constexpr const char* _kSVGStorage_DefaultSVGTextureName = "ui_vector_error.svg";
constexpr const char* _kSVGStorage_DefaultAtlasName = "svgdefaultatlas_";
constexpr unsigned short _kSVGStorage_MaxSubpathLength = 128;
constexpr u32 _kSVGStorage_DefaultAtlasID = 10;
// where element of specified size can be located because like we could add sizes (32,32); (128,128); but (256,256) can't be added for current atlas and it goes to different one and for that we have connection between two atlases by one texture name
constexpr unsigned char _kSVGStorage_MaxAtlasPlacement = 4;
// how many variants we can have per one texture
constexpr unsigned char _kSVGStorage_MaxElementsPerAtlas = 8;
constexpr int _kSVGStorage_DefaultAtlasSize = 512;

namespace lunasvg
{
	class Bitmap;
};

/// @brief author: wh1t3lord
class ECORE_API CSVGStorage
{
public:
	struct IconElement
	{

	};

	struct AtlasConnection
	{
		char atlas_ids[_kSVGStorage_MaxAtlasPlacement]{ -1,-1,-1,-1 };
		char elements_per_atlas[_kSVGStorage_MaxElementsPerAtlas * _kSVGStorage_MaxAtlasPlacement]{ -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1 };

		bool isValid(void) const 
		{ 
			bool result = false;

			for (int i = 0; i < _kSVGStorage_MaxAtlasPlacement; ++i)
			{
				if (atlas_ids[i] != char(-1))
				{
					result = true;
					break;
				}
			}

			return result;
		}
	};

public:
	CSVGStorage(u32 flags);
	~CSVGStorage();

	/// @brief call it only after RenderFactory was initialized and can allocate instances based on FactoryPtr
	void init();
	void uninit();

	// returns preallocated size that was specified initially (but it doesn't show current size)
	constexpr unsigned char get_static_size() const;

	// returns current size of storage
	unsigned int get_size() const;

	// if returns u32(-1) means it is failed to add atlas
	// see allocation policies that defined in eSVGStorageFlags
	u32 init_atlas(u32 w, u32 h, const char* pTextureName, CTextureAtlas& instance, bool generate_id = false);

	CTextureAtlas* get_atlas(u32 id);
	const CTextureAtlas* get_atlas(u32 id) const;
	const std::pmr::vector<CTextureAtlas>& get_atlases(void) const;

	const FactoryPtr<IUIShader>& get_shader(const std::string_view& subpath, float requested_width, float requested_height);
	const FactoryPtr<IUIShader>& get_default_shader();

	Frect get_uv(const std::string_view& subpath, float requested_width, float requested_height);

private:
	void init_default();
	void init_default_atlas();
	void init_default_shader();
	// always linerally adds new value to existed atlas_index_generator field (so obviously really trivial and efficient)
	u32 generate_id();

	// at runtime it might slow due to fact of IO
	// so use precaching strategy before loading level or before loading game
	AtlasConnection try_allocate(const std::string_view& subpath, float requested_width, float requested_height, AtlasConnection* p_existed);

	// allocates a new texture use it only when no atlas was allocated in storage or no valid atlas in storage
	AtlasConnection allocate(const std::string_view& subpath, float requested_width, float requested_height);

	bool add_data(const std::string_view& subpath, float requested_width, float requested_height, CTextureAtlas& atlas, AtlasConnection& connection);

	bool try_add_data(const std::string_view& subpath, float requested_width, float requested_height, const CTextureAtlas::element_lookupid_type atlas_lookup_id, CTextureAtlas& atlas, AtlasConnection& connection);

	bool get_bitmap(const std::string_view& subpath, float requested_width, float requested_height, lunasvg::Bitmap* bmp);

private:
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

	// probably better to replace with string_view but could we have a situation with temp allocated string?
	std::pmr::unordered_map<std::pmr::string, AtlasConnection> m_storage_textures;
};