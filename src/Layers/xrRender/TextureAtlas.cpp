#include "stdafx.h"
#include "TextureAtlas.h"
#include "SVGStorage.h"

#include "dxRenderDeviceRender.h"
#include "../xrRender/dxUIShader.h"

#include <lunasvg.h>
#include "smol-atlas.h"

float CTextureAtlas::CTextureAtlasElement::x() const
{
	if (p_placement)
		return sma_item_x(p_placement);

	assert(false && "early calling! you must init atlas first!");
	return -1.0f;
}

float CTextureAtlas::CTextureAtlasElement::y() const
{
	if (p_placement)
		return sma_item_y(p_placement);

	assert(false && "you must init atlas first!");
	return -1.0f;
}

float CTextureAtlas::CTextureAtlasElement::w() const
{
	if (p_placement)
		return sma_item_width(p_placement);

	assert(false && "you must init atlas first!");
	return -1.0f;
}

float CTextureAtlas::CTextureAtlasElement::h() const
{
	if (p_placement)
		return sma_item_height(p_placement);

	assert(false && "you must init atlas first!");
	return -1.0f;
}

float CTextureAtlas::CTextureAtlasElement::u0(u32 atlas_width) const
{
	float result = x();

	result /= static_cast<float>(atlas_width);

	return result;
}

float CTextureAtlas::CTextureAtlasElement::v0(u32 atlas_height) const
{
	float result = y();

	result /= static_cast<float>(atlas_height);

	return result;
}

float CTextureAtlas::CTextureAtlasElement::u1(u32 atlas_width) const
{
	float result = x();
	result += w();

	result /= static_cast<float>(atlas_width);

	return result;
}

float CTextureAtlas::CTextureAtlasElement::v1(u32 atlas_height) const
{
	float result = y();

	result += h();

	result /= static_cast<float>(atlas_height);

	return result;
}

CTextureAtlas::CTextureAtlas() :
#ifdef DEBUG
	init_was_called{},
	shader_was_created{},
#endif
	m_is_storage_dirty{},
	m_id{ _kRenderBackend_TextureAtlasInvalidID },
	m_p_atlas{},
	m_p_texture{},
	m_p_shader{},
	sais_wrapper{ &static_atlas_items_storage, sizeof(static_atlas_items_storage) },
	saissi_wrapper{ &static_atlas_items_storage_spatial_indexing, sizeof(static_atlas_items_storage_spatial_indexing) },
	m_atlas_items{ storage_allocator{&sais_wrapper} },
	m_atlas_items_spatial_indexing{ spatial_storage_allocator{&saissi_wrapper} }
{
	m_atlas_items.reserve(_kRenderBackend_TextureAtlasPreallocatedItems);
	m_atlas_items_spatial_indexing.reserve(_kRenderBackend_TextureAtlasPreallocatedItems);
}

CTextureAtlas::CTextureAtlas(CTextureAtlas&& other) noexcept :
#ifdef DEBUG
	init_was_called{ other.init_was_called },
	shader_was_created{},
#endif
	m_is_storage_dirty{},
	m_id{ other.m_id }, m_p_atlas{ other.m_p_atlas }, m_p_texture{ other.m_p_texture }, m_p_shader{ other.m_p_shader }, sais_wrapper{
	&static_atlas_items_storage, sizeof(static_atlas_items_storage)
	}, saissi_wrapper{ &static_atlas_items_storage_spatial_indexing, sizeof(static_atlas_items_storage_spatial_indexing) }, m_atlas_items{
		storage_allocator{ &sais_wrapper } }, m_atlas_items_spatial_indexing{ spatial_storage_allocator{&saissi_wrapper} }
{
	m_id = other.m_id;

	for (auto& element : other.m_atlas_items)
	{
		m_atlas_items.emplace_back(std::move(element));
	}

	for (auto& element : other.m_atlas_items_spatial_indexing)
	{
		m_atlas_items_spatial_indexing.emplace_back(std::move(element));
	}

	other.m_p_atlas = nullptr;
	other.m_p_texture = nullptr;
	other.m_p_shader = nullptr;
	other.m_id = _kRenderBackend_TextureAtlasInvalidID;
	other.m_atlas_items.clear();
	other.m_atlas_items_spatial_indexing.clear();

#ifdef DEBUG
	other.init_was_called = false;
#endif
}

CTextureAtlas::~CTextureAtlas()
{
#ifdef DEBUG
	R_ASSERT2(!init_was_called, "you forgot to call uninit or destroy this instance!");
#endif
}

CTextureAtlas& CTextureAtlas::operator=(CTextureAtlas&& other) noexcept
{
	if (this != &other)
	{
		uninit();

		m_id = other.m_id;
		m_p_atlas = other.m_p_atlas;

		m_p_texture = other.m_p_texture;
		m_p_shader = other.m_p_shader;

		for (auto& element : other.m_atlas_items)
		{
			m_atlas_items.emplace_back(std::move(element));
		}

		for (auto& element : other.m_atlas_items_spatial_indexing)
		{
			m_atlas_items_spatial_indexing.emplace_back(std::move(element));
		}

		other.m_p_atlas = nullptr;
		other.m_p_texture = nullptr;
		other.m_p_shader = nullptr;
		other.m_id = _kRenderBackend_TextureAtlasInvalidID;
		other.m_atlas_items.clear();
		other.m_atlas_items_spatial_indexing.clear();

#ifdef DEBUG
		init_was_called = other.init_was_called;
		other.init_was_called = false;
#endif
	}

	return *this;
}

void CTextureAtlas::init(int width, int height, const char* pName)
{
	R_ASSERT(width > 0 && "must be valid");
	R_ASSERT(height > 0 && "must be valid!");
	R_ASSERT(!m_p_atlas && "must be not initialized otherwise you forgot to call uninit!");
	R_ASSERT(DEV && "early calling?");

	if (!m_p_atlas)
	{
		m_p_atlas = sma_atlas_create(width, height);

		R_ASSERT(m_p_atlas && "failed to create logical layout atlas!");
	}

	m_p_texture = DEV->_CreateEmptyTexture(pName, width, height);

	R_ASSERT(m_p_texture && "must be created a valid texture from resource manager, failed to create!");

#ifdef DEBUG
	init_was_called = true;
#endif
}

void CTextureAtlas::uninit()
{
	if (m_p_texture)
	{
		m_p_texture->can_unload = false;
		DEV->_DeleteTexture(m_p_texture);
		m_p_texture->Unload();
		m_p_texture = nullptr;
	}

	if (m_p_shader)
	{
		delete m_p_shader;
		m_p_shader = nullptr;
	}

	if (m_p_atlas)
	{
		for (CTextureAtlasElement& item : m_atlas_items)
		{
			R_ASSERT(item.p_placement && "must be valid otherwise you didn't remove item from vector properly");
			if (item.p_placement)
			{
				sma_item_remove(m_p_atlas, item.p_placement);
			}
		}


		sma_atlas_destroy(m_p_atlas);

		m_atlas_items.clear();
		m_p_atlas = nullptr;
	}

#ifdef DEBUG
	init_was_called = false;
	shader_was_created = false;
#endif
}

bool CTextureAtlas::addRegion(element_lookupid_type& lookup_element_id, const xr_string_view& icon_subpath_name, u32 w, u32 h, const void* pData, u32 pitch)
{
	R_ASSERT(m_p_atlas && "must be initialized before calling this method!");
	R_ASSERT(m_p_texture && "you forgot to call init because texture wasn't initialized!");

	bool result = false;
	lookup_element_id = element_lookupid_type(-1);
	if (m_p_atlas && m_p_texture)
	{
		smol_atlas_item_t* p_current_placement = sma_item_add(m_p_atlas, w, h);
		R_ASSERT(p_current_placement && "failed to create logical placement item");
		result = !!(p_current_placement);
		if (p_current_placement)
		{
			u32 x = static_cast<u32>(sma_item_x(p_current_placement));
			u32 y = static_cast<u32>(sma_item_y(p_current_placement));

			CTextureAtlasElement item;
			item.p_placement = p_current_placement;

			u32 _w = m_p_texture->get_Width();
			u32 _h = m_p_texture->get_Height();

			// we don't need to store it but we need to calculate at runtime
		//	u0 = float(x) / float(_w);
		//	v0 = float(y) / float(_h);
		//	u1 = float(x + w) / float(_w);
		//	v1 = float(y + h) / float(_h);

			m_atlas_items.push_back(item);

			m_atlas_items_spatial_indexing.push_back({});
			lookup_element_id = static_cast<element_lookupid_type>(m_atlas_items.size() - 1);
			m_atlas_items_spatial_indexing.back().lookup_id = lookup_element_id;

			if (pitch == 0)
				pitch = _w * 4;

			addRegion(x, y, w, h, pData, pitch);
		}
	}

	return result;
}

bool CTextureAtlas::tryAddRegion(element_lookupid_type& lookup_element_id, const xr_string_view& icon_subpath_name, u32 w, u32 h)
{
	R_ASSERT(m_p_atlas && "must be initialized before calling this method!");
	R_ASSERT(m_p_texture && "you forgot to call init because texture wasn't initialized!");

	bool result = false;
	lookup_element_id = element_lookupid_type(-1);
	if (m_p_atlas && m_p_texture)
	{
		smol_atlas_item_t* p_current_placement = sma_item_add(m_p_atlas, w, h);
		R_ASSERT(p_current_placement && "failed to create logical placement item");
		result = !!(p_current_placement);
		if (p_current_placement)
		{
			CTextureAtlasElement item;
			item.p_placement = p_current_placement;

			m_atlas_items.push_back(item);

			m_atlas_items_spatial_indexing.push_back({});
			lookup_element_id = static_cast<element_lookupid_type>(m_atlas_items.size() - 1);
			m_atlas_items_spatial_indexing.back().lookup_id = static_cast<element_lookupid_type>(m_atlas_items.size() - 1);
		}
	}

	return result;
}

bool CTextureAtlas::addData(u32 w, u32 h, const void* pData, u32 pitch)
{
	bool result = true;

	CTextureAtlasElement& element = m_atlas_items.back();

	u32 x = static_cast<u32>(sma_item_x(element.p_placement));
	u32 y = static_cast<u32>(sma_item_y(element.p_placement));
	u32 _w = m_p_texture->get_Width();
	u32 _h = m_p_texture->get_Height();

	if (pitch == 0)
		pitch = _w * 4;

	addRegion(x, y, w, h, pData, pitch);

	return result;
}

void CTextureAtlas::getRegion(const xr_string_view& icon_subpath_name, u32& w, u32& h)
{
}

void CTextureAtlas::addRegion(u32 x, u32 y, u32 w, u32 h, const void* pData, u32 pitch)
{
	if (!m_p_texture || !m_p_texture->pSurface || !pData)
		return;

	RHISubResource sub;
	sub.Data = pData;
	sub.Width = w;
	sub.Height = h;
	sub.Depth = 1;
	sub.RowPitch = pitch;
	sub.DepthPitch = 0;
	sub.TextureFormat = ERHI_FORMAT::R8G8B8A8_UNORM;

	RHIBox box;
	box.left = x;
	box.top = y;
	box.front = 0;
	box.right = x + w;
	box.bottom = y + h;
	box.back = 1;

	m_p_texture->pSurface->UpdateData(0, 0, &sub, box);
}


void* CTextureAtlas::getResource()
{
	if (m_p_texture)
	{
		return m_p_texture->get_SRView()->GetRawSRV();
	}

	return nullptr;
}

void* CTextureAtlas::getResource() const
{
	if (m_p_texture)
	{
		return m_p_texture->get_SRView()->GetRawSRV();
	}

	return nullptr;
}

const char* CTextureAtlas::getTextureName() const
{
	if (m_p_texture)
		return m_p_texture->cName.c_str();

	return "";
}


void CTextureAtlas::saveOnDisk()
{
#ifdef DEBUG

#endif
}

u32 CTextureAtlas::getID() const
{
	return m_id;
}

void CTextureAtlas::setID(u32 id)
{
	m_id = id;
}

u32 CTextureAtlas::getWidth(void) const
{
	R_ASSERT(m_p_texture && "must be valid!");

	if (m_p_texture)
	{
		return m_p_texture->get_Width();
	}

	return 0;
}

u32 CTextureAtlas::getHeight(void) const
{
	R_ASSERT(m_p_texture && "must be valid!");

	if (m_p_texture)
	{
		return m_p_texture->get_Height();
	}

	return 0;
}

const CTextureAtlas::storage_type& CTextureAtlas::getElements(void) const
{
	return m_atlas_items;
}

CTextureAtlas::CTextureAtlasElement* CTextureAtlas::findNearest(float x, float y)
{
	CTextureAtlasElement* pResult = nullptr;

	if (m_atlas_items.empty())
		return pResult;

	element_lookupid_type id = findNearestSpatialIndex(x, y);
	R_ASSERT(id >= -1 && "failed to obtain nearest!");

	if (id == -1)
		return pResult;

	pResult = &m_atlas_items[m_atlas_items_spatial_indexing[id].lookup_id];

	return pResult;
}

const CTextureAtlas::CTextureAtlasElement* CTextureAtlas::findNearest(float x, float y) const
{
	CTextureAtlasElement* pResult = nullptr;

	if (m_atlas_items.empty())
		return pResult;

	element_lookupid_type id = findNearestSpatialIndex(x, y);
	R_ASSERT(id >= -1 && "failed to obtain nearest!");

	if (id == -1)
		return pResult;

	pResult = &m_atlas_items[m_atlas_items_spatial_indexing[id].lookup_id];

	return pResult;
}

CTextureAtlas::element_lookupid_type CTextureAtlas::findNearestSpatialIndex(float x, float y) const
{
	element_lookupid_type result = element_lookupid_type(-1);

	auto pMortonCodeCalculate = [](float _x, float _y) -> u64
		{
			constexpr float minVal = 0.0f;
			constexpr float maxVal = 32768.0f;
			constexpr uint32_t maxInt = 0x7FFFFF; // 23 bits for precision

			// Normalize to [0, 1] range
			float nx = (_x - minVal) / (maxVal - minVal);
			float ny = (_y - minVal) / (maxVal - minVal);

			// Scale to integer range
			uint32_t ix = static_cast<uint32_t>(nx * maxInt);
			uint32_t iy = static_cast<uint32_t>(ny * maxInt);

			// Interleave bits using magic numbers (faster than loop)
			uint64_t x64 = ix;
			uint64_t y64 = iy;

			x64 = (x64 | (x64 << 16)) & 0x0000FFFF0000FFFF;
			x64 = (x64 | (x64 << 8)) & 0x00FF00FF00FF00FF;
			x64 = (x64 | (x64 << 4)) & 0x0F0F0F0F0F0F0F0F;
			x64 = (x64 | (x64 << 2)) & 0x3333333333333333;
			x64 = (x64 | (x64 << 1)) & 0x5555555555555555;

			y64 = (y64 | (y64 << 16)) & 0x0000FFFF0000FFFF;
			y64 = (y64 | (y64 << 8)) & 0x00FF00FF00FF00FF;
			y64 = (y64 | (y64 << 4)) & 0x0F0F0F0F0F0F0F0F;
			y64 = (y64 | (y64 << 2)) & 0x3333333333333333;
			y64 = (y64 | (y64 << 1)) & 0x5555555555555555;

			return x64 | (y64 << 1);
		};

	if (m_is_storage_dirty)
	{
		std::sort(m_atlas_items_spatial_indexing.begin(), m_atlas_items_spatial_indexing.end(), [pMortonCodeCalculate, this](const CTAESpatialIndex& left, const CTAESpatialIndex& right) -> bool {

			R_ASSERT(left.lookup_id >= 0 && "must be initialized and valid!");
			R_ASSERT(right.lookup_id >= 0 && "must be initialized and valid!");

			const CTextureAtlasElement& el_left = m_atlas_items[left.lookup_id];
			const CTextureAtlasElement& el_right = m_atlas_items[right.lookup_id];

			return pMortonCodeCalculate(el_left.w(), el_left.h()) < pMortonCodeCalculate(el_right.w(), el_right.h());
			});

		m_is_storage_dirty = false;
	}

	u64 queryCode = pMortonCodeCalculate(x, y);

	// Binary search for the closest Morton code
	auto it = std::lower_bound(m_atlas_items_spatial_indexing.begin(), m_atlas_items_spatial_indexing.end(),
		pMortonCodeCalculate(0.0f, 0.0f),
		[pMortonCodeCalculate, queryCode, this](const CTAESpatialIndex& p, const u64) {
			R_ASSERT(p.lookup_id >= 0 && "must be initialized and valid!");

			const CTextureAtlasElement& el = m_atlas_items[p.lookup_id];

			return pMortonCodeCalculate(el.w(), el.h()) < queryCode;
		});

	// Check if we're at the beginning or end
	if (it == m_atlas_items_spatial_indexing.begin())
		return 0;

	if (it == m_atlas_items_spatial_indexing.end())
		return static_cast<element_lookupid_type>(m_atlas_items_spatial_indexing.size() - 1);

	// Compare with previous element to find which is closer
	element_lookupid_type idx = static_cast<element_lookupid_type>(it - m_atlas_items_spatial_indexing.begin());

	const CTextureAtlasElement& el_code1 = m_atlas_items[m_atlas_items_spatial_indexing[idx].lookup_id];
	const CTextureAtlasElement& el_code2 = m_atlas_items[m_atlas_items_spatial_indexing[idx - 1].lookup_id];

	u64 code1 = pMortonCodeCalculate(el_code1.w(), el_code1.h());
	u64 code2 = pMortonCodeCalculate(el_code2.w(), el_code2.h());

	return static_cast<element_lookupid_type>((std::abs(static_cast<int64_t>(queryCode - code1)) <
		std::abs(static_cast<int64_t>(queryCode - code2))) ? idx : idx - 1);
}

bool CTextureAtlas::removeElement(float x, float y)
{
	element_lookupid_type id = findNearestSpatialIndex(x, y);

	return removeElement(id);
}

bool CTextureAtlas::removeElement(element_lookupid_type lookup_id)
{
	bool result = true;

	if (lookup_id < 0)
	{
		result = false;
		return result;
	}

	element_lookupid_type real_lookup = m_atlas_items_spatial_indexing[lookup_id].lookup_id;

	m_atlas_items.erase(m_atlas_items.begin() + real_lookup);
	m_atlas_items_spatial_indexing.erase(m_atlas_items_spatial_indexing.begin() + lookup_id);

	R_ASSERT(m_atlas_items.size() == m_atlas_items_spatial_indexing.size() && "must be equal!");

	// now we reset lookup indexing due to sorting
	for (char i = 0; i < m_atlas_items_spatial_indexing.size(); ++i)
	{
		m_atlas_items_spatial_indexing[i].lookup_id = i;
	}

	m_is_storage_dirty = true;

	return result;
}

FactoryPtr<IUIShader>* CTextureAtlas::getShader(void) const
{
	return m_p_shader;
}

void CTextureAtlas::createShader()
{
#ifdef DEBUG
	R_ASSERT(!shader_was_created && "you must call only once!");
	R_ASSERT(!m_p_shader && "must be not inited!");
#endif

	R_ASSERT(m_p_texture && "early calling, texture must exist!");

	if (!m_p_shader && m_p_texture)
	{
		m_p_shader = new FactoryPtr<IUIShader>();
		R_ASSERT(m_p_shader && "failed to allocate shader! (CPU)");

		if (m_p_shader)
		{
			char buf[128];
			std::sprintf(buf, "hud%sdefault", Platform::kPreferredSeparator);
			(*m_p_shader)->create(buf, m_p_texture->cName.c_str());

#ifdef DEBUG
			shader_was_created = true;
#endif
		}
	}
}

CSVGStorage::CSVGStorage(u32 flags) :

#ifdef DEBUG
	m_init_was_called{},
#endif
	m_atlas_index_generator{},
	m_p_default_shader{},
	m_default_atlas{},
	m_static_storage{},
	m_ss_wrapper{ &m_static_storage, sizeof(m_static_storage), flags & eSVGStorageFlags::kFeatureSVGStorage_Static_Allocation ? std::pmr::null_memory_resource() : std::pmr::get_default_resource() },
	m_storage_atlases{ std::pmr::polymorphic_allocator<CTextureAtlas>{&m_ss_wrapper} }
{
	R_ASSERT(!(flags & eSVGStorageFlags::kFeatureSVGStorage_Static_Allocation && flags & eSVGStorageFlags::kFeatureSVGStorage_Dynamic_Allocation) && "invalid flags");

	// if allocation size is changed in static mode you will get throw bad_alloc due to fact that required allocation formula was changed so in such case you have to change the size of static_storage field please
	m_storage_atlases.reserve(_kRenderBackend_SVGStorageSizeInitial);
}

CSVGStorage::~CSVGStorage()
{
#ifdef DEBUG
	R_ASSERT(!m_init_was_called && "you forgot to call uninit!");
#endif
}

void CSVGStorage::init()
{
	init_default();

#ifdef DEBUG
	m_init_was_called = true;
#endif
}

void CSVGStorage::uninit()
{
	m_default_atlas.uninit();
	xr_delete(m_p_default_shader);

	for (CTextureAtlas& atlas : m_storage_atlases)
	{
		atlas.uninit();
	}


#ifdef DEBUG
	m_init_was_called = false;
#endif
}

// returns preallocated size that was specified initially (but it doesn't show current size)
constexpr unsigned char CSVGStorage::get_static_size() const
{
	return _kRenderBackend_SVGStorageSizeInitial;
}

// returns current size of storage
unsigned int CSVGStorage::get_size() const
{
	return m_storage_atlases.size();
}

u32 CSVGStorage::init_atlas(u32 w, u32 h, const char* pTextureName, CTextureAtlas& instance, bool is_generate_id)
{
	R_ASSERT(pTextureName && pTextureName[0] != '\0' && "you have to pass a valid and not empty string!");

	u32 result = u32(-1);
	if (is_generate_id)
		result = generate_id();

	instance.init(w, h, pTextureName);

	return result;
}

CTextureAtlas* CSVGStorage::get_atlas(u32 id)
{
	if (id == _kSVGStorage_DefaultAtlasID)
	{
		return &m_default_atlas;
	}

	auto it = std::find_if(m_storage_atlases.begin(), m_storage_atlases.end(), [id](const CTextureAtlas& atlas) -> bool {
		return atlas.getID() == id;
		});

	if (it != m_storage_atlases.end())
		return &(*it);

	return nullptr;
}

const CTextureAtlas* CSVGStorage::get_atlas(u32 id) const
{
	if (id == _kSVGStorage_DefaultAtlasID)
	{
		return &m_default_atlas;
	}

	auto it = std::find_if(m_storage_atlases.begin(), m_storage_atlases.end(), [id](const CTextureAtlas& atlas) -> bool {
		return atlas.getID() == id;
		});

	if (it != m_storage_atlases.end())
		return &(*it);

	return nullptr;
}

const std::pmr::vector<CTextureAtlas>& CSVGStorage::get_atlases(void) const
{
	return m_storage_atlases;
}

void CSVGStorage::delete_atlas(u32 id)
{

}

void CSVGStorage::generate_cache()
{

}

// make it optional field that will check should we cache
void CSVGStorage::load_cache()
{

}

const FactoryPtr<IUIShader>& CSVGStorage::get_shader(const std::string_view& subpath, float requested_width, float requested_height)
{
	R_ASSERT(m_p_default_shader && "default shader must be initialized!");

	if (subpath.empty() == false)
	{
		// todo: 
	//	R_ASSERT(false && "todo");

		if (subpath == _kDefaultSVGShader)
			return get_default_shader();

		if (m_storage_textures.find(subpath.data()) == m_storage_textures.end())
		{
			auto lookup = try_allocate(subpath, requested_width, requested_height, nullptr);
			R_ASSERT(lookup.isValid() && "failed to allocate!");

			m_storage_textures[subpath.data()] = lookup;

			char idx = lookup.atlas_ids[0];

			CTextureAtlas& atlas = m_storage_atlases[idx];

			R_ASSERT(atlas.getShader() && "must be valid!");

			return *(atlas.getShader());
		}
		else
		{
			AtlasConnection& lookup_list = m_storage_textures.at(subpath.data());
			bool found = false;

			for (int i = 0; i < _kSVGStorage_MaxAtlasPlacement; ++i)
			{
				if (
					lookup_list.atlas_ids[i] != CTextureAtlas::element_lookupid_type(-1))
				{
					const CTextureAtlas& atlas = m_storage_atlases[i];

					const CTextureAtlas::storage_type& elements = atlas.getElements();
					for (int j = 0; j < _kSVGStorage_MaxElementsPerAtlas; ++j)
					{
						CTextureAtlas::element_lookupid_type element_id = j + (i * _kSVGStorage_MaxElementsPerAtlas);

						// no avaiable so our searching is done
						if (lookup_list.elements_per_atlas[element_id] == CTextureAtlas::element_lookupid_type(-1))
							break;
						
						const CTextureAtlas::CTextureAtlasElement& element = elements[lookup_list.elements_per_atlas[element_id]];

						if (element.w() == int(requested_width) && element.h() == int(requested_height))
						{
							found = true;
							break;
						}
					}

					if (found)
					{
						R_ASSERT(atlas.getShader() && "must be initialized");
						return *atlas.getShader();
					}
				}
			}

			if (!found)
			{
				// didn't find appropriate size so let's allocate

				auto lookup = try_allocate(subpath, requested_width, requested_height, &lookup_list);
				R_ASSERT(lookup.isValid() && "failed to allocate!");

				char idx = lookup.atlas_ids[0];

				CTextureAtlas& atlas = m_storage_atlases[idx];

				R_ASSERT(atlas.getShader() && "must be valid!");

				return *atlas.getShader();
			}
		}
	}

	return get_default_shader();
}

const FactoryPtr<IUIShader>& CSVGStorage::get_default_shader()
{
	R_ASSERT(m_p_default_shader && "must be valid and initialized!");
	if (m_p_default_shader)
	{
		return *(m_p_default_shader);
	}

	return m_empty_default_shader;
}

Frect CSVGStorage::get_uv(const std::string_view& subpath, float requested_width, float requested_height)
{
	Frect result;
	bool found = false;
	if (subpath.empty() == false && subpath != _kDefaultSVGShader)
	{
		if (m_storage_textures.find(subpath.data()) != m_storage_textures.end())
		{
			AtlasConnection& lookup_list = m_storage_textures.at(subpath.data());

			R_ASSERT(lookup_list.isValid() && "must be valid!!!");
			constexpr int _kSize = sizeof(AtlasConnection::atlas_ids) / sizeof(AtlasConnection::atlas_ids[0]);


			for (int i = 0; i < _kSize; ++i)
			{
				if (found)
					break;

				CTextureAtlas& atlas = m_storage_atlases[lookup_list.atlas_ids[i]];
				R_ASSERT(atlas.getShader() && "must be inited and valid!");

				if (atlas.getShader() == nullptr)
				{
#ifdef DEBUG
					Msg("! [svg]: atlas[%s] has invalid shader", atlas.getTextureName());
#endif
					break;
				}

				if (atlas.getResource() == nullptr)
				{
#ifdef DEBUG
					Msg("! [svg]: atlas[%s] has invalid texture", atlas.getTextureName());
#endif
					break;
				}

				const CTextureAtlas::storage_type& elements = atlas.getElements();

				for (int j = 0; j < _kSVGStorage_MaxElementsPerAtlas; ++j)
				{
					int real_j = j + (_kSVGStorage_MaxElementsPerAtlas * i);

					CTextureAtlas::element_lookupid_type el_id = lookup_list.elements_per_atlas[real_j];

					if (el_id != CTextureAtlas::element_lookupid_type(-1))
					{
						const CTextureAtlas::CTextureAtlasElement& element = elements[el_id];

						if (element.w() == int(requested_width) && element.h() == int(requested_height))
						{
							found = true;
							float w = atlas.getWidth();
							float h = atlas.getHeight();

							result.lt.set(w * element.u0(static_cast<u32>(w)), h * element.v0(static_cast<u32>(h)));
							result.rb.set(w * element.u1(static_cast<u32>(w)), h * element.v1(static_cast<u32>(h)));

							break;
						}
					}
				}
			}

			if (!found)
			{
#ifdef DEBUG
				R_ASSERT(false && "shouldn't happen?");
				Msg("! [svg]: failed to obtain [tex_name:%s;w:%.2f;h:%.2f]",
					subpath.data(),
					requested_width,
					requested_height
				);
#endif
			}
		}
#ifdef DEBUG
		else
		{
			Msg("! [svg]: can't find texture[%s]", subpath.data());
		}
#endif
	}

	if (m_default_atlas.getResource() && !found)
	{
		CTextureAtlas::CTextureAtlasElement* pElement = m_default_atlas.findNearest(requested_width, requested_height);

		if (pElement)
		{
			float w = m_default_atlas.getWidth();
			float h = m_default_atlas.getHeight();

			result.lt.set(w * pElement->u0(static_cast<u32>(w)), h * pElement->v0(static_cast<u32>(h)));
			result.rb.set(w * pElement->u1(static_cast<u32>(w)), h * pElement->v1(static_cast<u32>(h)));
		}
	}

	return result;
}


void CSVGStorage::init_default()
{
	init_default_atlas();
	init_default_shader();
}

void CSVGStorage::init_default_atlas()
{
	string_path fn;
	FS.update_path(fn, "$game_textures$", _kSVGStorge_DefaultSVGTextureSubPathName);
	// 	bool try_load = FS.TryLoad(fn);
	// 	R_ASSERT(try_load && "failed to obtain file");

	IReader* pReader = FS.r_open(fn);

	R_ASSERT(pReader && "there's no default SVG texture file it must be presented on disk or archive!");

	if (pReader)
	{
		init_atlas(384, 384, _kSVGStorage_DefaultAtlasName, m_default_atlas);
		m_default_atlas.setID(_kSVGStorage_DefaultAtlasID);

		u32 len = pReader->length();
		std::unique_ptr<lunasvg::Document> doc;

		xr_string data;
		data.resize(len);
		pReader->r(&data[0], len);
		doc = std::move(lunasvg::Document::loadFromData(data.c_str()));

		R_ASSERT(doc.get() && "failed to load svg document!");

		if (doc.get())
		{
			char _notused_lookupid;
			for (unsigned char i = 1; i <= 4; ++i)
			{
				float fStartDim = 32.0f;
				fStartDim *= i;
				lunasvg::Bitmap bmp = doc->renderToBitmap(fStartDim, fStartDim);
#ifdef USE_DX11
				bmp.convertToRGBA();
#else
#endif


				m_default_atlas.addRegion(_notused_lookupid, _kSVGStorge_DefaultSVGTextureSubPathName, bmp.width(), bmp.height(), bmp.data(), bmp.stride());
			}
		}

		FS.r_close(pReader);
	}
}

void CSVGStorage::init_default_shader()
{
	m_p_default_shader = new FactoryPtr<IUIShader>();

	R_ASSERT(m_p_default_shader && "failed to allocate default shader");
	if (m_p_default_shader)
	{
		(*m_p_default_shader)->create("hud\\default", _kSVGStorage_DefaultAtlasName);
	}
}

CSVGStorage::AtlasConnection CSVGStorage::try_allocate(const std::string_view& subpath, float requested_width, float requested_height, AtlasConnection* p_existed)
{
	AtlasConnection result;

	CTextureAtlas::element_lookupid_type iter = 0;
	bool was_added = false;
	for (CTextureAtlas& atlas : m_storage_atlases)
	{
		bool status = try_add_data(
			subpath,
			requested_width,
			requested_height,
			iter,
			atlas,
			p_existed ? *p_existed : result
		);

#ifdef DEBUG
		if (status)
		{
			Msg("[svg]: added region w: %.2f h: %.2f to atlas [%d]", requested_width, requested_height, atlas.getID());
		}
#endif

		was_added = status;

		if (was_added)
		{
			if (p_existed)
				result = *p_existed;
			
			break;
		}

		++iter;
	}

	if (!was_added)
	{
		result = allocate(subpath, requested_width, requested_height);
	}

	return result;
}


CSVGStorage::AtlasConnection CSVGStorage::allocate(const std::string_view& subpath, float requested_width, float requested_height)
{
	AtlasConnection result;

	if (requested_width <= _kSVGStorage_DefaultAtlasSize && requested_height <= _kSVGStorage_DefaultAtlasSize)
	{
		char texture_name[32];

		std::sprintf(texture_name, "svg_atlas_%zu", m_storage_atlases.size());

		CTextureAtlas atlas;
		u32 atlas_id = init_atlas(_kSVGStorage_DefaultAtlasSize, _kSVGStorage_DefaultAtlasSize, texture_name, atlas, true);
		atlas.setID(atlas_id);

		R_ASSERT2(requested_height <= atlas.getHeight(), "invalid height! Too big height");
		R_ASSERT2(requested_width <= atlas.getWidth(), "invalid width! Too big width");

		bool data_insert_status = add_data(subpath, requested_width, requested_height, atlas, result);

		R_ASSERT2(data_insert_status, "failed to insert data to atlas");

		if (data_insert_status)
		{
			R_ASSERT(atlas.getShader() == nullptr && "must be nullptr!");

			atlas.createShader();

#ifdef DEBUG
			Msg("[svg]: allocated atlas[id:%d;w:%d;h:%d;tex_name:%s] and addded region w: %.2f h: %.2f ",
				atlas.getID(),
				atlas.getWidth(),
				atlas.getHeight(),
				atlas.getTextureName(),
				requested_width,
				requested_height
			);
#endif
			m_storage_atlases.emplace_back(std::move(atlas));
			result.atlas_ids[0] = static_cast<char>(m_storage_atlases.size() - 1);
		}
	}

	return result;
}


bool CSVGStorage::add_data(const std::string_view& subpath, float requested_width, float requested_height, CTextureAtlas& atlas, AtlasConnection& connection)
{
	R_ASSERT(subpath.empty() == false && "must be valid!");

	bool result = false;

	if (subpath.empty() == false)
	{
		result = true;

		lunasvg::Bitmap bmp;
		result = get_bitmap(subpath, requested_width, requested_height, &bmp);

		R_ASSERT(result && "failed to obtain bitmap!");

		if (result)
		{
			CTextureAtlas::element_lookupid_type lookup_element_id;
			result = atlas.addRegion(lookup_element_id, subpath, bmp.width(), bmp.height(), bmp.data(), bmp.stride());

			R_ASSERT(connection.atlas_ids[0] == CTextureAtlas::element_lookupid_type(-1) && "expected minus one because it is not existed in map!");
			R_ASSERT(connection.elements_per_atlas[0] == CTextureAtlas::element_lookupid_type(-1) && "expected minus one because it is not existed in map!");

			connection.elements_per_atlas[0] = lookup_element_id;
		}
	}

	return result;
}

bool CSVGStorage::try_add_data(const std::string_view& subpath, float requested_width, float requested_height, const CTextureAtlas::element_lookupid_type atlas_lookup_id, CTextureAtlas& atlas, AtlasConnection& connection)
{
	bool result = false;

	CTextureAtlas::element_lookupid_type lookup_el_id;
	result = atlas.tryAddRegion(lookup_el_id, subpath, requested_width, requested_height);

	if (lookup_el_id != CTextureAtlas::element_lookupid_type(-1))
	{
		//	R_ASSERT(false && "todo: implement");

		R_ASSERT(atlas_lookup_id <= CTextureAtlas::element_lookupid_type(3) && "overflow, same texture can be placed at most in 4 atlases!");

		bool filled_atlas_info = false;
		for (int i = 0; i < _kSVGStorage_MaxAtlasPlacement; ++i)
		{
			if (connection.atlas_ids[i] == CTextureAtlas::element_lookupid_type(-1))
			{
				connection.atlas_ids[i] = atlas_lookup_id;
				filled_atlas_info = true;
				break;
			}
			else
			{
				if (connection.atlas_ids[i] == atlas_lookup_id)
				{
					filled_atlas_info = true;
					break;
				}
			}
		}

		R_ASSERT(filled_atlas_info && "probably overflow it means we can't insert new information to existed connection");

		if (filled_atlas_info)
		{
			bool filled_element_info = false;
			for (int j = 0; j < _kSVGStorage_MaxElementsPerAtlas; ++j)
			{
				int connection_el_id = j + (atlas_lookup_id * _kSVGStorage_MaxElementsPerAtlas);

				if (
					connection.elements_per_atlas[connection_el_id] == CTextureAtlas::element_lookupid_type(-1))
				{
					connection.elements_per_atlas[connection_el_id] = lookup_el_id;
					filled_element_info = true;
					break;
				}
			}

			R_ASSERT(filled_element_info && "failed to set data, overflow!");
		}
	}

#ifdef DEBUG
	if (!result)
		Msg("[svg]: can't add atlas with w: %d h: %d to [%d]", requested_width, requested_height, atlas.getID());
#endif

	if (result)
	{
		lunasvg::Bitmap bmp;

		result = get_bitmap(subpath, requested_width, requested_height, &bmp);

		R_ASSERT(result && "failed to obtain data!");

		if (result)
		{
			result = atlas.addData(bmp.width(), bmp.height(), bmp.data(), bmp.stride());
		}
	}

	return result;
}

bool CSVGStorage::get_bitmap(const std::string_view& subpath, float requested_width, float requested_height, lunasvg::Bitmap* bmp)
{
	R_ASSERT(bmp && "pass valid pointer!");

	bool result = false;

	char buf[256];
	constexpr unsigned int _kSize = sizeof(buf) / sizeof(buf[0]);
	if (subpath.size() > _kSize)
	{
		R_ASSERT(false && "you have too long subpath, there's no need to move files to different folders and making chaos...");
		Msg("! [svg]: too long subpath, max length is 255, can't add data");
		return result;
	}

	std::sprintf(buf, "ui%s%s", Platform::kPreferredSeparator, subpath.data());

	string_path fn;
	FS.update_path(fn, "$game_textures$", buf);
	// 	bool try_load = FS.TryLoad(fn);
	// 	R_ASSERT(try_load && "failed to load");

	IReader* pReader = FS.r_open(fn);

	result = !!(pReader);

	R_ASSERT(pReader && "failed to open file!");

	if (pReader)
	{
		u32 len = pReader->length();
		std::unique_ptr<lunasvg::Document> doc;

		// todo: probably pmr would be better?
		xr_string data;
		data.resize(len);
		pReader->r(&data[0], len);
		doc = std::move(lunasvg::Document::loadFromData(data.c_str()));

		R_ASSERT(doc.get() && "failed to load svg document!");

		result = !!(doc.get());

		if (doc.get())
		{
			*bmp = doc->renderToBitmap(requested_width, requested_height);

#ifdef USE_DX11
			bmp->convertToRGBA();
#else
#endif
		}

		FS.r_close(pReader);
	}

	return result;
}

u32 CSVGStorage::generate_id()
{
	++m_atlas_index_generator;

	// we should regenerate id due to avoiding collision with defined id
	if (m_atlas_index_generator == _kSVGStorage_DefaultAtlasID)
		++m_atlas_index_generator;

	return m_atlas_index_generator;
}