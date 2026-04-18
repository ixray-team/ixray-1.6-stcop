#include "stdafx.h"
#include "TextureAtlas.h"
#include "SVGStorage.h"

#include "dxRenderDeviceRender.h"
#include "dxUIShader.h"

#include <lunasvg.h>
#include "smol-atlas.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdlib>

#include "../../xrCore/_math.h"

namespace
{
IC u64 SvgHashCombineU64(u64 a, u64 b)
{
	return a ^ (b + 0x9e3779b97f4a7c15ULL + (a << 6) + (a >> 2));
}

void ApplySvgTintToNearWhitePixels(lunasvg::Bitmap& bmp, const SVGTintRGBA& tint)
{
	if (tint.IsWhite())
		return;

	const u32 w = bmp.width();
	const u32 h = bmp.height();
	if (!w || !h || !bmp.data())
		return;

	u8* base = static_cast<u8*>(bmp.data());
	const u32 stride = bmp.stride();

	for (u32 y = 0; y < h; ++y)
	{
		u8* row = base + y * stride;
		for (u32 x = 0; x < w; ++x)
		{
			u8* px = row + x * 4;
			const u8 pr = px[0];
			const u8 pg = px[1];
			const u8 pb = px[2];
			const u8 pa = px[3];
			if (pa == 0)
				continue;
			if (pr >= 235 && pg >= 235 && pb >= 235)
			{
				px[0] = static_cast<u8>(u32(pr) * u32(tint.r) / 255u);
				px[1] = static_cast<u8>(u32(pg) * u32(tint.g) / 255u);
				px[2] = static_cast<u8>(u32(pb) * u32(tint.b) / 255u);
				px[3] = static_cast<u8>(u32(pa) * u32(tint.a) / 255u);
			}
		}
	}
}
} // namespace

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
		std::sort
		(
			m_atlas_items_spatial_indexing.begin(), m_atlas_items_spatial_indexing.end(),
			[pMortonCodeCalculate, this](const CTAESpatialIndex& left, const CTAESpatialIndex& right) -> bool
			{
				R_ASSERT(left.lookup_id >= 0 && "must be initialized and valid!");
				R_ASSERT(right.lookup_id >= 0 && "must be initialized and valid!");

				const CTextureAtlasElement& el_left = m_atlas_items[left.lookup_id];
				const CTextureAtlasElement& el_right = m_atlas_items[right.lookup_id];

				return pMortonCodeCalculate(el_left.w(), el_left.h()) < pMortonCodeCalculate(el_right.w(), el_right.h());
			}
		);

		m_is_storage_dirty = false;
	}

	u64 queryCode = pMortonCodeCalculate(x, y);

	// Binary search for the closest Morton code
	auto it = std::lower_bound
	(
		m_atlas_items_spatial_indexing.begin(), m_atlas_items_spatial_indexing.end(), pMortonCodeCalculate(0.0f, 0.0f),
		[pMortonCodeCalculate, queryCode, this](const CTAESpatialIndex& p, const u64)
		{
			R_ASSERT(p.lookup_id >= 0 && "must be initialized and valid!");

			const CTextureAtlasElement& el = m_atlas_items[p.lookup_id];

			return pMortonCodeCalculate(el.w(), el.h()) < queryCode;
		}
	);

	// Check if we're at the beginning or end
	if (it == m_atlas_items_spatial_indexing.begin())
	{
		return 0;
	}

	if (it == m_atlas_items_spatial_indexing.end())
	{
		return static_cast<element_lookupid_type>(m_atlas_items_spatial_indexing.size() - 1);
	}

	// Compare with previous element to find which is closer
	element_lookupid_type idx = static_cast<element_lookupid_type>(it - m_atlas_items_spatial_indexing.begin());

	const CTextureAtlasElement& el_code1 = m_atlas_items[m_atlas_items_spatial_indexing[idx].lookup_id];
	const CTextureAtlasElement& el_code2 = m_atlas_items[m_atlas_items_spatial_indexing[idx - 1].lookup_id];

	u64 code1 = pMortonCodeCalculate(el_code1.w(), el_code1.h());
	u64 code2 = pMortonCodeCalculate(el_code2.w(), el_code2.h());

	return static_cast<element_lookupid_type>((std::abs(static_cast<int64_t>(queryCode - code1)) < std::abs(static_cast<int64_t>(queryCode - code2))) ? idx : idx - 1);
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
	VERIFY(!shader_was_created && "you must call only once!");
	VERIFY(!m_p_shader && "must be not inited!");
	R_ASSERT(m_p_texture && "early calling, texture must exist!");

	if (!m_p_shader && m_p_texture)
	{
		m_p_shader = new FactoryPtr<IUIShader>();
		R_ASSERT(m_p_shader && "failed to allocate shader! (CPU)");

		if (m_p_shader)
		{
			char buf[128];
			xr_sprintf(buf, sizeof(buf), "hud%sdefault", Platform::kPreferredSeparator);
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
	m_ss_wrapper{ &m_static_storage, sizeof(m_static_storage), flags & static_cast<u32>(eSVGStorageFlags::kFeatureSVGStorage_Static_Allocation) ? std::pmr::null_memory_resource() : std::pmr::get_default_resource() },
	m_storage_atlases{ std::pmr::polymorphic_allocator<CTextureAtlas>{&m_ss_wrapper} }
{
	R_ASSERT(!(flags & static_cast<u32>(eSVGStorageFlags::kFeatureSVGStorage_Static_Allocation) && flags & static_cast<u32>(eSVGStorageFlags::kFeatureSVGStorage_Dynamic_Allocation)) && "invalid flags");

	// if allocation size is changed in static mode you will get throw bad_alloc due to fact that required allocation formula was changed so in such case you have to change the size of static_storage field please
	m_storage_atlases.reserve(_kRenderBackend_SVGStorageSizeInitial);
}

CSVGStorage::~CSVGStorage()
{
	VERIFY(!m_init_was_called && "you forgot to call uninit!");
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
	m_docLruSlots.clear();
	m_readBuffer.clear();
	m_frameShaderUvCache.clear();
	m_loggedSvgFailures.clear();
	m_atlasEntryStats.clear();
	m_default_atlas.uninit();
	xr_delete(m_p_default_shader);

	for (CTextureAtlas& atlas : m_storage_atlases)
	{
		atlas.uninit();
	}

	m_storage_atlases.clear();
	m_storage_textures.clear();
	m_atlasIdToStorageIndex.clear();
	m_atlas_index_generator = 0;

#ifdef DEBUG
	m_init_was_called = false;
	m_debugDocCacheHits = 0;
	m_debugDocCacheMisses = 0;
	m_debugNewAtlasAllocCount = 0;
	m_debugRenderToBitmapNsAccum = 0;
	m_debugRenderToBitmapSamples = 0;
#endif
}

// returns preallocated size that was specified initially (but it doesn't show current size)
constexpr unsigned char CSVGStorage::get_static_size() const
{
	return _kRenderBackend_SVGStorageSizeInitial;
}

// returns current size of storage
u32 CSVGStorage::get_size() const
{
	return static_cast<u32>(m_storage_atlases.size());
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
		return &m_default_atlas;

	const auto it = m_atlasIdToStorageIndex.find(id);
	if (it != m_atlasIdToStorageIndex.end())
		return &m_storage_atlases[it->second];

	return nullptr;
}

const CTextureAtlas* CSVGStorage::get_atlas(u32 id) const
{
	if (id == _kSVGStorage_DefaultAtlasID)
		return &m_default_atlas;

	const auto it = m_atlasIdToStorageIndex.find(id);
	if (it != m_atlasIdToStorageIndex.end())
		return &m_storage_atlases[it->second];

	return nullptr;
}

const std::pmr::vector<CTextureAtlas>& CSVGStorage::get_atlases(void) const
{
	return m_storage_atlases;
}

const FactoryPtr<IUIShader>& CSVGStorage::get_shader(const std::string_view& subpath, float requested_width, float requested_height, SVGTintRGBA tint)
{
	R_ASSERT(m_p_default_shader && "default shader must be initialized!");

	const FactoryPtr<IUIShader>* pShader = nullptr;
	Frect uvUnused{};
	ResolveSvgRasterDraw(subpath, requested_width, requested_height, tint, &pShader, &uvUnused);
	if (!pShader)
		return get_default_shader();
	return *pShader;
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

Frect CSVGStorage::get_uv(const std::string_view& subpath, float requested_width, float requested_height, SVGTintRGBA tint)
{
	const FactoryPtr<IUIShader>* pShader = nullptr;
	Frect uv{};
	ResolveSvgRasterDraw(subpath, requested_width, requested_height, tint, &pShader, &uv);
	return uv;
}

void CSVGStorage::BeginRasterFrameCache()
{
	m_frameShaderUvCache.clear();
}

void CSVGStorage::SetRasterUserScale(float scale)
{
	m_rasterUserScale = (scale > 0.f) ? scale : 1.f;
}

void CSVGStorage::SetMaxRasterPixels(u32 pixels)
{
	m_maxRasterPixels = (pixels > 0) ? pixels : _kSVGStorage_DefaultMaxRasterPixels;
}

void CSVGStorage::PrecacheSVG(std::string_view subpath, float width, float height, SVGTintRGBA tint)
{
	if (subpath.empty())
		return;
	const FactoryPtr<IUIShader>* pShader = nullptr;
	Frect uv{};
	ResolveSvgRasterDraw(subpath, width, height, tint, &pShader, &uv);
}

void CSVGStorage::PrecacheSVGList(const xr_vector<xr_string>& paths, float width, float height, SVGTintRGBA tint)
{
	for (const xr_string& p : paths)
		PrecacheSVG(p, width, height, tint);
}

void CSVGStorage::InvalidateAllSvgDocuments()
{
	m_docLruSlots.clear();
}

void CSVGStorage::ReloadSVG(std::string_view subpath)
{
	EraseDocumentLruByPathKey(MakeSubpathKey(subpath));
}

u64 CSVGStorage::MakeFrameCacheKey(const xr_string& atlasTableKey, int pixelW, int pixelH) const
{
	u64 h = static_cast<u64>(std::hash<xr_string>{}(atlasTableKey));
	h = SvgHashCombineU64(h, static_cast<u64>(static_cast<u32>(pixelW)));
	h = SvgHashCombineU64(h, static_cast<u64>(static_cast<u32>(pixelH)));
	return h;
}

xr_string CSVGStorage::BuildAtlasTableKey(const std::string_view& filesystemSubpath, SVGTintRGBA tint) const
{
	xr_string key = MakeSubpathKey(filesystemSubpath);
	if (!tint.IsWhite())
	{
		char suffix[24];
		xr_sprintf(suffix, sizeof(suffix), "\x1f%08x", tint.PackKey());
		key += suffix;
	}
	return key;
}

void CSVGStorage::NormalizeRasterRequest(float requestedWidth, float requestedHeight, int& outW, int& outH) const
{
	const float w = requestedWidth * m_rasterUserScale;
	const float h = requestedHeight * m_rasterUserScale;
	int iw = static_cast<int>(std::lroundf(std::max(1.f, w)));
	int ih = static_cast<int>(std::lroundf(std::max(1.f, h)));
	const int cap = static_cast<int>(m_maxRasterPixels);
	iw = std::min(iw, cap);
	ih = std::min(ih, cap);
	outW = iw;
	outH = ih;
}

bool CSVGStorage::ConnectionHasExactPixelSize(const AtlasConnection& connection, int pixelW, int pixelH) const
{
	for (int i = 0; i < _kSVGStorage_MaxAtlasPlacement; ++i)
	{
		if (connection.atlas_ids[i] == CTextureAtlas::element_lookupid_type(-1))
			continue;
		const size_t atlasStorageIdx = static_cast<size_t>(static_cast<unsigned char>(connection.atlas_ids[i]));
		const CTextureAtlas& atlas = m_storage_atlases[atlasStorageIdx];
		const CTextureAtlas::storage_type& elements = atlas.getElements();
		for (int j = 0; j < _kSVGStorage_MaxElementsPerAtlas; ++j)
		{
			const CTextureAtlas::element_lookupid_type element_slot = j + (i * _kSVGStorage_MaxElementsPerAtlas);
			if (connection.elements_per_atlas[element_slot] == CTextureAtlas::element_lookupid_type(-1))
				break;
			const CTextureAtlas::CTextureAtlasElement& element = elements[static_cast<size_t>(static_cast<unsigned char>(connection.elements_per_atlas[element_slot]))];
			if (element.w() == pixelW && element.h() == pixelH)
				return true;
		}
	}
	return false;
}

bool CSVGStorage::FindNearestCachedPixelSize(const AtlasConnection& connection, int pixelW, int pixelH, int& outW, int& outH) const
{
	bool hasAny = false;
	int bestScore = INT_MAX;
	int bestW = 0;
	int bestH = 0;
	for (int i = 0; i < _kSVGStorage_MaxAtlasPlacement; ++i)
	{
		if (connection.atlas_ids[i] == CTextureAtlas::element_lookupid_type(-1))
			continue;
		const size_t atlasStorageIdx = static_cast<size_t>(static_cast<unsigned char>(connection.atlas_ids[i]));
		const CTextureAtlas& atlas = m_storage_atlases[atlasStorageIdx];
		const CTextureAtlas::storage_type& elements = atlas.getElements();
		for (int j = 0; j < _kSVGStorage_MaxElementsPerAtlas; ++j)
		{
			const CTextureAtlas::element_lookupid_type element_slot = j + (i * _kSVGStorage_MaxElementsPerAtlas);
			if (connection.elements_per_atlas[element_slot] == CTextureAtlas::element_lookupid_type(-1))
				break;
			const CTextureAtlas::CTextureAtlasElement& element = elements[static_cast<size_t>(static_cast<unsigned char>(connection.elements_per_atlas[element_slot]))];
			const int ew = static_cast<int>(element.w());
			const int eh = static_cast<int>(element.h());
			const int score = std::abs(ew - pixelW) + std::abs(eh - pixelH);
			if (!hasAny || score < bestScore)
			{
				hasAny = true;
				bestScore = score;
				bestW = ew;
				bestH = eh;
			}
		}
	}
	if (!hasAny)
		return false;
	outW = bestW;
	outH = bestH;
	return true;
}

void CSVGStorage::ResolveRasterDimensions(const xr_string& atlasTableKey, float requestedWidth, float requestedHeight, int& useW, int& useH) const
{
	int nw = 0;
	int nh = 0;
	NormalizeRasterRequest(requestedWidth, requestedHeight, nw, nh);
	const auto it = m_storage_textures.find(atlasTableKey);
	if (it == m_storage_textures.end())
	{
		useW = nw;
		useH = nh;
		return;
	}
	if (ConnectionHasExactPixelSize(it->second, nw, nh))
	{
		useW = nw;
		useH = nh;
		return;
	}
	int bw = 0;
	int bh = 0;
	if (FindNearestCachedPixelSize(it->second, nw, nh, bw, bh))
	{
		useW = bw;
		useH = bh;
		return;
	}
	useW = nw;
	useH = nh;
}

void CSVGStorage::FillDefaultAtlasUvForSize(int useW, int useH, Frect& result) const
{
	result = {};
	if (!m_default_atlas.getResource())
		return;
	const CTextureAtlas::CTextureAtlasElement* pElement = m_default_atlas.findNearest(static_cast<float>(useW), static_cast<float>(useH));
	if (!pElement)
		return;
	const float w = m_default_atlas.getWidth();
	const float h = m_default_atlas.getHeight();
	result.lt.set(w * pElement->u0(static_cast<u32>(w)), h * pElement->v0(static_cast<u32>(h)));
	result.rb.set(w * pElement->u1(static_cast<u32>(w)), h * pElement->v1(static_cast<u32>(h)));
}

bool CSVGStorage::TryLookupUvForSize(AtlasConnection& connection, int pixelW, int pixelH, Frect& outUv, bool useNearest)
{
	int rw = pixelW;
	int rh = pixelH;
	if (useNearest && !ConnectionHasExactPixelSize(connection, pixelW, pixelH))
	{
		int nw = 0;
		int nh = 0;
		if (FindNearestCachedPixelSize(connection, pixelW, pixelH, nw, nh))
		{
			rw = nw;
			rh = nh;
		}
	}
	for (int i = 0; i < _kSVGStorage_MaxAtlasPlacement; ++i)
	{
		if (connection.atlas_ids[i] == CTextureAtlas::element_lookupid_type(-1))
			continue;
		CTextureAtlas& atlas = m_storage_atlases[static_cast<size_t>(static_cast<unsigned char>(connection.atlas_ids[i]))];
		if (atlas.getShader() == nullptr || atlas.getResource() == nullptr)
			continue;
		const CTextureAtlas::storage_type& elements = atlas.getElements();
		for (int j = 0; j < _kSVGStorage_MaxElementsPerAtlas; ++j)
		{
			const int real_j = j + (_kSVGStorage_MaxElementsPerAtlas * i);
			const CTextureAtlas::element_lookupid_type el_id = connection.elements_per_atlas[real_j];
			if (el_id == CTextureAtlas::element_lookupid_type(-1))
				break;
			const CTextureAtlas::CTextureAtlasElement& element = elements[static_cast<size_t>(static_cast<unsigned char>(el_id))];
			if (element.w() == rw && element.h() == rh)
			{
				const float w = atlas.getWidth();
				const float h = atlas.getHeight();
				outUv.lt.set(w * element.u0(static_cast<u32>(w)), h * element.v0(static_cast<u32>(h)));
				outUv.rb.set(w * element.u1(static_cast<u32>(w)), h * element.v1(static_cast<u32>(h)));
				return true;
			}
		}
	}
	return false;
}

void CSVGStorage::TouchAtlasEntryStats(const xr_string& atlasTableKey)
{
	SvgAtlasEntryStats& st = m_atlasEntryStats[atlasTableKey];
	st.lastAccessSeq = ++m_globalAccessSeq;
}

void CSVGStorage::RegisterNewRasterVariant(const xr_string& atlasTableKey, int pixelW, int pixelH)
{
	SvgAtlasEntryStats& st = m_atlasEntryStats[atlasTableKey];
	st.variantCount += 1;
	st.totalRasterPixels += static_cast<u32>(pixelW) * static_cast<u32>(pixelH);
	st.lastAccessSeq = ++m_globalAccessSeq;
}

void CSVGStorage::LogSvgLoadFailureOnce(const xr_string& pathKey, ESVGLoadResult code)
{
	if (m_loggedSvgFailures.insert(pathKey).second == false)
		return;
	const char* reason = "unknown";
	switch (code)
	{
		case ESVGLoadResult::PathTooLong:
			reason = "path too long";
			break;
		case ESVGLoadResult::FileOpenFailed:
			reason = "file open failed";
			break;
		case ESVGLoadResult::ParseFailed:
			reason = "parse failed";
			break;
		default:
			reason = "unknown";
			break;
	}
	Msg("! [svg]: load failed (%s) [%s]", reason, pathKey.c_str());
}

void CSVGStorage::EraseDocumentLruByPathKey(const xr_string& pathKey)
{
	for (size_t i = 0; i < m_docLruSlots.size(); ++i)
	{
		if (m_docLruSlots[i].pathKey == pathKey)
		{
			m_docLruSlots.erase(m_docLruSlots.begin() + static_cast<ptrdiff_t>(i));
			return;
		}
	}
}

CSVGStorage::SvgDocumentLruEntry* CSVGStorage::AccessDocumentLru(const xr_string& pathKey, LPCSTR resolvedPathForValidate)
{
	for (size_t i = 0; i < m_docLruSlots.size(); ++i)
	{
		SvgDocumentLruEntry& slot = m_docLruSlots[i];
		if (slot.pathKey != pathKey)
			continue;
		const time_t diskMtime = FS.get_file_age(resolvedPathForValidate);
		if (diskMtime != slot.fileMtimeAtLoad)
		{
			m_docLruSlots.erase(m_docLruSlots.begin() + static_cast<ptrdiff_t>(i));
#ifdef DEBUG
			++m_debugDocCacheMisses;
#endif
			return nullptr;
		}
#ifdef DEBUG
		++m_debugDocCacheHits;
#endif
		slot.lastAccessSeq = ++m_docLruSeqCounter;
		return &slot;
	}
	return nullptr;
}

void CSVGStorage::InsertDocumentLru(const xr_string& pathKey, LPCSTR resolvedPath, std::unique_ptr<lunasvg::Document> doc, time_t mtime, u32 fileSize)
{
	EraseDocumentLruByPathKey(pathKey);
	SvgDocumentLruEntry slot;
	slot.pathKey = pathKey;
	slot.resolvedFsPath = resolvedPath;
	slot.doc = std::move(doc);
	slot.fileMtimeAtLoad = mtime;
	slot.fileSizeAtLoad = fileSize;
	slot.lastAccessSeq = ++m_docLruSeqCounter;
	m_docLruSlots.push_back(std::move(slot));
	while (m_docLruSlots.size() > _kSVGStorage_DocumentLruCapacity)
	{
		auto oldest = std::min_element(m_docLruSlots.begin(), m_docLruSlots.end(), [](const SvgDocumentLruEntry& a, const SvgDocumentLruEntry& b) {
			return a.lastAccessSeq < b.lastAccessSeq;
		});
		if (oldest != m_docLruSlots.end())
			m_docLruSlots.erase(oldest);
	}
}

#ifdef DEBUG
void CSVGStorage::DebugRecordRenderToBitmapTime(u64 deltaTicks)
{
	if (CPU::qpc_freq == 0)
		return;
	const u64 ns = (deltaTicks * 1000000000ULL) / CPU::qpc_freq;
	m_debugRenderToBitmapNsAccum += ns;
	++m_debugRenderToBitmapSamples;
}

void CSVGStorage::DebugCollectSvgCacheRows(xr_vector<SvgDebugCacheTableRow>& out) const
{
	out.clear();
	out.reserve(m_storage_textures.size());
	for (const auto& kv : m_storage_textures)
	{
		SvgDebugCacheTableRow row;
		row.tableKey = kv.first;
		const auto stIt = m_atlasEntryStats.find(kv.first);
		if (stIt != m_atlasEntryStats.end())
		{
			row.variantCount = stIt->second.variantCount;
			row.totalRasterPixels = stIt->second.totalRasterPixels;
			row.lastAccessSeq = stIt->second.lastAccessSeq;
		}
		out.push_back(std::move(row));
	}
}

void CSVGStorage::DebugResetSvgMetrics()
{
	m_debugDocCacheHits = 0;
	m_debugDocCacheMisses = 0;
	m_debugNewAtlasAllocCount = 0;
	m_debugRenderToBitmapNsAccum = 0;
	m_debugRenderToBitmapSamples = 0;
}
#endif

void CSVGStorage::ResolveSvgRasterDraw(const std::string_view& filesystemSubpath, float requestedWidth, float requestedHeight, SVGTintRGBA tint, const FactoryPtr<IUIShader>** outShader, Frect* outUv)
{
	R_ASSERT(outShader && outUv);
	*outShader = nullptr;
	*outUv = Frect();

	if (filesystemSubpath.empty() || filesystemSubpath == _kDefaultSVGShader)
	{
		if (m_p_default_shader)
			*outShader = m_p_default_shader;
		int uw = 0;
		int uh = 0;
		NormalizeRasterRequest(requestedWidth, requestedHeight, uw, uh);
		FillDefaultAtlasUvForSize(uw, uh, *outUv);
		return;
	}

	const xr_string atlasTableKey = BuildAtlasTableKey(filesystemSubpath, tint);
	int useW = 0;
	int useH = 0;
	ResolveRasterDimensions(atlasTableKey, requestedWidth, requestedHeight, useW, useH);

	const u64 frameKey = MakeFrameCacheKey(atlasTableKey, useW, useH);
	const auto fIt = m_frameShaderUvCache.find(frameKey);
	if (fIt != m_frameShaderUvCache.end())
	{
		*outShader = fIt->second.pShader;
		*outUv = fIt->second.uv;
		TouchAtlasEntryStats(atlasTableKey);
		return;
	}

	const FactoryPtr<IUIShader>* pFoundShader = nullptr;
	Frect uvRect{};
	bool hasUv = false;

	auto itEntry = m_storage_textures.find(atlasTableKey);
	if (itEntry == m_storage_textures.end())
	{
		AtlasConnection lookup = try_allocate(atlasTableKey, filesystemSubpath, static_cast<float>(useW), static_cast<float>(useH), nullptr, tint);
		R_ASSERT(lookup.isValid() && "failed to allocate!");
		m_storage_textures.insert_or_assign(atlasTableKey, lookup);
		const char idx = lookup.atlas_ids[0];
		CTextureAtlas& atlas = m_storage_atlases[static_cast<size_t>(static_cast<unsigned char>(idx))];
		FactoryPtr<IUIShader>* pSh = atlas.getShader();
		R_ASSERT(pSh && "must be valid!");
		pFoundShader = pSh;
		hasUv = TryLookupUvForSize(lookup, useW, useH, uvRect, true);
	}
	else
	{
		AtlasConnection& lookupList = itEntry->second;
		bool foundExact = false;
		for (int i = 0; i < _kSVGStorage_MaxAtlasPlacement; ++i)
		{
			if (lookupList.atlas_ids[i] == CTextureAtlas::element_lookupid_type(-1))
				continue;
			const size_t atlasStorageIdx = static_cast<size_t>(static_cast<unsigned char>(lookupList.atlas_ids[i]));
			const CTextureAtlas& atlas = m_storage_atlases[atlasStorageIdx];
			const CTextureAtlas::storage_type& elements = atlas.getElements();
			for (int j = 0; j < _kSVGStorage_MaxElementsPerAtlas; ++j)
			{
				const CTextureAtlas::element_lookupid_type element_slot = j + (i * _kSVGStorage_MaxElementsPerAtlas);
				if (lookupList.elements_per_atlas[element_slot] == CTextureAtlas::element_lookupid_type(-1))
					break;
				const CTextureAtlas::CTextureAtlasElement& element = elements[static_cast<size_t>(static_cast<unsigned char>(lookupList.elements_per_atlas[element_slot]))];
				if (element.w() == useW && element.h() == useH)
				{
					FactoryPtr<IUIShader>* pSh = atlas.getShader();
					R_ASSERT(pSh && "must be initialized");
					pFoundShader = pSh;
					const float w = atlas.getWidth();
					const float h = atlas.getHeight();
					uvRect.lt.set(w * element.u0(static_cast<u32>(w)), h * element.v0(static_cast<u32>(h)));
					uvRect.rb.set(w * element.u1(static_cast<u32>(w)), h * element.v1(static_cast<u32>(h)));
					hasUv = true;
					foundExact = true;
					break;
				}
			}
			if (foundExact)
				break;
		}
		if (!foundExact)
		{
			AtlasConnection lookup = try_allocate(atlasTableKey, filesystemSubpath, static_cast<float>(useW), static_cast<float>(useH), &lookupList, tint);
			R_ASSERT(lookup.isValid() && "failed to allocate!");
			const char idx = lookup.atlas_ids[0];
			CTextureAtlas& atlas = m_storage_atlases[static_cast<size_t>(static_cast<unsigned char>(idx))];
			pFoundShader = atlas.getShader();
			R_ASSERT(pFoundShader && "must be valid!");
			hasUv = TryLookupUvForSize(lookup, useW, useH, uvRect, true);
		}
	}

	if (!pFoundShader)
	{
		if (m_p_default_shader)
			*outShader = m_p_default_shader;
		FillDefaultAtlasUvForSize(useW, useH, *outUv);
		return;
	}

	if (!hasUv)
		FillDefaultAtlasUvForSize(useW, useH, uvRect);

	TouchAtlasEntryStats(atlasTableKey);
	*outShader = pFoundShader;
	*outUv = uvRect;
	m_frameShaderUvCache[frameKey] = { pFoundShader, uvRect };
}


void CSVGStorage::init_default()
{
	init_default_atlas();
	init_default_shader();
}

void CSVGStorage::init_default_atlas()
{
	string_path fn;
	FS.update_path(fn, _game_textures_, _kSVGStorage_DefaultSVGTextureSubPathName);

	IReader* pReader = FS.r_open(fn);

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
#endif

				m_default_atlas.addRegion(_notused_lookupid, _kSVGStorage_DefaultSVGTextureSubPathName, bmp.width(), bmp.height(), bmp.data(), bmp.stride());
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

CSVGStorage::AtlasConnection CSVGStorage::try_allocate(const xr_string& atlasTableKey, const std::string_view& filesystemSubpath, float requested_width, float requested_height, AtlasConnection* p_existed, SVGTintRGBA tint)
{
	AtlasConnection result;

	CTextureAtlas::element_lookupid_type iter = 0;
	bool was_added = false;
	for (CTextureAtlas& atlas : m_storage_atlases)
	{
		const bool status = try_add_data(atlasTableKey, filesystemSubpath, requested_width, requested_height, iter, atlas, p_existed ? *p_existed : result, tint);

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
		result = allocate(atlasTableKey, filesystemSubpath, requested_width, requested_height, tint);

	return result;
}

CSVGStorage::AtlasConnection CSVGStorage::allocate(const xr_string& atlasTableKey, const std::string_view& filesystemSubpath, float requested_width, float requested_height, SVGTintRGBA tint)
{
	AtlasConnection result;

	if (requested_width <= _kSVGStorage_DefaultAtlasSize && requested_height <= _kSVGStorage_DefaultAtlasSize)
	{
		char texture_name[32];

		xr_sprintf(texture_name, sizeof(texture_name), "svg_atlas_%zu", m_storage_atlases.size());

		CTextureAtlas atlas;
		const u32 atlas_id = init_atlas(_kSVGStorage_DefaultAtlasSize, _kSVGStorage_DefaultAtlasSize, texture_name, atlas, true);
		atlas.setID(atlas_id);

		R_ASSERT2(requested_height <= atlas.getHeight(), "invalid height! Too big height");
		R_ASSERT2(requested_width <= atlas.getWidth(), "invalid width! Too big width");

		const bool data_insert_status = add_data(atlasTableKey, filesystemSubpath, requested_width, requested_height, atlas, result, tint);

		R_ASSERT2(data_insert_status, "failed to insert data to atlas");

		if (data_insert_status)
		{
			R_ASSERT(atlas.getShader() == nullptr && "must be nullptr!");

			atlas.createShader();

#ifdef DEBUG
			Msg("[svg]: allocated atlas[id:%d;w:%d;h:%d;tex_name:%s] and addded region w: %.2f h: %.2f ",
				atlas.getID(),
				atlas.getWidth(), atlas.getHeight(),
				atlas.getTextureName(),
				requested_width, requested_height
			);
			++m_debugNewAtlasAllocCount;
#endif
			m_storage_atlases.emplace_back(std::move(atlas));
			const u32 storageIndex = static_cast<u32>(m_storage_atlases.size() - 1);
			result.atlas_ids[0] = static_cast<char>(storageIndex);
			m_atlasIdToStorageIndex[m_storage_atlases[storageIndex].getID()] = storageIndex;
		}
	}

	return result;
}

bool CSVGStorage::add_data(const xr_string& atlasTableKey, const std::string_view& filesystemSubpath, float requested_width, float requested_height, CTextureAtlas& atlas, AtlasConnection& connection, SVGTintRGBA tint)
{
	R_ASSERT(filesystemSubpath.empty() == false && "must be valid!");

	bool result = false;

	if (filesystemSubpath.empty() == false)
	{
		lunasvg::Bitmap bmp;
		const ESVGLoadResult loadRes = get_bitmap(filesystemSubpath, requested_width, requested_height, &bmp, tint);
		result = loadRes == ESVGLoadResult::Success;

		R_ASSERT(result && "failed to obtain bitmap!");

		if (result)
		{
			CTextureAtlas::element_lookupid_type lookup_element_id;
			const xr_string_view atlasKeyView{ atlasTableKey.c_str(), atlasTableKey.size() };
			result = atlas.addRegion(lookup_element_id, atlasKeyView, bmp.width(), bmp.height(), bmp.data(), bmp.stride());

			R_ASSERT(connection.atlas_ids[0] == CTextureAtlas::element_lookupid_type(-1) && "expected minus one because it is not existed in map!");
			R_ASSERT(connection.elements_per_atlas[0] == CTextureAtlas::element_lookupid_type(-1) && "expected minus one because it is not existed in map!");

			connection.elements_per_atlas[0] = lookup_element_id;
			RegisterNewRasterVariant(atlasTableKey, static_cast<int>(bmp.width()), static_cast<int>(bmp.height()));
		}
	}

	return result;
}

bool CSVGStorage::try_add_data(const xr_string& atlasTableKey, const std::string_view& filesystemSubpath, float requested_width, float requested_height, const CTextureAtlas::element_lookupid_type atlas_lookup_id, CTextureAtlas& atlas, AtlasConnection& connection, SVGTintRGBA tint)
{
	bool result = false;

	CTextureAtlas::element_lookupid_type lookup_el_id;
	const xr_string_view atlasKeyView{ atlasTableKey.c_str(), atlasTableKey.size() };
	result = atlas.tryAddRegion(lookup_el_id, atlasKeyView, requested_width, requested_height);

	if (lookup_el_id != CTextureAtlas::element_lookupid_type(-1))
	{
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

			if (connection.atlas_ids[i] == atlas_lookup_id)
			{
				filled_atlas_info = true;
				break;
			}
		}

		R_ASSERT(filled_atlas_info && "probably overflow it means we can't insert new information to existed connection");

		if (filled_atlas_info)
		{
			bool filled_element_info = false;
			for (int j = 0; j < _kSVGStorage_MaxElementsPerAtlas; ++j)
			{
				const int connection_el_id = j + (atlas_lookup_id * _kSVGStorage_MaxElementsPerAtlas);

				if (connection.elements_per_atlas[connection_el_id] == CTextureAtlas::element_lookupid_type(-1))
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

		const ESVGLoadResult loadRes = get_bitmap(filesystemSubpath, requested_width, requested_height, &bmp, tint);
		result = loadRes == ESVGLoadResult::Success;

		R_ASSERT(result && "failed to obtain data!");

		if (result)
		{
			result = atlas.addData(bmp.width(), bmp.height(), bmp.data(), bmp.stride());
			RegisterNewRasterVariant(atlasTableKey, static_cast<int>(bmp.width()), static_cast<int>(bmp.height()));
		}
	}

	return result;
}

xr_string CSVGStorage::MakeSubpathKey(const std::string_view& subpath)
{
	return xr_string(subpath.data(), static_cast<u32>(subpath.size()));
}

ESVGLoadResult CSVGStorage::get_bitmap(const std::string_view& filesystemSubpath, float requested_width, float requested_height, lunasvg::Bitmap* bmp, SVGTintRGBA tint)
{
	R_ASSERT(bmp && "pass valid pointer!");

	const xr_string pathKey = MakeSubpathKey(filesystemSubpath);

	char buf[256];
	constexpr size_t bufElemCount = sizeof(buf) / sizeof(buf[0]);
	const size_t prefixLen = 2 + xr_strlen(Platform::kPreferredSeparator);
	if (filesystemSubpath.size() + prefixLen + 1 > bufElemCount)
	{
		LogSvgLoadFailureOnce(pathKey, ESVGLoadResult::PathTooLong);
		return ESVGLoadResult::PathTooLong;
	}

	xr_sprintf(buf, sizeof(buf), "ui%s%.*s", Platform::kPreferredSeparator, static_cast<int>(filesystemSubpath.size()), filesystemSubpath.data());

	string_path fn;
	FS.update_path(fn, _game_textures_, buf);

	lunasvg::Document* docPtr = nullptr;
	SvgDocumentLruEntry* slot = AccessDocumentLru(pathKey, fn);
	if (slot && slot->doc.get())
		docPtr = slot->doc.get();
	else
	{
#ifdef DEBUG
		++m_debugDocCacheMisses;
#endif
		IReader* pReader = FS.r_open(fn);
		if (!pReader)
		{
			LogSvgLoadFailureOnce(pathKey, ESVGLoadResult::FileOpenFailed);
			return ESVGLoadResult::FileOpenFailed;
		}

		const u32 len = pReader->length();
		if (m_readBuffer.capacity() < len)
			m_readBuffer.reserve(len);
		m_readBuffer.resize(len);
		pReader->r(&m_readBuffer[0], len);
		FS.r_close(pReader);

		auto doc = std::move(lunasvg::Document::loadFromData(m_readBuffer.c_str()));
		if (!doc.get())
		{
			LogSvgLoadFailureOnce(pathKey, ESVGLoadResult::ParseFailed);
			return ESVGLoadResult::ParseFailed;
		}

		const time_t mtime = FS.get_file_age(fn);
		InsertDocumentLru(pathKey, fn, std::move(doc), mtime, len);
		slot = AccessDocumentLru(pathKey, fn);
		if (!slot || !slot->doc.get())
			return ESVGLoadResult::ParseFailed;
		docPtr = slot->doc.get();
	}

#ifdef DEBUG
	const u64 t0 = CPU::QPC();
#endif
	*bmp = docPtr->renderToBitmap(requested_width, requested_height);
#ifdef DEBUG
	const u64 t1 = CPU::QPC();
	DebugRecordRenderToBitmapTime(t1 - t0);
#endif

#ifdef USE_DX11
	bmp->convertToRGBA();
#endif
	ApplySvgTintToNearWhitePixels(*bmp, tint);
	return ESVGLoadResult::Success;
}

u32 CSVGStorage::generate_id()
{
	++m_atlas_index_generator;

	// we should regenerate id due to avoiding collision with defined id
	if (m_atlas_index_generator == _kSVGStorage_DefaultAtlasID)
		++m_atlas_index_generator;

	return m_atlas_index_generator;
}