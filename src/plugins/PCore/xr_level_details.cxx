#include "xr_level_version.h"
#include "xr_level_details.h"
#include "xr_level_dm.h"
#include "xr_image.h"
#if 1
#include "xr_file_system.h"
#else
#include "xr_reader.h"
#include "xr_writer.h"
#endif
#include "xr_utils.h"

using namespace xray_re;

xr_level_details::~xr_level_details()
{
	delete[] m_slots;
	delete_elements(m_models);
	delete m_texture;
	delete[] m_raw_texture;
}

struct read_slot_v2 { void operator()(detail_slot_v3& ds, xr_reader& r) const {
	detail_slot_v2 ds2;
	r.r(ds2);
	if (ds2.y_top > ds2.y_base) {
		uint32_t y_base = uint32_t(std::floor((ds2.y_base + 200.f)*5.f));
		ds.y_base = y_base > 0xfff ? 0xfff : y_base;
		uint32_t y_height = uint32_t(std::ceil((ds2.y_top - ds.r_ybase())*10.f));
		ds.y_height = y_height > 0xff ? 0xff : y_height;
//xr_assert(ds2.y_base >= -200.f);
//xr_assert(ds.y_height != 0);
//xr_assert(ds.r_ybase() <= ds2.y_base + 0.0001);
//if (y_height <= 0xff)
//	xr_assert(ds.r_ybase() + ds.r_yheight() >= ds2.y_top);
	} else {
		ds.y_base = 0;
		ds.y_height = 0;
	}
	ds.id0 = ds2.part[0].id;
	ds.id1 = ds2.part[1].id;
	ds.id2 = ds2.part[2].id;
	ds.id3 = ds2.part[3].id;
	ds.c_dir = 7;
	ds.c_hemi = 7;
	ds.c_r = 0;
	ds.c_g = 0;
	ds.c_b = 0;
	for (uint_fast32_t i = 4; i != 0;) {
		--i;
		ds.palette[i] = ds2.part[i].palette;
	}
}};

void xr_level_details::load(xr_reader& r)
{
	if (!r.find_chunk(FSD_HEADER))
		xr_not_expected();
	m_header.load(r);
	r.debug_find_chunk();
	xr_assert(m_header.version == DETAILS_VERSION_2 || m_header.version == DETAILS_VERSION_3);

	if (m_header.object_count == 0)
		return;

	m_models.reserve(m_header.object_count);
	xr_reader* s = r.open_chunk(FSD_MESHES);
	xr_assert(s);
	for (uint32_t id = 0; s->find_chunk(id); ++id) {
		xr_dm* dm = new xr_level_dm();
		dm->load_dm(*s);
		m_models.push_back(dm);
	}
	r.close_chunk(s);

	size_t size = r.find_chunk(FSD_SLOTS);
	if (m_header.version == DETAILS_VERSION_2) {
		xr_assert(size && (size % sizeof(detail_slot_v2) == 0));
		m_slots = new detail_slot_v3[size /= sizeof(detail_slot_v2)];
		r.r_cseq(size, m_slots, read_slot_v2());
	} else if (m_header.version == DETAILS_VERSION_3) {
		xr_assert(size && (size % sizeof(detail_slot_v3) == 0));
		m_slots = new detail_slot_v3[size /= sizeof(detail_slot_v3)];
		r.r_cseq(size, m_slots);
	} else {
		xr_not_expected();
	}
	xr_assert(size == num_slots());
}

bool xr_level_details::load_texture(const char* path)
{
	if (m_models.empty())
		return false;
	std::string name(m_models[0]->texture());
	for (xr_dm_vec_it it = m_models.begin() + 1, end = m_models.end(); it != end; ++it) {
		if ((*it)->texture() != name)
			return false;
	}
	name += ".dds";
#if 1
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	m_raw_texture_size = r->size();
	r->r_raw(m_raw_texture = new uint8_t[m_raw_texture_size], m_raw_texture_size);
	fs.r_close(r);
	return true;
#else
	m_texture = new xr_image;
	if (m_texture->load_dds(path, name))
		return true;
	delete m_texture;
	m_texture = 0;
	return false;
#endif
}
