#include "xr_scene.h"
#include "xr_scene_portals.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_portal_object::xr_portal_object(xr_scene& scene):
	xr_custom_object(scene, TOOLS_CLASS_PORTAL) {}

xr_portal_object::~xr_portal_object() {}

void xr_portal_object::load(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk(PORTAL_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == PORTAL_VERSION);
	xr_custom_object::load(r);
	if (r.find_chunk(PORTAL_CHUNK_SECTOR_FRONT)) {
		r.r_sz(m_sector_front);
	}
	if (r.find_chunk(PORTAL_CHUNK_SECTOR_BACK))
		r.r_sz(m_sector_back);
	xr_assert(!m_sector_front.empty() || !m_sector_back.empty());
	if (!r.find_chunk(PORTAL_CHUNK_VERTICES))
		xr_not_expected();
	size_t n = r.r_u16();
	xr_assert(n > 3);
	r.r_seq(n, m_vertices);
}

void xr_portal_object::save_v12(xr_ini_writer* w) const
{
	xr_custom_object::save_v12(w);

	w->write("version", PORTAL_VERSION);
	
	w->write("sector_back", this->m_sector_back, false);
	w->write("sector_front", this->m_sector_front, false);

	w->write("vert_count", this->m_vertices.size());
	w->w_ini_seq(m_vertices, "vertex");
}

void xr_portal_object::save(xr_writer& w) const
{
	xr_custom_object::save(w);
	w.w_chunk<uint16_t>(PORTAL_CHUNK_VERSION, PORTAL_VERSION);
	if (!m_sector_front.empty())
		w.w_chunk(PORTAL_CHUNK_SECTOR_FRONT, m_sector_front);
	if (!m_sector_back.empty())
		w.w_chunk(PORTAL_CHUNK_SECTOR_BACK, m_sector_back);
	w.open_chunk(PORTAL_CHUNK_VERTICES);
	w.w_size_u16(m_vertices.size());
	w.w_seq(m_vertices);
	w.close_chunk();
}

////////////////////////////////////////////////////////////////////////////////

xr_scene_portals::xr_scene_portals(xr_scene& scene):
	xr_scene_objects(scene, "portal.part", SCENE_CHUNK_PORTALS),
	m_flags(0) {}

xr_scene_portals::~xr_scene_portals() {}

void xr_scene_portals::load(xr_reader& r)
{
	uint16_t version = 0;
	r.r_chunk<uint16_t>(TOOLS_CHUNK_VERSION, version);
	xr_assert(version == 0);
	xr_scene_objects::load(r);
	r.r_chunk<uint32_t>(PORTALS_CHUNK_COMMON_FLAGS, m_flags);
}

void xr_scene_portals::save(xr_writer& w) const
{
	xr_scene_objects::save(w);
	w.w_chunk<uint16_t>(TOOLS_CHUNK_VERSION, 0);
	w.w_chunk<uint32_t>(PORTALS_CHUNK_COMMON_FLAGS, m_flags);
}

void xr_scene_portals::save_v12(xr_ini_writer* w) const
{
	w->open_section("main");
	w->write("flags", this->m_flags);
	w->write("objects_count", this->objects().size());
	w->write("version", 0);
	w->close_section();

	scene().write_revision(w);

	xr_scene_objects::save_v12(w);
}
