#include "xr_scene.h"
#include "xr_scene_ways.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_string_utils.h"

#include <map>

using namespace xray_re;

xr_way_object::xr_way_object(xr_scene& scene):
	xr_custom_object(scene, TOOLS_CLASS_WAY),
	m_type(WAY_TYPE_PATROL_PATH) {}

xr_way_object::~xr_way_object() {}

struct read_point_le { void operator()(way_point_le& point, xr_reader& r) {
	r.r_fvector3(point.position);
	point.flags = r.r_u32();
	point.id = r.r_u16();
	r.r_sz(point.name);
}};

void xr_way_object::load(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk<uint16_t>(WAYOBJECT_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == WAYOBJECT_VERSION);

	xr_custom_object::load(r);

	if (!r.find_chunk(WAYOBJECT_CHUNK_POINTS))
		xr_not_expected();
	r.r_seq(r.r_u16(), m_points, read_point_le());
	r.debug_find_chunk();

	if (!r.find_chunk(WAYOBJECT_CHUNK_LINKS))
		xr_not_expected();
	r.r_seq(r.r_u16(), m_links, way_link_io());
	r.debug_find_chunk();

	r.r_chunk<uint32_t>(WAYOBJECT_CHUNK_TYPE, m_type);
}

struct write_point_le { void operator()(const way_point_le& point, xr_writer& w) const {
	w.w_fvector3(point.position);
	w.w_u32(point.flags);
	w.w_u16(point.id);
	w.w_sz(point.name);
}};

void xr_way_object::save(xr_writer& w) const
{
	xr_custom_object::save(w);
	w.w_chunk<uint16_t>(WAYOBJECT_CHUNK_VERSION, WAYOBJECT_VERSION);

	w.open_chunk(WAYOBJECT_CHUNK_POINTS);
	w.w_size_u16(m_points.size());
	w.w_seq(m_points, write_point_le());
	w.close_chunk();

	w.open_chunk(WAYOBJECT_CHUNK_LINKS);
	w.w_size_u16(m_links.size());
	w.w_seq(m_links, way_link_io());
	w.close_chunk();

	w.w_chunk<uint32_t>(WAYOBJECT_CHUNK_TYPE, m_type);
}

struct write_point_ini { void operator()(const way_point_le& point, xr_ini_writer* w, uint32_t id) const {
	char buffer[128];
	char* buf = &buffer[0];
	int n = xr_snprintf(buf, sizeof(buffer), "wp_%d_flags", id);
	if (n > 0)
		w->write(buf, point.flags);

	n = xr_snprintf(buf, sizeof(buffer), "wp_%d_name", id);
	if (n > 0)
		w->write(buf, point.name, false);

	n = xr_snprintf(buf, sizeof(buffer), "wp_%d_pos", id);
	if (n > 0)
		w->write(buf, point.position);

	n = sprintf_s(buf, sizeof(buffer), "wp_%d_selected", id);
	if (n > 0)
		w->write(buf, "off", false);
}};

void xr_way_object::save_v12(xr_ini_writer* w) const
{
	xr_custom_object::save_v12(w);

	// write links
	char buffer[128];
	char* buf = &buffer[0];
	typedef std::map<uint16_t, uint8_t> dict;
	typedef std::pair<uint16_t, uint8_t> dict_pair;
	dict indices;
	fvector2 link_vector;
	link_vector.y = 1.0;

	way_link_vec_cit it = m_links.begin(), end = m_links.end();
	for (uint8_t id = 0; it != end; ++it) {
		way_link* link = (way_link *)&(*it);

		dict::iterator pair = indices.find(link->from);
		if (pair != indices.end())
			id = ++pair->second;
		else
			indices.insert(dict_pair(link->from, id = 0));

		int n = xr_snprintf(buf, sizeof(buffer), "link_wp_%d_%d", link->from, id);
		if (n > 0)
		{
			link_vector.x = link->to;
			link_vector.y = link->weight;
			w->write(buf, link_vector);
		}
	}

	w->write("type", this->m_type);
	w->write("version", WAYOBJECT_VERSION);

	// write waypoints

	w->w_ini_seq(this->m_points, write_point_ini());
	w->write("wp_count", this->m_points.size());
}

////////////////////////////////////////////////////////////////////////////////

xr_scene_ways::xr_scene_ways(xr_scene& scene):
	xr_scene_objects(scene, "way.part", SCENE_CHUNK_WAYS) {}

xr_scene_ways::~xr_scene_ways() {}

void xr_scene_ways::load(xr_reader& r)
{
	uint16_t version = 0;
	r.r_chunk<uint16_t>(TOOLS_CHUNK_VERSION, version);
	xr_assert(version == 0);
	xr_scene_objects::load(r);
}

void xr_scene_ways::save(xr_writer& w) const
{
	xr_scene_objects::save(w);
	w.w_chunk<uint16_t>(TOOLS_CHUNK_VERSION, 0);
}

void xr_scene_ways::save_v12(xr_ini_writer* w) const
{
	w->open_section("main");
	w->write("objects_count", this->objects().size());
	w->write("version", 0);
	w->close_section();

	scene().write_revision(w);

	xr_scene_objects::save_v12(w);
}
