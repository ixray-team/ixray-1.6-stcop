#include "xr_scene.h"
#include "xr_scene_sectors.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_string_utils.h"

using namespace xray_re;

xr_sector_object::xr_sector_object(xr_scene& scene):
	xr_custom_object(scene, TOOLS_CLASS_SECTOR),
	m_private(0)
{
	m_color.set(1.f, 1.f, 1.f, 0);
}

xr_sector_object::~xr_sector_object() {}

struct read_item { void operator()(sector_item& item, xr_reader& r) const {
	if (!r.find_chunk(SECTOR_CHUNK_ONE_ITEM))
		xr_not_expected();
	r.r_sz(item.object);
	r.r_sz(item.mesh);
	r.debug_find_chunk();
}};

void xr_sector_object::load(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk<uint16_t>(SECTOR_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == SECTOR_VERSION);
	xr_custom_object::load(r);
	if (!r.r_chunk<fcolor>(SECTOR_CHUNK_COLOR, m_color))
		xr_not_expected();
	if (!r.r_chunk<uint8_t>(SECTOR_CHUNK_PRIVATE, m_private))
		xr_not_expected();
	xr_reader* s = r.open_chunk(SECTOR_CHUNK_ITEMS);
	if (s) {
		s->r_chunks(m_items, read_item());
		r.close_chunk(s);
	}
}

struct write_item { void operator()(const sector_item& item, xr_writer& w) const {
	w.open_chunk(SECTOR_CHUNK_ONE_ITEM);
	w.w_sz(item.object);
	w.w_sz(item.mesh);
	w.close_chunk();
}};

void xr_sector_object::save(xr_writer& w) const
{
	xr_custom_object::save(w);
	w.w_chunk<uint16_t>(SECTOR_CHUNK_VERSION, SECTOR_VERSION);
	w.w_chunk<fcolor>(SECTOR_CHUNK_COLOR, m_color);
	w.w_chunk<uint8_t>(SECTOR_CHUNK_PRIVATE, m_private);
	w.open_chunk(SECTOR_CHUNK_ITEMS);
	w.w_chunks(m_items, write_item());
	w.close_chunk();
}

struct write_ini_item { void operator()(const sector_item& item, xr_ini_writer* w, uint32_t id) const {
	char buf[128];
	int n = xr_snprintf(buf, sizeof(buf), "item_mesh_name_%04d", id);
	if (n > 0)
		w->write(buf, item.mesh, false);

	n = xr_snprintf(buf, sizeof(buf), "item_object_name_%04d", id);
	if (n > 0)
		w->write(buf, item.object, false);
}};

void xr_sector_object::save_v12(xr_ini_writer* w) const
{
	xr_custom_object::save_v12(w);

	w->write("change_map_to_idx", 255);
	w->write("sector_color", this->m_color);
	w->write("default", this->m_private ? "on" : "off");

	w->w_ini_seq(this->m_items, write_ini_item());
	w->write("items_count", this->m_items.size());
	w->write("version", SECTOR_VERSION_V12);
}

////////////////////////////////////////////////////////////////////////////////

xr_scene_sectors::xr_scene_sectors(xr_scene& scene):
	xr_scene_objects(scene, "sector.part", SCENE_CHUNK_SECTORS), m_flags(0) {}

xr_scene_sectors::~xr_scene_sectors() {}

void xr_scene_sectors::load(xr_reader& r)
{
	uint16_t version = 0;
	r.r_chunk<uint16_t>(TOOLS_CHUNK_VERSION, version);
	xr_assert(version == 0);
	xr_scene_objects::load(r);
	r.r_chunk<uint32_t>(SECTORS_CHUNK_COMMON_FLAGS, m_flags);
}

void xr_scene_sectors::save(xr_writer& w) const
{
	xr_scene_objects::save(w);
	w.w_chunk<uint16_t>(TOOLS_CHUNK_VERSION, 0);
	w.w_chunk<uint32_t>(SECTORS_CHUNK_COMMON_FLAGS, m_flags);
}

void xr_scene_sectors::save_v12(xr_ini_writer* w) const
{
	w->open_section("main");
	w->write("flags", this->m_flags);
	w->write("objects_count", this->objects().size());
	w->write("version", 0);
	w->close_section();

	scene().write_revision(w);

	xr_scene_objects::save_v12(w);
}
