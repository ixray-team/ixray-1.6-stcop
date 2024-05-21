#include "xr_scene.h"
#include "xr_scene_glows.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_glow_object::xr_glow_object(xr_scene& scene):
	xr_custom_object(scene, TOOLS_CLASS_GLOW),
	m_shader("effects\\glow"), m_radius(0.5f), m_flags(0) {}

xr_glow_object::~xr_glow_object() {}

void xr_glow_object::load(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk<uint16_t>(GLOW_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == GLOW_VERSION_17 || version == GLOW_VERSION);

	xr_custom_object::load(r);

	if (r.find_chunk(GLOW_CHUNK_SHADER)) {
		r.r_sz(m_shader);
		r.debug_find_chunk();
	}

	if (!r.find_chunk(GLOW_CHUNK_TEXTURE))
		xr_not_expected();
	r.r_sz(m_texture);
	r.debug_find_chunk();

	if (!r.find_chunk(GLOW_CHUNK_PARAMS))
		xr_not_expected();
	m_radius = r.r_float();
	if (version == GLOW_VERSION_17)
		r.r_fvector3(co_position());
	r.debug_find_chunk();

	r.r_chunk<uint16_t>(GLOW_CHUNK_FLAGS, m_flags);
}

void xr_glow_object::save_v12(xr_ini_writer* w) const
{
	xr_custom_object::save_v12(w);

	w->write("version", GLOW_VERSION);
	w->write("radius", m_radius);
	w->write("shader_name", m_shader, false);
	w->write("texture_name", m_texture, false);

	w->write("flags", m_flags);
}

void xr_glow_object::save(xr_writer& w) const
{
	xr_custom_object::save(w);

	w.w_chunk<uint16_t>(GLOW_CHUNK_VERSION, GLOW_VERSION);

	w.open_chunk(GLOW_CHUNK_PARAMS);
	w.w_float(m_radius);
	w.close_chunk();

	w.w_chunk(GLOW_CHUNK_SHADER, m_shader);
	w.w_chunk(GLOW_CHUNK_TEXTURE, m_texture);

	w.w_chunk<uint16_t>(GLOW_CHUNK_FLAGS, m_flags);
}

////////////////////////////////////////////////////////////////////////////////

xr_scene_glows::xr_scene_glows(xr_scene& scene):
	xr_scene_objects(scene, "glow.part", SCENE_CHUNK_GLOWS),
	m_flags(0) {}

xr_scene_glows::~xr_scene_glows() {}

void xr_scene_glows::load(xr_reader& r)
{
	uint16_t version;
	r.r_chunk<uint16_t>(TOOLS_CHUNK_VERSION, version);
	xr_assert(version == 0);
	xr_scene_objects::load(r);
	r.r_chunk<uint32_t>(GLOWS_CHUNK_COMMON_FLAGS, m_flags);
}

void xr_scene_glows::save(xr_writer& w) const
{
	xr_scene_objects::save(w);
	w.w_chunk<uint16_t>(TOOLS_CHUNK_VERSION, 0);
	w.w_chunk<uint32_t>(GLOWS_CHUNK_COMMON_FLAGS, m_flags);
}

void xr_scene_glows::save_v12(xr_ini_writer* w) const
{
	w->open_section("main");
	w->write("flags", this->m_flags);
	w->write("glow_tool_version", 0);
	w->write("objects_count", this->objects().size());
	w->close_section();

	scene().write_revision(w);

	xr_scene_objects::save_v12(w);
}
