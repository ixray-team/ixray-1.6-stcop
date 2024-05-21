#include "xr_scene.h"
#include "xr_scene_particles.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_particle_object::xr_particle_object(xr_scene& scene):
	xr_custom_object(scene, TOOLS_CLASS_PARTICLE) {}

xr_particle_object::~xr_particle_object() {}

void xr_particle_object::load(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk<uint16_t>(CPSOBJECT_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == CPSOBJECT_VERSION);
	xr_custom_object::load(r);
	if (!r.find_chunk(CPSOBJECT_CHUNK_REFERENCE))
		xr_not_expected();
	r.r_sz(m_reference);
	r.debug_find_chunk();
}

void xr_particle_object::save_v12(xr_ini_writer* w) const
{
	xr_custom_object::save_v12(w);

	w->write("game_type", 65535);
	w->write("ref_name", m_reference, false);
	w->write("version", CPSOBJECT_VERSION_V12);
}

void xr_particle_object::save(xr_writer& w) const
{
	xr_custom_object::save(w);
	w.w_chunk<uint16_t>(CPSOBJECT_CHUNK_VERSION, CPSOBJECT_VERSION);
	w.w_chunk(CPSOBJECT_CHUNK_REFERENCE, m_reference);
}

////////////////////////////////////////////////////////////////////////////////

xr_scene_particles::xr_scene_particles(xr_scene& scene):
	xr_scene_objects(scene, "ps.part", SCENE_CHUNK_PARTICLES) {}

xr_scene_particles::~xr_scene_particles() {}

void xr_scene_particles::load(xr_reader& r)
{
	uint16_t version = 0;
	r.r_chunk<uint16_t>(TOOLS_CHUNK_VERSION, version);
	xr_assert(version == 0);
	xr_scene_objects::load(r);
}

void xr_scene_particles::save(xr_writer& w) const
{
	xr_scene_objects::save(w);
	w.w_chunk<uint16_t>(TOOLS_CHUNK_VERSION, 0);
}

void xr_scene_particles::save_v12(xr_ini_writer* w) const
{
	w->open_section("main");
	w->write("objects_count", this->objects().size());
	w->write("version", 0);
	w->close_section();

	scene().write_revision(w);

	xr_scene_objects::save_v12(w);
}
