#include "xr_scene.h"
#include "xr_scene_visuals.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_visual_object::xr_visual_object(xr_scene& scene):
	xr_custom_object(scene, TOOLS_CLASS_SCENE_OBJECT),
	m_file_version(0), m_flags(0) {}

xr_visual_object::~xr_visual_object() {}

void xr_visual_object::load(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk<uint16_t>(SCENEOBJ_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == SCENEOBJ_VERSION_16 || version == SCENEOBJ_VERSION);

	if (version == SCENEOBJ_VERSION_16) {
		if (!r.find_chunk(SCENEOBJ_CHUNK_PLACEMENT))
			xr_not_expected();
		r.r_fvector3(co_position());
		r.r_fvector3(co_rotation());
		r.r_fvector3(co_scale());
		r.debug_find_chunk();
	}

	xr_custom_object::load(r);

	if (!r.find_chunk(SCENEOBJ_CHUNK_REFERENCE))
		xr_not_expected();
	m_file_version = r.r_u32();
	r.r_u32();
	r.r_sz(m_reference);
	r.debug_find_chunk();

	r.r_chunk<uint32_t>(SCENEOBJ_CHUNK_FLAGS, m_flags);
}

void xr_visual_object::save(xr_writer& w) const
{
	xr_custom_object::save(w);
	w.w_chunk<uint16_t>(SCENEOBJ_CHUNK_VERSION, SCENEOBJ_VERSION);
	w.open_chunk(SCENEOBJ_CHUNK_REFERENCE);
	w.w_u32(m_file_version);
	w.w_u32(0);
	w.w_sz(m_reference);
	w.close_chunk();
	w.w_chunk<uint32_t>(SCENEOBJ_CHUNK_FLAGS, m_flags);
}

void xr_visual_object::save_v12(xr_ini_writer* w) const
{
	xr_custom_object::save_v12(w);

	w->write("reference_name", m_reference, false);
	w->write("version", SCENEOBJ_VERSION_18);
}

////////////////////////////////////////////////////////////////////////////////

xr_scene_visuals::xr_scene_visuals(xr_scene& scene):
	xr_scene_objects(scene, "scene_object.part", SCENE_CHUNK_SCENE_OBJECTS),
	m_flags(0)
{
	m_min_scale.set(1.f, 1.f, 1.f);
	m_max_scale.set(1.f, 1.f, 1.f);
	m_min_rotate.set();
	m_max_rotate.set();
}

xr_scene_visuals::~xr_scene_visuals() {}

void xr_scene_visuals::load(xr_reader& r)
{
	uint16_t version = 0;
	r.r_chunk<uint16_t>(TOOLS_CHUNK_VERSION, version);
	xr_assert(version == 0);
	xr_scene_objects::load(r);
	r.r_chunk<uint32_t>(SCENEOBJS_CHUNK_COMMON_FLAGS, m_flags);
	if (r.find_chunk(SCENEOBJS_CHUNK_COMMON_PARAMS)) {
		r.r_fvector3(m_min_scale);
		r.r_fvector3(m_max_scale);
		r.r_fvector3(m_min_rotate);
		r.r_fvector3(m_max_rotate);
		r.r_seq(r.r_u32(), m_snap_objects, xr_reader::f_r_sz());
	}
}

void xr_scene_visuals::save(xr_writer& w) const
{
	xr_scene_objects::save(w);

	w.w_chunk<uint16_t>(TOOLS_CHUNK_VERSION, 0);
	w.w_chunk<uint32_t>(SCENEOBJS_CHUNK_COMMON_FLAGS, m_flags);

	w.open_chunk(SCENEOBJS_CHUNK_COMMON_PARAMS);
	w.w_fvector3(m_min_scale);
	w.w_fvector3(m_max_scale);
	w.w_fvector3(m_min_rotate);
	w.w_fvector3(m_max_rotate);
	w.w_size_u32(m_snap_objects.size());
	w.w_seq(m_snap_objects, xr_writer::f_w_sz());
	w.close_chunk();
}

void xr_scene_visuals::save_v12(xr_ini_writer* w) const
{
	w->open_section("appendrandom");
	w->write("AppendRandomMaxRotation", m_max_rotate);
	w->write("AppendRandomMaxScale", m_max_scale);
	w->write("AppendRandomMinRotation", m_min_rotate);
	w->write("AppendRandomMinScale", m_min_scale);
	w->write("AppendRandomObjects_size", m_snap_objects.size());

	w->close_section();

	w->open_section("main");
	w->write("flags", this->m_flags);
	w->write("version", 0);
	w->write("objects_count", this->objects().size());
	w->close_section();

	scene().write_revision(w);

	xr_scene_objects::save_v12(w);
}
