#include "xr_scene_objects.h"
#include "xr_scene.h"
#include "xr_obj_motion.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_utils.h"

using namespace xray_re;

xr_custom_object::xr_custom_object(xr_scene& scene, tools_class_id class_id):
	m_scene(scene), m_class_id(class_id),
	m_flags(COF_VISIBLE), m_motion(0), m_time(0)
{
	m_position.set();
	m_rotation.set();
	m_scale.set(1.f, 1.f, 1.f);
}

xr_custom_object::~xr_custom_object()
{
	delete m_motion;
}

void xr_custom_object::load(xr_reader& r)
{
	if (r.r_chunk(CUSTOMOBJECT_CHUNK_FLAGS, m_flags)) {
		if (!r.find_chunk(CUSTOMOBJECT_CHUNK_NAME))
			xr_not_expected();
		r.r_sz(m_name);
		r.debug_find_chunk();
	} else if (r.find_chunk(CUSTOMOBJECT_CHUNK_PARAMS)) {
		m_flags = 0;
		uint16_t flag = r.r_u16();
		if (flag)
			m_flags |= COF_SELECTED;
		flag = r.r_u16();
		if (flag)
			m_flags |= COF_VISIBLE;
		r.r_sz(m_name);
		r.debug_find_chunk();

		if (r.r_chunk(CUSTOMOBJECT_CHUNK_f902, flag) && flag)
			m_flags |= COF_LOCKED;
	} else {
		xr_not_expected();
	}
	if (r.find_chunk(CUSTOMOBJECT_CHUNK_XFORM)) {
		r.r_fvector3(m_position);
		r.r_fvector3(m_rotation);
		r.r_fvector3(m_scale);
		r.debug_find_chunk();
	}
	if (r.find_chunk(CUSTOMOBJECT_CHUNK_MOTION)) {
		m_motion = new xr_obj_motion();
		m_motion->load(r);
		r.debug_find_chunk();
	}
	if (r.r_chunk(CUSTOMOBJECT_CHUNK_TIME, m_time)) {
		xr_assert(m_motion);
	}
}

void xr_custom_object::save_v12(xr_ini_writer* w) const
{
	w->write("clsid", m_class_id);
	w->write("co_flags", m_flags);
	w->write("flags", 0);
	w->write("name", m_name, false);
	w->write("position", m_position);
	w->write("rotation", m_rotation);
	w->write("scale", m_scale);
}

void xr_custom_object::save(xr_writer& w) const
{
	w.w_chunk(CUSTOMOBJECT_CHUNK_FLAGS, m_flags);
	w.w_chunk(CUSTOMOBJECT_CHUNK_NAME, m_name);

	w.open_chunk(CUSTOMOBJECT_CHUNK_XFORM);
	w.w_fvector3(m_position);
	w.w_fvector3(m_rotation);
	w.w_fvector3(m_scale);
	w.close_chunk();

	if (m_flags & COF_MOTIONABLE) {
		xr_assert(m_motion);
		w.open_chunk(CUSTOMOBJECT_CHUNK_MOTION);
		m_motion->save(w);
		w.close_chunk();

		w.w_chunk<float>(CUSTOMOBJECT_CHUNK_TIME, m_time);
	}
}

////////////////////////////////////////////////////////////////////////////////

xr_scene_objects::xr_scene_objects(xr_scene& scene, const char* file_name, scene_chunk_id chunk_id):
	xr_scene_part(scene, file_name, chunk_id) {}

xr_scene_objects::~xr_scene_objects()
{
	delete_elements(m_objects);
}

void xr_scene_objects::load(xr_reader& r)
{
	revision().load(r);
	if (!r.find_chunk(TOOLS_CHUNK_COUNT))
		xr_not_expected();
	size_t num_objects = r.r_u32();
	r.debug_find_chunk();

	scene().load_objects(r, TOOLS_CHUNK_OBJECTS, m_objects);
	xr_assert(num_objects == m_objects.size());
}

void xr_scene_objects::save(xr_writer& w) const
{
	revision().save(w);
	w.open_chunk(TOOLS_CHUNK_COUNT);
	w.w_size_u32(m_objects.size());
	w.close_chunk();

	scene().save_objects(w, TOOLS_CHUNK_OBJECTS, m_objects);
}

void xr_scene_objects::save_v12(xr_ini_writer* w) const
{
	scene().save_objects(w, m_objects);
}
