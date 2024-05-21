#include "xr_scene.h"
#include "xr_scene_shapes.h"
#include "xr_string_utils.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_shape_object::xr_shape_object(xr_scene& scene):
	xr_custom_object(scene, TOOLS_CLASS_SHAPE) {}

xr_shape_object::xr_shape_object(xr_scene& scene, tools_class_id class_id):
	xr_custom_object(scene, class_id) {}

xr_shape_object::~xr_shape_object() {}

struct read_shape { void operator()(shape_def& s, xr_reader& r) const {
	s.type = r.r_u8();
	r.r(s.box);
}};

void xr_shape_object::load(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk<uint16_t>(SHAPE_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == SHAPE_VERSION);
	xr_custom_object::load(r);
	if (!r.find_chunk(SHAPE_CHUNK_SHAPES))
		xr_not_expected();
	r.r_seq(r.r_u32(), m_shapes, read_shape());
	r.debug_find_chunk();
}

struct write_shape { void operator()(const shape_def& s, xr_writer& w) const {
	w.w_u8(s.type);
	w.w(s.box);
}};

void xr_shape_object::save(xr_writer& w) const
{
	xr_custom_object::save(w);
	w.w_chunk<uint16_t>(SHAPE_CHUNK_VERSION, SHAPE_VERSION);
	w.open_chunk(SHAPE_CHUNK_SHAPES);
	w.w_size_u32(m_shapes.size());
	w.w_seq(m_shapes, write_shape());
	w.close_chunk();
}

struct write_shape_ini { void operator()(const shape_def& s, xr_ini_writer* w, uint32_t id) const {
	char buffer[128];
	char* buf = &buffer[0];
	int n = xr_snprintf(buf, sizeof(buffer), "shape_type_%d", id);

	if (n > 0)
		w->write(buf, s.type);

	if (s.type == SHAPE_SPHERE)
	{
		n = xr_snprintf(buf, sizeof(buffer), "shape_center_%d", id);
		if (n > 0)
			w->write(buf, s.sphere.p);

		n = xr_snprintf(buf, sizeof(buffer), "shape_radius_%d", id);
		if (n > 0)
			w->write(buf, s.sphere.r);
	}
	else if (s.type == SHAPE_BOX)
	{
		n = xr_snprintf(buf, sizeof(buffer), "shape_matrix_c_%d", id);
		if (n > 0)
			w->write(buf, s.box.c);

		n = xr_snprintf(buf, sizeof(buffer), "shape_matrix_i_%d", id);
		if (n > 0)
			w->write(buf, s.box.i);

		n = xr_snprintf(buf, sizeof(buffer), "shape_matrix_j_%d", id);
		if (n > 0)
			w->write(buf, s.box.j);

		n = xr_snprintf(buf, sizeof(buffer), "shape_matrix_k_%d", id);
		if (n > 0)
			w->write(buf, s.box.k);
	}
}};

void xr_shape_object::save_v12(xr_ini_writer* w) const
{
	xr_custom_object::save_v12(w);

	w->write("shape_type", 0);

	w->w_ini_seq(m_shapes, write_shape_ini());
	w->write("shapes_count", m_shapes.size());

	w->write("version", SHAPE_VERSION_V12);
}

////////////////////////////////////////////////////////////////////////////////

xr_scene_shapes::xr_scene_shapes(xr_scene& scene):
	xr_scene_objects(scene, "shape.part", SCENE_CHUNK_SHAPES),
	m_flags(0) {}

xr_scene_shapes::~xr_scene_shapes() {}

void xr_scene_shapes::load(xr_reader& r)
{
	uint16_t version = 0;
	r.r_chunk<uint16_t>(TOOLS_CHUNK_VERSION, version);
	xr_assert(version == 0);
	xr_scene_objects::load(r);
//	r.r_chunk<uint32_t>(SHAPES_CHUNK_COMMON_FLAGS, m_flags);
}

void xr_scene_shapes::save(xr_writer& w) const
{
	xr_scene_objects::save(w);
	w.w_chunk<uint16_t>(TOOLS_CHUNK_VERSION, 0);
//	w.w_chunk<uint32_t>(SHAPES_CHUNK_COMMON_FLAGS, m_flags);
}

void xr_scene_shapes::save_v12(xr_ini_writer* w) const
{
	w->open_section("main");
	w->write("objects_count", this->objects().size());
	w->write("version", 0);
	w->close_section();

	scene().write_revision(w);

	xr_scene_objects::save_v12(w);
}
