#include "xr_scene.h"
#include "xr_scene_lights.h"
#include "xr_d3d_light.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_math.h"

using namespace xray_re;

xr_light_object::xr_light_object(xr_scene& scene):
	xr_custom_object(scene, TOOLS_CLASS_LIGHT),
	m_type(D3D_LIGHT_POINT),
	m_brightness(1.f),
	m_range(8.f),
	m_attenuation_constant(1.f),
	m_attenuation_linear(0),
	m_attenuation_quadratic(0),
	m_cone_angle(float(M_PI/8)),
	m_unknown(0),
	m_use_in_d3d(1),
	m_flags(LIGHT_FLAG_LIGHTMAP),
	m_control(0)
{
	m_color.set(1.f, 1.f, 1.f, 0);
	// FIXME
	m_shape.type = FUZZY_SPHERE;
	m_shape.fixme.set(1.f, 0.1f, 0.1f, 0.1f);
}

xr_light_object::~xr_light_object() {}

void xr_light_object::load(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk<uint16_t>(LIGHT_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == LIGHT_VERSION_16 || version == LIGHT_VERSION);
	xr_custom_object::load(r);
	if (r.find_chunk(LIGHT_CHUNK_PARAMS)) {
		m_type = r.r_u32();
		r.r<fcolor>(m_color);
		m_brightness = r.r_float();
		m_range = r.r_float();
		m_attenuation_constant = r.r_float();
		m_attenuation_linear = r.r_float();
		m_attenuation_quadratic = r.r_float();
		m_cone_angle = r.r_float();
		m_unknown = r.r_float();
		r.debug_find_chunk();
	} else if (r.find_chunk(LIGHT_CHUNK_D3D_PARAMS)) {
		d3d_light light;
		light.load(r);
		m_type = light.type;
		m_color = light.diffuse;
		co_position().set(light.position);
		m_range = light.range;
		m_attenuation_constant = light.attenuation0;
		m_attenuation_linear = light.attenuation1;
		m_attenuation_quadratic = light.attenuation2;
		m_cone_angle = light.phi;
		r.debug_find_chunk();
		if (!r.r_chunk<float>(LIGHT_CHUNK_BRIGHTNESS, m_brightness))
			xr_not_expected();
	} else {
		xr_not_expected();
	}
	if (!r.r_chunk<uint32_t>(LIGHT_CHUNK_USE_IN_D3D, m_use_in_d3d))
		xr_not_expected();
	if (version == LIGHT_VERSION_16 &&
			!r.r_chunk<fvector3>(LIGHT_CHUNK_ROTATION, co_rotation())) {
		co_rotation().set();
	}
	r.r_chunk<uint32_t>(LIGHT_CHUNK_FLAGS, m_flags);
	r.r_chunk<uint32_t>(LIGHT_CHUNK_CONTROL, m_control);
	xr_assert(m_type != D3D_LIGHT_DIRECTIONAL);	// FIXME
	if (r.find_chunk(LIGHT_CHUNK_ANIMATION)) {
		r.r_sz(m_animation);
		r.debug_find_chunk();
	}
	if (r.find_chunk(LIGHT_CHUNK_TEXTURE)) {
		r.r_sz(m_texture);
		r.debug_find_chunk();
	}
	if (r.find_chunk(LIGHT_CHUNK_FUZZY_DATA)) {
		m_shape.type = r.r_u8();
		r.r<fvector4>(m_shape.fixme);
		r.r_seq(r.r_u16(), m_vertices);
		r.debug_find_chunk();
		m_flags |= LIGHT_FLAG_FUZZY;
	} else {
		m_flags &= ~LIGHT_FLAG_FUZZY;
	}
}

void xr_light_object::save_v12(xr_ini_writer* w) const
{
	xr_custom_object::save_v12(w);

	w->write("anim_ref_name", this->m_animation, false);

	w->write("attenuation0", this->m_attenuation_constant);
	w->write("attenuation1", this->m_attenuation_linear);
	w->write("attenuation2", this->m_attenuation_quadratic);
	w->write("brightness", this->m_brightness);
	w->write("color", this->m_color);
	w->write("cone", this->m_cone_angle);

	w->write("fallof_texture", this->m_texture, false);

	w->write("light_control", this->m_control);
	w->write("light_flags", this->m_flags);
	w->write("range", this->m_range);
	w->write("type", this->m_type);
	w->write("use_in_d3d", this->m_use_in_d3d ? "on" : "off");
	w->write("version", LIGHT_VERSION);
	w->write("virtual_size", 0.0f);
}

void xr_light_object::save(xr_writer& w) const
{
	xr_custom_object::save(w);
	w.w_chunk<uint16_t>(LIGHT_CHUNK_VERSION, LIGHT_VERSION);

	w.open_chunk(LIGHT_CHUNK_PARAMS);
	w.w_u32(m_type);
	w.w<fcolor>(m_color);
	w.w_float(m_brightness);
	w.w_float(m_range);
	w.w_float(m_attenuation_constant);
	w.w_float(m_attenuation_linear);
	w.w_float(m_attenuation_quadratic);
	w.w_float(m_cone_angle);
	w.w_float(m_unknown);
	w.close_chunk();

	w.w_chunk<uint32_t>(LIGHT_CHUNK_USE_IN_D3D, m_use_in_d3d);
	w.w_chunk<uint32_t>(LIGHT_CHUNK_FLAGS, m_flags);
	w.w_chunk<uint32_t>(LIGHT_CHUNK_CONTROL, m_control);

	if (!m_animation.empty())
		w.w_chunk(LIGHT_CHUNK_ANIMATION, m_animation);
	if (!m_texture.empty())
		w.w_chunk(LIGHT_CHUNK_TEXTURE, m_texture);

	if (m_flags & LIGHT_FLAG_FUZZY) {
		w.open_chunk(LIGHT_CHUNK_FUZZY_DATA);
		w.w_u8(m_shape.type);
		w.w<fvector4>(m_shape.fixme);
		w.w_size_u16(m_vertices.size());
		w.w_seq(m_vertices);
		w.close_chunk();
	}
}

////////////////////////////////////////////////////////////////////////////////

xr_token::xr_token(): id(0) {}

xr_token::xr_token(const char* _name, uint32_t _id):
	name(_name), id(_id) {}

xr_scene_lights::xr_scene_lights(xr_scene& scene):
	xr_scene_objects(scene, "light.part", SCENE_CHUNK_LIGHTS),
	m_flags(0)
{
	m_sun.set(deg2rad(-25.f), deg2rad(292.f));
	m_controls.push_back(xr_token("$static", 0));
	m_controls.push_back(xr_token("$hemi", 1));
	m_controls.push_back(xr_token("$sun", 2));
}

xr_scene_lights::~xr_scene_lights() {}

void xr_scene_lights::load(xr_reader& r)
{
	uint16_t version;
	r.r_chunk<uint16_t>(TOOLS_CHUNK_VERSION, version);
	xr_assert(version == 0);
	xr_scene_objects::load(r);
	r.r_chunk<uint32_t>(LIGHTS_CHUNK_COMMON_FLAGS, m_flags);
	if (r.find_chunk(LIGHTS_CHUNK_COMMON_SUN)) {
		r.r_u8();
		r.r_fvector2(m_sun);
		r.debug_find_chunk();
	}

	uint32_t count = 0;
	r.r_chunk<uint32_t>(LIGHTS_CHUNK_COMMON_COUNT, count);
	if (r.find_chunk(LIGHTS_CHUNK_COMMON_CONTROLS)) {
		xr_assert(count == m_controls.size());
		for (xr_token_vec_it it = m_controls.begin(), end = m_controls.end();
				it != end; ++it) {
			const char* name = r.skip_sz();
			xr_assert(name == it->name);
			it->id = r.r_u32();
		}
		r.debug_find_chunk();
	}
}

struct write_token { void operator()(const xr_token& t, xr_writer& w) const {
	w.w_sz(t.name);
	w.w_u32(t.id);
}};

void xr_scene_lights::save(xr_writer& w) const
{
	xr_scene_objects::save(w);
	w.w_chunk<uint16_t>(TOOLS_CHUNK_VERSION, 0);
	w.w_chunk<uint32_t>(LIGHTS_CHUNK_COMMON_FLAGS, m_flags);

	w.open_chunk(LIGHTS_CHUNK_COMMON_SUN);
	w.w_u8(0);
	w.w_fvector2(m_sun);
	w.close_chunk();

	w.open_chunk(LIGHTS_CHUNK_COMMON_COUNT);
	w.w_size_u32(m_controls.size());
	w.close_chunk();

	w.open_chunk(LIGHTS_CHUNK_COMMON_CONTROLS);
	w.w_seq(m_controls, write_token());
	w.close_chunk();
}

struct write_ini_token { void operator()(const xr_token& t, xr_ini_writer* w, uint32_t dummy) const {
	w->write(t.name.c_str(), t.id);
}};

void xr_scene_lights::save_v12(xr_ini_writer* w) const
{
	w->open_section("lcontrols");
	w->w_ini_seq(m_controls, write_ini_token());
	w->close_section();

	w->open_section("main");
	w->write("flags", this->m_flags);
	w->write("lcontrol_last_idx", this->m_controls.size());
	w->write("objects_count", this->objects().size());
	w->write("sun_shadow_dir", this->m_sun);
	w->write("version", 0);
	w->close_section();

	scene().write_revision(w);

	xr_scene_objects::save_v12(w);
}
