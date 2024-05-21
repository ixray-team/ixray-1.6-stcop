#include "xr_scene.h"
#include "xr_scene_ai_map.h"
#include "xr_scene_details.h"
#include "xr_scene_glows.h"
#include "xr_scene_groups.h"
#include "xr_scene_lights.h"
#include "xr_scene_particles.h"
#include "xr_scene_portals.h"
#include "xr_scene_sectors.h"
#include "xr_scene_shapes.h"
#include "xr_scene_sound_envs.h"
#include "xr_scene_sound_srcs.h"
#include "xr_scene_spawns.h"
#include "xr_scene_visuals.h"
#include "xr_scene_wallmarks.h"
#include "xr_scene_ways.h"
#include "xr_file_system.h"
#include "xr_motion.h"
#include "xr_utils.h"

using namespace xray_re;

void b_params::init()
{
	sm_angle = 75.f;
	weld_distance = 0.005f;
	lm_rms_zero = 4;
	lm_rms = 4;
	lm_jitter_samples = 9;
	lm_pixels_per_meter = 10.0;

	convert_progressive = 0;
	pm_uv = 0;
	pm_pos = 0;
	pm_curv = 0;
	pm_border_h_angle = 0;
	pm_border_h_distance = 0;
	pm_heuristic = 0;
}

void b_params::set_debug()
{
	lm_pixels_per_meter = 0.1f;
	lm_jitter_samples = 1;
	convert_progressive = XRLC_QUALITY_DRAFT;
}

void b_params::set_release()
{
	lm_pixels_per_meter = 10.f;
	lm_jitter_samples = 9;
	convert_progressive = XRLC_QUALITY_HIGH;
}

void b_params::save_v12(xr_ini_writer *w)
{
	w->open_section("build_params");
	w->write("light_jitter_samples", lm_jitter_samples);
	w->write("light_pixel_per_meter", lm_pixels_per_meter);
	w->write("light_quality", 2);			// TEMP
	w->write("light_quality_reserved", 0);	// TEMP
	w->write("light_rms", lm_rms);
	w->write("light_rms_zero", lm_rms_zero);
	w->write("reserved_0", pm_uv);
	w->write("reserved_1", pm_pos);
	w->write("reserved_2", pm_curv);
	w->write("reserved_3", pm_border_h_angle);
	w->write("reserved_4", pm_border_h_distance);
	w->write("reserved_5", pm_heuristic);
	w->write("smooth_angle", sm_angle);
	w->write("weld_distance", weld_distance);
	w->close_section();
}

////////////////////////////////////////////////////////////////////////////////

xr_scene::xr_scene():
	m_name("level"), m_name_prefix("level_prefix")
{
	m_parts.push_back(new xr_scene_ai_map(*this));
	m_parts.push_back(new xr_scene_details(*this));
	m_parts.push_back(new xr_scene_glows(*this));
	m_parts.push_back(new xr_scene_groups(*this));
	m_parts.push_back(new xr_scene_lights(*this));
	m_parts.push_back(new xr_scene_visuals(*this));
	m_parts.push_back(new xr_scene_particles(*this));
	m_parts.push_back(new xr_scene_portals(*this));
	m_parts.push_back(new xr_scene_sectors(*this));
	m_parts.push_back(new xr_scene_shapes(*this));
	m_parts.push_back(new xr_scene_sound_envs(*this));
	m_parts.push_back(new xr_scene_sound_srcs(*this));
	m_parts.push_back(new xr_scene_spawns(*this));
	m_parts.push_back(new xr_scene_wallmarks(*this));
	m_parts.push_back(new xr_scene_ways(*this));
	m_camera_pos.set();
	m_camera_orient.set();
	m_bparams.init();
	set_quality(XRLC_QUALITY_HIGH);
	m_guid.reset();
}

xr_scene::~xr_scene()
{
	delete_elements(m_parts);
	delete_elements(m_objects);
}

void xr_scene::set_quality(unsigned xrlc_quality)
{
	switch (xrlc_quality) {
	case XRLC_QUALITY_DRAFT:
		m_hemi_quality = 0;
		m_sun_quality = 0;
		m_bparams.set_debug();
		break;
	case XRLC_QUALITY_HIGH:
	case XRLC_QUALITY_CUSTOM:
	default:
		m_hemi_quality = 3;
		m_sun_quality = 3;
		m_bparams.set_release();
		break;
	}
}

xr_scene_part* xr_scene::part(scene_chunk_id chunk_id)
{
	for (xr_scene_part_vec_it it = m_parts.begin(), end = m_parts.end(); it != end; ++it) {
		if ((*it)->chunk_id() == chunk_id)
			return *it;
	}
	return 0;
}

xr_scene_ai_map* xr_scene::ai_map() { return dynamic_cast<xr_scene_ai_map*>(part(SCENE_CHUNK_AI_MAP)); }
xr_scene_details* xr_scene::details() { return dynamic_cast<xr_scene_details*>(part(SCENE_CHUNK_DETAIL_OBJECTS)); }
xr_scene_glows* xr_scene::glows() { return dynamic_cast<xr_scene_glows*>(part(SCENE_CHUNK_GLOWS)); }
xr_scene_groups* xr_scene::groups() { return dynamic_cast<xr_scene_groups*>(part(SCENE_CHUNK_GROUPS)); }
xr_scene_lights* xr_scene::lights() { return dynamic_cast<xr_scene_lights*>(part(SCENE_CHUNK_LIGHTS)); }
xr_scene_particles* xr_scene::particles() { return dynamic_cast<xr_scene_particles*>(part(SCENE_CHUNK_PARTICLES)); }
xr_scene_portals* xr_scene::portals() { return dynamic_cast<xr_scene_portals*>(part(SCENE_CHUNK_PORTALS)); }
xr_scene_sectors* xr_scene::sectors() { return dynamic_cast<xr_scene_sectors*>(part(SCENE_CHUNK_SECTORS)); }
xr_scene_shapes* xr_scene::shapes() { return dynamic_cast<xr_scene_shapes*>(part(SCENE_CHUNK_SHAPES)); }
xr_scene_sound_envs* xr_scene::sound_envs() { return dynamic_cast<xr_scene_sound_envs*>(part(SCENE_CHUNK_SOUND_ENVS)); }
xr_scene_sound_srcs* xr_scene::sound_srcs() { return dynamic_cast<xr_scene_sound_srcs*>(part(SCENE_CHUNK_SOUND_SRCS)); }
xr_scene_spawns* xr_scene::spawns() { return dynamic_cast<xr_scene_spawns*>(part(SCENE_CHUNK_SPAWNS)); }
xr_scene_visuals* xr_scene::visuals() { return dynamic_cast<xr_scene_visuals*>(part(SCENE_CHUNK_SCENE_OBJECTS)); }
xr_scene_wallmarks* xr_scene::wallmarks() { return dynamic_cast<xr_scene_wallmarks*>(part(SCENE_CHUNK_WALLMARKS)); }
xr_scene_ways* xr_scene::ways() { return dynamic_cast<xr_scene_ways*>(part(SCENE_CHUNK_WAYS)); }

xr_custom_object* xr_scene::create_object(tools_class_id class_id)
{
	switch (class_id) {
	case TOOLS_CLASS_GROUP:
		return new xr_group_object(*this);
	case TOOLS_CLASS_GLOW:
		return new xr_glow_object(*this);
	case TOOLS_CLASS_SCENE_OBJECT:
		return new xr_visual_object(*this);
	case TOOLS_CLASS_LIGHT:
		return new xr_light_object(*this);
	case TOOLS_CLASS_SHAPE:
		return new xr_shape_object(*this);
	case TOOLS_CLASS_SOUND_ENV:
		return new xr_sound_env_object(*this);
	case TOOLS_CLASS_SOUND_SRC:
		return new xr_sound_src_object(*this);
	case TOOLS_CLASS_SPAWN:
		return new xr_spawn_object(*this);
	case TOOLS_CLASS_WAY:
		return new xr_way_object(*this);
	case TOOLS_CLASS_SECTOR:
		return new xr_sector_object(*this);
	case TOOLS_CLASS_PORTAL:
		return new xr_portal_object(*this);
	case TOOLS_CLASS_PARTICLE:
		return new xr_particle_object(*this);
	default:
		return 0;
	}
}

struct read_object {
	explicit read_object(xr_scene& _scene): scene(_scene) {}
	void operator()(xr_custom_object*& object, xr_reader& r) {
		xr_assert(sizeof(tools_class_id) == 4);
		tools_class_id class_id;
		if (!r.r_chunk(TOOLS_CHUNK_OBJECT_CLASS, class_id))
			xr_not_expected();
		object = scene.create_object(class_id);
		xr_assert(object);
		xr_reader* s = r.open_chunk(TOOLS_CHUNK_OBJECT_DATA);
		xr_assert(s);
		object->load(*s);
		r.close_chunk(s);
	}
	xr_scene& scene;
};

void xr_scene::load_objects(xr_reader& r, uint32_t chunk_id, xr_custom_object_vec& objects)
{
	xr_reader* s = r.open_chunk(chunk_id);
	if (s) {
		s->r_chunks(objects, read_object(*this));
		r.close_chunk(s);
	}
}

struct write_object { void operator()(xr_custom_object* const& object, xr_writer& w) const {
	tools_class_id class_id = object->class_id();
	w.w_chunk(TOOLS_CHUNK_OBJECT_CLASS, class_id);
	w.open_chunk(TOOLS_CHUNK_OBJECT_DATA);
	object->save(w);
	w.close_chunk();
}};

void xr_scene::save_objects(xr_writer& w, uint32_t chunk_id, const xr_custom_object_vec& objects) const
{
	w.open_chunk(chunk_id);
	w.w_chunks(objects, write_object());
	w.close_chunk();
}

struct ini_write_object { void operator()(xr_custom_object* const& object, xr_ini_writer* w) const {
	object->save_v12(w);
}};

void xr_scene::save_objects(xr_ini_writer* w, const std::vector<xr_custom_object*>& objects, const char* prefix) const
{
	w->w_sections(objects, ini_write_object(), prefix);
}

void xr_scene::load_options(xr_reader& r)
{
	uint32_t version;
	if (!r.r_chunk<uint32_t>(SCENE_CHUNK_LO_VERSION, version))
		xr_not_expected();
	xr_assert(version == SCENE_LO_VERSION);

	if (!r.r_chunk(SCENE_CHUNK_LO_NAMES, m_name))
		xr_not_expected();
	r.r_chunk(SCENE_CHUNK_LO_NAME_PREFIX, m_name_prefix);
	r.r_chunk(SCENE_CHUNK_LO_BOP, m_custom_data);

	if (r.r_chunk(SCENE_CHUNK_LO_BPARAMS_VERSION, version) &&
			version == SCENE_LO_BPARAMS_VERSION) {
		r.r_chunk<b_params>(SCENE_CHUNK_LO_BPARAMS, m_bparams);
	}
	if (r.find_chunk(SCENE_CHUNK_LO_QUALITY)) {
		m_hemi_quality = r.r_u8();
		m_sun_quality = r.r_u8();
		r.debug_find_chunk();
	}
}

void xr_scene::save_options(xr_writer& w)
{
	w.w_chunk<uint32_t>(SCENE_CHUNK_LO_VERSION, SCENE_LO_VERSION);
	w.w_chunk(SCENE_CHUNK_LO_NAMES, m_name);
	w.w_chunk(SCENE_CHUNK_LO_NAME_PREFIX, m_name_prefix);
	w.w_chunk(SCENE_CHUNK_LO_BOP, m_custom_data);
	w.w_chunk<uint32_t>(SCENE_CHUNK_LO_BPARAMS_VERSION, SCENE_LO_BPARAMS_VERSION);
	w.w_chunk(SCENE_CHUNK_LO_BPARAMS, m_bparams);
	w.open_chunk(SCENE_CHUNK_LO_QUALITY);
	w.w_u8(m_hemi_quality);
	w.w_u8(m_sun_quality);
	w.close_chunk();
}

bool xr_scene::load(const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(PA_MAPS, std::string(name) + ".level");
	if (r == 0)
		return false;

	uint32_t version;
	if (!r->r_chunk<uint32_t>(SCENE_CHUNK_VERSION, version)) {
		msg("no scene version chunk");
		xr_not_expected();
	}
	xr_assert(version == SCENE_VERSION);
	if (version != SCENE_VERSION) {
		msg("unexpected scene version %" PRIu32, version);
		xr_not_expected();
	}

	xr_reader* s = r->open_chunk(SCENE_CHUNK_OPTIONS);
	if (s) {
		load_options(*s);
		r->close_chunk(s);
	} else {
		msg("no level options chunk");
		xr_not_expected();
	}

	if (r->find_chunk(SCENE_CHUNK_CAMERA)) {
		r->r_fvector3(m_camera_pos);
		r->r_fvector3(m_camera_orient);
		r->debug_find_chunk();
	} else {
		msg("no scene camera chunk");
		xr_not_expected();
	}

	if (!r->r_chunk<xr_guid>(TOOLS_CHUNK_GUID, m_guid)) {
		msg("no tools GUID chunk");
		xr_not_expected();
	}

	m_revision.load(*r);

	size_t num_objects = 0;
	if (r->find_chunk(SCENE_CHUNK_COUNT)) {
		num_objects = r->r_u32();
		r->debug_find_chunk();
	}

	load_objects(*r, SCENE_CHUNK_OBJECTS, m_objects);

	if (r->find_chunk(SCENE_CHUNK_SNAP_OBJECTS))
		r->r_seq(r->r_u32(), m_snap_objects, xr_reader::f_r_sz());

	fs.r_close(r);

	std::string parts_path(name);
	parts_path += '\\';
	for (xr_scene_part_vec_it it = m_parts.begin(), end = m_parts.end();
			it != end; ++it) {
		xr_scene_part* part = *it;

		r = fs.r_open(PA_MAPS, parts_path + part->file_name());
		if (r == 0)
			msg("can't load scene part %s", part->file_name());
		xr_assert(r);
		xr_guid guid;
		if (!r->r_chunk<xr_guid>(TOOLS_CHUNK_GUID, guid) || guid != m_guid)
			xr_not_expected();

		xr_reader* s = r->open_chunk(part->chunk_id());
		xr_assert(s);
		if (s) {
			part->load(*s);
			r->close_chunk(s);
		}
		fs.r_close(r);
	}

	return true;
}


bool xr_scene::save(const char* name)
{
	xr_memory_writer* w = new xr_memory_writer();

	w->w_chunk<uint32_t>(SCENE_CHUNK_VERSION, SCENE_VERSION);

	w->open_chunk(SCENE_CHUNK_OPTIONS);
	save_options(*w);
	w->close_chunk();

	w->w_chunk(TOOLS_CHUNK_GUID, m_guid);

	m_revision.save(*w);

	w->open_chunk(SCENE_CHUNK_CAMERA);
	w->w_fvector3(m_camera_pos);
	w->w_fvector3(m_camera_orient);
	w->close_chunk();

	w->open_chunk(SCENE_CHUNK_SNAP_OBJECTS);
	w->w_size_u32(m_snap_objects.size());
	w->w_seq(m_snap_objects, xr_writer::f_w_sz());
	w->close_chunk();

	if (!m_objects.empty())
		save_objects(*w, SCENE_CHUNK_OBJECTS, m_objects);

	bool status = w->save_to(PA_MAPS, std::string(name) + ".level");
	delete w;

	xr_file_system& fs = xr_file_system::instance();
	fs.create_folder(PA_MAPS, name);

	std::string parts_path(name);
	parts_path += '\\';
	for (xr_scene_part_vec_it it = m_parts.begin(), end = m_parts.end();
			it != end; ++it) {
		xr_scene_part* part = *it;

		w = new xr_memory_writer();
		w->w_chunk(TOOLS_CHUNK_GUID, m_guid);
		w->open_chunk(part->chunk_id());
		part->save(*w);
		w->close_chunk();

		if (!w->save_to(PA_MAPS, parts_path + part->file_name()))
			msg("can't save scene part %s", part->file_name());
		delete w;
	}

	return status;
}


bool xr_scene::save_v12(const char* name)
{
	xr_ini_writer *w = new xr_ini_writer();

	m_bparams.save_v12(w);

	w->open_section("camera");

	w->write("hpb", m_camera_orient);
	w->write("pos", m_camera_pos);

	w->close_section();

	write_guid(w);

	w->open_section("level_options");
	w->write("bop", "");
	w->write("game_type", 1);
	w->write("level_path", m_name, false);
	w->write("level_prefix", m_name_prefix, false);
	w->write("light_hemi_quality", m_hemi_quality);
	w->write("light_sun_quality", m_sun_quality);
	w->write("map_version", 1.0f);
	w->write("version", 12);
	w->write("version_bp", SCENE_LO_BPARAMS_VERSION);

	w->close_section();

	m_revision.save_v12(w);

	w->open_section("version");
	w->write("value", SCENE_VERSION);
	w->close_section();

	w->save_to(PA_MAPS, std::string(name) + ".level");

	delete w;

	xr_file_system& fs = xr_file_system::instance();
	fs.create_folder(PA_MAPS, name);

	std::string parts_path(name);
	parts_path += '\\';
	for (xr_scene_part_vec_it it = m_parts.begin(), end = m_parts.end();
			it != end; ++it) {
		xr_scene_part* part = *it;

		const type_info& type = typeid(*part);

		w = new xr_ini_writer();
		if (type == typeid(xr_scene_ai_map) ||
			type == typeid(xr_scene_details) ||
			type == typeid(xr_scene_wallmarks))
		{
			w->w_chunk(TOOLS_CHUNK_GUID, m_guid);
			w->open_chunk(part->chunk_id());
			part->save(*w);
			w->close_chunk();
		}
		else
		{
			write_guid(w);
			part->save_v12(w);
		}

		if (!w->save_to(PA_MAPS, parts_path + part->file_name()))
			msg("can't save scene part %s", part->file_name());
		delete w;
	}

	return true;
}

void xr_scene::write_guid(xr_ini_writer* w)
{
	//msg("%s", "guid save_v12");
	w->open_section("guid");
	uint32_t tmp[4];
	memcpy(tmp, ((char *)m_guid.g) + 1, sizeof(uint32_t) * 4);
	xr_guid *guid2 = new xr_guid;
	memcpy(&guid2->g, tmp, sizeof(uint32_t) * 4);
	w->write("guid_g0", &m_guid);
	w->write("guid_g1", guid2);
	w->close_section();
}

void xr_scene::write_revision(xr_ini_writer* w, bool scene_part)
{
	m_revision.save_v12(w, scene_part);
}
