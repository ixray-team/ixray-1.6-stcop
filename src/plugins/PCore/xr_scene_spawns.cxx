// spawn.part (ESceneSpawnTools, CSpawnPoint, ::SSpawnData)
#include "xr_scene_spawns.h"
#include "xr_scene.h"
#include "xr_entity_factory.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_spawn_object::xr_spawn_object(xr_scene& scene, const char* name):
	xr_custom_object(scene, TOOLS_CLASS_SPAWN), m_attached_object(0)
{
	if (name == 0) {
		m_type = SPAWNPOINT_TYPE_ENTITY;
		m_entity = 0;
	} else if (std::strcmp(name, "$rpoint") == 0) {
		m_type = SPAWNPOINT_TYPE_RPOINT;
		m_team = 0;
		m_respawn = 0;
		m_game = 0;
	} else if (std::strcmp(name, "$env_mod") == 0) {
		m_type = SPAWNPOINT_TYPE_ENV_MOD;
		m_radius = 10.f;
		m_power = 1.f;
		m_view_distance = 300.f;
		m_fog_color = 0x00808080;
		m_fog_density = 1.f;
		m_ambient_color = 0;
		m_sky_color = 0xffffff;
		m_hemi_color = 0xffffff;
	} else {
		m_type = SPAWNPOINT_TYPE_ENTITY;
		m_entity = create_entity(name);
		xr_assert(m_entity);
	}
}

xr_spawn_object::~xr_spawn_object()
{
	if (m_type == SPAWNPOINT_TYPE_ENTITY)
		delete m_entity;
	delete m_attached_object;
}

void xr_spawn_object::load(xr_reader& r)
{
	uint16_t version;
	if (!r.r_chunk(SPAWNPOINT_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == SPAWNPOINT_VERSION);

	xr_custom_object::load(r);

	if (r.find_chunk(SPAWNPOINT_CHUNK_SECTION)) {
		m_type = SPAWNPOINT_TYPE_ENTITY;
		const char* name = r.skip_sz();
		r.debug_find_chunk();

		if (!r.find_chunk(SPAWNPOINT_CHUNK_SPAWNDATA))
			xr_not_expected();
		size_t size = r.r_u32();
		xr_packet packet;
		r.r_packet(packet, size);
		r.debug_find_chunk();
		m_entity = create_entity(name);
		xr_assert(m_entity);
		m_entity->spawn_read(packet);
	} else {
		r.r_chunk<uint32_t>(SPAWNPOINT_CHUNK_TYPE, m_type);
		if (m_type == SPAWNPOINT_TYPE_RPOINT) {
			if (!r.find_chunk(SPAWNPOINT_CHUNK_RPOINT_PARAMS))
				xr_not_expected();
			m_team = r.r_u8();
			m_respawn = r.r_u8();
			m_game = r.r_u8();
			r.advance(sizeof(uint8_t));
			r.debug_find_chunk();
		} else if (m_type == SPAWNPOINT_TYPE_ENV_MOD) {
			if (!r.find_chunk(SPAWNPOINT_CHUNK_ENV_MOD_PARAMS))
				xr_not_expected();
			m_radius = r.r_float();
			m_power = r.r_float();
			m_view_distance = r.r_float();
			m_fog_color = r.r_u32();
			m_fog_density = r.r_float();
			m_ambient_color = r.r_u32();
			m_sky_color = r.r_u32();
			r.debug_find_chunk();
			r.r_chunk<uint32_t>(SPAWNPOINT_CHUNK_ENV_MOD_HEMI, m_hemi_color);
		} else {
			xr_not_expected();
		}
	}

	xr_custom_object_vec objects;
	scene().load_objects(r, SPAWNPOINT_CHUNK_ATTACHED_OBJECT, objects);
	if (!objects.empty()) {
		xr_assert(objects.size() == 1 && objects[0]->class_id() == TOOLS_CLASS_SHAPE);
		m_attached_object = objects[0];
	}
}

void xr_spawn_object::save(xr_writer& w) const
{
	xr_custom_object::save(w);

	uint16_t version = SPAWNPOINT_VERSION;
	w.w_chunk<uint16_t>(SPAWNPOINT_CHUNK_VERSION, version);

	if (m_attached_object) {
		xr_custom_object_vec objects(1, m_attached_object);
		scene().save_objects(w, SPAWNPOINT_CHUNK_ATTACHED_OBJECT, objects);
	}
	if (m_type == SPAWNPOINT_TYPE_ENTITY) {
		xr_assert(m_entity);
		w.w_chunk(SPAWNPOINT_CHUNK_SECTION, m_entity->name());

		w.open_chunk(SPAWNPOINT_CHUNK_SPAWNDATA);
		xr_packet packet;
		m_entity->spawn_write(packet, true);
		w.w_size_u32(packet.w_tell());
		w.w_packet(packet);
		w.close_chunk();
	} else {
		w.w_chunk(SPAWNPOINT_CHUNK_TYPE, m_type);
		if (m_type == SPAWNPOINT_TYPE_RPOINT) {
			w.open_chunk(SPAWNPOINT_CHUNK_RPOINT_PARAMS);
			w.w_u8(m_team);
			w.w_u8(m_respawn);
			w.w_u8(m_game);
			w.w_u8(0);
			w.close_chunk();
		} else if (m_type == SPAWNPOINT_TYPE_ENV_MOD) {
			w.open_chunk(SPAWNPOINT_CHUNK_ENV_MOD_PARAMS);
			w.w_float(m_radius);
			w.w_float(m_power);
			w.w_float(m_view_distance);
			w.w_u32(m_fog_color);
			w.w_float(m_fog_density);
			w.w_u32(m_ambient_color);
			w.w_u32(m_sky_color);
			w.close_chunk();
			w.w_chunk<uint32_t>(SPAWNPOINT_CHUNK_ENV_MOD_HEMI, m_hemi_color);
		} else {
			xr_not_expected();
		}
	}
}

void xr_spawn_object::save_v12(xr_ini_writer* w) const
{
	xr_custom_object::save_v12(w);

	if (this->m_attached_object)
		w->write("attached_count", 1);

	w->write("type", this->m_type);
	w->write("version", SPAWNPOINT_VERSION_V12);

	if (m_type == SPAWNPOINT_TYPE_ENTITY) {
		xr_assert(m_entity);

		w->open_section("spawndata");
		
		xr_ini_packet *ini_packet = new xr_ini_packet();
		xr_packet* packet = ini_packet;
		m_entity->spawn_write(*packet, true);
		//w->w_packet(*ini_packet);
		w->write_packet(ini_packet);

		w->write("fl", 0);
		w->write("name", this->m_entity->name(), false);
		w->close_section();

		delete packet;
	}
	else if (m_type == SPAWNPOINT_TYPE_RPOINT)
	{
		w->write("game_type", m_game);
		w->write("rp_profile", "", false);
		w->write("rp_type", m_respawn);
		w->write("team_id", m_team);
	} 
	else if (m_type == SPAWNPOINT_TYPE_ENV_MOD)
	{
		w->write("ambient_color", m_ambient_color);
		w->write("em_flags", 65535); // apparently em_flags don't exist in current code
		w->write("em_power", m_power);
		w->write("em_radius", m_radius);
		w->write("fog_color", m_fog_color);
		w->write("fog_density", m_fog_density);
		w->write("hemi_color", m_hemi_color);
		w->write("sky_color", m_sky_color);
		w->write("view_dist", m_view_distance);
	}
	else
	{
		xr_not_expected();
	}
	
	if (this->m_attached_object)
	{
		xr_custom_object_vec objects(1, m_attached_object);
		scene().save_objects(w, objects, "attached");
	}
}

////////////////////////////////////////////////////////////////////////////////

xr_scene_spawns::xr_scene_spawns(xr_scene& scene):
	xr_scene_objects(scene, "spawn.part", SCENE_CHUNK_SPAWNS), m_flags(0) {}

xr_scene_spawns::~xr_scene_spawns() {}

void xr_scene_spawns::load(xr_reader& r)
{
	uint16_t version = 0;
	r.r_chunk<uint16_t>(TOOLS_CHUNK_VERSION, version);
	xr_assert(version == 0);
	xr_scene_objects::load(r);
	r.r_chunk<uint32_t>(SPAWNS_CHUNK_COMMON_FLAGS, m_flags);
}

void xr_scene_spawns::save(xr_writer& w) const
{
	xr_scene_objects::save(w);
	w.w_chunk<uint16_t>(TOOLS_CHUNK_VERSION, 0);
	w.w_chunk<uint32_t>(SPAWNS_CHUNK_COMMON_FLAGS, m_flags);
}

void xr_scene_spawns::save_v12(xr_ini_writer* w) const
{
	w->open_section("main");
	w->write("flags", 0);
	w->write("objects_count", this->objects().size());
	w->write("version", 0);
	w->close_section();

	scene().write_revision(w);

	xr_scene_objects::save_v12(w);
}
