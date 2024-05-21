#include <cstring>
#include "xr_math.h"
#include "xr_entity.h"
#include "xr_packet.h"

using namespace xray_re;

const uint8_t UNDEF8 = UINT8_MAX;
const uint16_t UNDEF16 = UINT16_MAX;
const uint32_t UNDEF32 = UINT32_MAX;

////////////////////////////////////////////////////////////////////////////////

cse_motion::cse_motion() {}

cse_motion::~cse_motion() {}

void cse_motion::motion_read(xr_packet& packet)
{
	packet.r_sz(m_motion_name);
}

void cse_motion::motion_write(xr_packet& packet)
{
	packet.w_sz(m_motion_name);
}

////////////////////////////////////////////////////////////////////////////////

cse_visual::cse_visual(): m_flags(0) {}

cse_visual::~cse_visual() {}

void cse_visual::visual_read(xr_packet& packet, uint16_t version)
{
	packet.r_sz(m_visual_name);
	if (version > CSE_VERSION_0x67)
		packet.r_u8(m_flags);
}

void cse_visual::visual_write(xr_packet& packet, uint16_t version)
{
	packet.w_sz(m_visual_name);
	if (version > CSE_VERSION_0x67)
		packet.w_u8(m_flags);
}

////////////////////////////////////////////////////////////////////////////////

cse_shape::cse_shape() {}

cse_shape::~cse_shape() {}

shape_def_vec& cse_shape::shapes() { return m_shapes; }

void cse_shape::cform_merge(xr_packet& packet)
{
	for (uint_fast32_t n = packet.r_u8(); n; --n) {
		uint8_t def_type = packet.r_u8();
		if (def_type == SHAPE_SPHERE) {
			packet.r_advance(sizeof(fvector3));
			packet.r_float();
		} else if (def_type == SHAPE_BOX) {
			packet.r_advance(4*sizeof(fvector3));
		} else {
			xr_not_expected();
		}
	}
}

void cse_shape::cform_read(xr_packet& packet)
{
	m_shapes.clear();
	for (uint_fast32_t n = packet.r_u8(); n; --n) {
		m_shapes.push_back(shape_def());
		shape_def& def = m_shapes.back();
		packet.r_u8(def.type);
		if (def.type == SHAPE_SPHERE) {
			packet.r_vec3(def.sphere.p);
			packet.r_float(def.sphere.r);
		} else if (def.type == SHAPE_BOX) {
			packet.r_matrix(def.box);
		} else {
			xr_not_expected();
		}
	}
}

void cse_shape::cform_write(xr_packet& packet)
{
	packet.w_size_u8(m_shapes.size());
	for (shape_def_vec_it it = m_shapes.begin(), end = m_shapes.end(); it != end; ++it) {
		packet.w_u8(it->type);
		if (it->type == SHAPE_SPHERE) {
			packet.w_vec3(it->sphere.p);
			packet.w_float(it->sphere.r);
		} else if (it->type == SHAPE_BOX) {
			packet.w_matrix(it->box);
		} else {
			xr_not_expected();
		}
	}
}

////////////////////////////////////////////////////////////////////////////////

cse_ph_skeleton::cse_ph_skeleton(): m_flags(0), m_source_id(UNDEF16) {}

cse_ph_skeleton::~cse_ph_skeleton() {}

void cse_ph_skeleton::state_read(xr_packet& packet, uint16_t size)
{
	packet.r_sz(m_root_bone);
	packet.r_u8(m_flags);
	packet.r_u16(m_source_id);
}

void cse_ph_skeleton::state_write(xr_packet& packet)
{
	packet.w_sz(m_root_bone);
	packet.w_u8(m_flags);
	packet.w_u16(m_source_id);
}

void cse_ph_skeleton::update_read(xr_packet& packet) {}
void cse_ph_skeleton::update_write(xr_packet& packet) {}

void cse_ph_skeleton::data_load(xr_packet& packet) {}
void cse_ph_skeleton::data_save(xr_packet& packet) {}

////////////////////////////////////////////////////////////////////////////////

cse_abstract::cse_abstract():
	m_version(0), m_script_version(0), m_respawn_time(0),
	m_id(UNDEF16), m_id_parent(UNDEF16), m_id_phantom(UNDEF16),
	m_s_game_id(GAME_ANY), m_s_rp(0xfe), m_s_flags(0),
	m_cs_unk1_u16(UNDEF16),
	m_spawn_id(UNDEF16)
{
	m_o_position.set();
	m_o_angle.set();
}

cse_abstract::~cse_abstract() {}
cse_shape* cse_abstract::shape() { return 0; }
cse_visual* cse_abstract::visual() { return 0; }
cse_motion* cse_abstract::motion() { return 0; }
cse_abstract* cse_abstract::base() { return this; }

void cse_abstract::spawn_merge(xr_packet& packet)
{
	uint16_t dummy16 = 0;
	packet.r_begin(dummy16);
	xr_assert(dummy16 == M_SPAWN);

	packet.skip_sz();
	const char* s_name_replace = packet.skip_sz();
	xr_assert(m_s_name_replace == s_name_replace);
	packet.r_u8();
	packet.r_u8();
	packet.r_advance(2*sizeof(fvector3));
	packet.r_u16();
	packet.r_u16();
	packet.r_u16();
	packet.r_u16();
	uint16_t s_flags;
	packet.r_u16(s_flags);
	uint16_t version = 0;
	if (s_flags & FL_SPAWN_DESTROY_ON_SPAWN)
		packet.r_u16(version);
	if (version > CSE_VERSION_0x78)
		packet.r_u16();
	uint16_t script_version = 0;
	if (version > CSE_VERSION_0x45)
		packet.r_u16(script_version);
	if (version > CSE_VERSION_0x46) {
		size_t n = (version > CSE_VERSION_0x5d) ? packet.r_u16() : packet.r_u8();
		xr_assert(n == 0);
	}
	if (version > CSE_VERSION_0x4f)
		packet.r_u16(m_spawn_id);
	if (version < CSE_VERSION_0x70) {
		if (version > CSE_VERSION_0x52)
			packet.r_float();
		if (version > CSE_VERSION_0x53) {
			packet.r_u32();
			packet.skip_sz();
			packet.r_u32();
			packet.r_u32();
			packet.r_u64();
		}
		if (version > CSE_VERSION_0x54) {
			packet.r_u64();
			packet.r_u64();
		}
	}

	uint16_t size = packet.r_u16();
	xr_assert(size > 2);
	state_merge(packet, version);
}

void cse_abstract::spawn_read(xr_packet& packet)
{
	uint16_t dummy16 = 0;
	packet.r_begin(dummy16);
	xr_assert(dummy16 == M_SPAWN);

	packet.r_sz(m_s_name);
	packet.r_sz(m_s_name_replace);
	packet.r_u8(m_s_game_id);
	packet.r_u8(m_s_rp);
	packet.r_vec3(m_o_position);
	packet.r_vec3(m_o_angle);
	packet.r_u16(m_respawn_time);
	packet.r_u16(m_id);
	packet.r_u16(m_id_parent);
	packet.r_u16(m_id_phantom);
	packet.r_u16(m_s_flags);
	if (m_s_flags & FL_SPAWN_DESTROY_ON_SPAWN)
		packet.r_u16(m_version);
	if (m_version > CSE_VERSION_0x78)
		packet.r_u16(m_cs_unk1_u16);
	else
		m_cs_unk1_u16 = UNDEF16;
	if (m_version > CSE_VERSION_0x45)
		packet.r_u16(m_script_version);
	if (m_version > CSE_VERSION_0x46) {
		size_t n = (m_version > CSE_VERSION_0x5d) ? packet.r_u16() : packet.r_u8();
		xr_assert(n == 0);
	}
	if (m_version > CSE_VERSION_0x4f)
		packet.r_u16(m_spawn_id);
	if (m_version < CSE_VERSION_0x70) {
		if (m_version > CSE_VERSION_0x52)
			packet.r_float();
		if (m_version > CSE_VERSION_0x53) {
			packet.r_u32();
			packet.skip_sz();
			packet.r_u32();
			packet.r_u32();
			packet.r_u64();
		}
		if (m_version > CSE_VERSION_0x54) {
			packet.r_u64();
			packet.r_u64();
		}
	}
	uint16_t size = packet.r_u16();
	xr_assert(size > 2);
	state_read(packet, size);
}

void cse_abstract::spawn_write(xr_packet& packet, bool local)
{
	packet.w_begin(M_SPAWN);
	packet.w_sz(m_s_name);
	packet.w_sz(m_s_name_replace);
	packet.w_u8(m_s_game_id);
	packet.w_u8(m_s_rp);
	packet.w_vec3(m_o_position);
	packet.w_vec3(m_o_angle);
	packet.w_u16(m_respawn_time);
	packet.w_u16(m_id);
	packet.w_u16(m_id_parent);
	packet.w_u16(m_id_phantom);
	m_s_flags |= FL_SPAWN_DESTROY_ON_SPAWN;
	if (local)
		m_s_flags |= FL_SPAWN_ENABLED;
	else
		m_s_flags &= ~(FL_SPAWN_ENABLED|FL_SPAWN_IF_DESTROYED_ONLY);
	packet.w_u16(m_s_flags);
	packet.w_u16(m_version);
	if (m_version > CSE_VERSION_0x78)
		packet.w_u16(m_cs_unk1_u16);
	packet.w_u16(m_script_version);
	packet.w_u16(0);
	packet.w_u16(m_spawn_id);
	if (m_version < CSE_VERSION_0x70) {
		packet.w_float(1.f);		// m_spawn_probability
		packet.w_u32(0x1f);		// m_spawn_flags
		packet.w_sz("");		// m_spawn_control
		packet.w_u32(1);		// m_max_spawn_count
		packet.w_u32(0);		// m_spawn_count
		packet.w_u64(0);		// m_last_spawn_time
		packet.w_u64(0);		// m_min_spawn_interval
		packet.w_u64(0);		// m_max_spawn_interval
	}
	size_t pos = packet.w_tell();
	packet.w_u16(0);
	state_write(packet);
	size_t size = packet.w_tell() - pos;
	xr_assert(size > 2);
	packet.w_seek(pos);
	packet.w_size_u16(size);
	packet.w_seek(pos + size);
}

inline cse_abstract::sm_record::sm_record(const char* _name, size_t _offset):
	name(_name), offset(_offset) {}

void cse_abstract::set_save_marker(xr_packet& packet, esm_mode mode, bool check, const char* name)
{
	if (m_version < CSE_VERSION_0x7c_HACK)
		return;

	if (check) {
		const sm_record& sm = m_markers.top();
		if (std::strcmp(name, sm.name))
			xr_not_expected();
		if (mode == SM_SAVE) {
			size_t diff = packet.w_tell() - sm.offset;
			packet.w_u16(uint16_t(diff & UINT16_MAX));
		} else {
			size_t diff = packet.r_tell() - sm.offset;
			if (diff != packet.r_u16())
				xr_not_expected();
		}
		m_markers.pop();
	} else {
		m_markers.push(sm_record(name,
				mode == SM_SAVE ? packet.w_tell() : packet.r_tell()));
	}
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_graph_point::cse_alife_graph_point()
{
	std::memset(m_locations, 0, sizeof(m_locations));
}

void cse_alife_graph_point::state_merge(xr_packet& packet, uint16_t version) {}

void cse_alife_graph_point::state_read(xr_packet& packet, uint16_t size)
{
	packet.r_sz(m_connection_point_name);
	if (m_version >= CSE_VERSION_0x21)
		packet.r_sz(m_connection_level_name);
	else
		packet.r_s32();
	packet.r_cseq(4, m_locations);
}

void cse_alife_graph_point::state_write(xr_packet& packet)
{
	packet.w_sz(m_connection_point_name);
	packet.w_sz(m_connection_level_name);
	packet.w_cseq(4, m_locations);
}

void cse_alife_graph_point::update_read(xr_packet& packet) {}

void cse_alife_graph_point::update_write(xr_packet& packet) {}

////////////////////////////////////////////////////////////////////////////////

cse_alife_object::cse_alife_object():
	m_graph_id(UNDEF16), m_distance(0), m_direct_control(false),
	m_node_id(UNDEF32), m_flags(UNDEF32),
	m_story_id(UNDEF32), m_spawn_story_id(UNDEF32) {}

void cse_alife_object::state_merge(xr_packet& packet, uint16_t version)
{
	if (version >= CSE_VERSION_0x01) {
		if (version <= CSE_VERSION_0x18)
			packet.r_s8();
		else if (version < CSE_VERSION_0x53)
			packet.r_float();
		if (version < CSE_VERSION_0x53)
			packet.r_s32();
		if (version < CSE_VERSION_0x04)
			packet.r_u16();
		packet.r_u16(m_graph_id);
		packet.r_float(m_distance);
	}
	if (version >= CSE_VERSION_0x04)
		packet.r_u32();
	if (version >= CSE_VERSION_0x08)
		packet.r_u32(m_node_id);
	if (version > CSE_VERSION_0x16 && version <= CSE_VERSION_0x4f)
		packet.r_u16();
	if (version > CSE_VERSION_0x17 && version < CSE_VERSION_0x54)
		packet.skip_sz();
	if (version > CSE_VERSION_0x31)
		packet.r_u32();
	if (version > CSE_VERSION_0x39)
		packet.skip_sz();
	if (version > CSE_VERSION_0x3d)
		packet.r_u32();
	if (version > CSE_VERSION_0x6f)
		packet.r_u32();
}

void cse_alife_object::state_read(xr_packet& packet, uint16_t size)
{
	if (m_version >= CSE_VERSION_0x01) {
		if (m_version <= CSE_VERSION_0x18)
			packet.r_s8();
		else if (m_version < CSE_VERSION_0x53)
			packet.r_float();
		if (m_version < CSE_VERSION_0x53)
			packet.r_s32();
		if (m_version < CSE_VERSION_0x04)
			packet.r_u16();
		packet.r_u16(m_graph_id);
		packet.r_float(m_distance);
	}
	if (m_version >= CSE_VERSION_0x04)
		m_direct_control = (packet.r_u32() & 1) != 0;
	if (m_version >= CSE_VERSION_0x08)
		packet.r_u32(m_node_id);
	if (m_version > CSE_VERSION_0x16 && m_version <= CSE_VERSION_0x4f)
		packet.r_u16(m_spawn_id);
	if (m_version > CSE_VERSION_0x17 && m_version < CSE_VERSION_0x54)
		packet.skip_sz();
	if (m_version > CSE_VERSION_0x31)
		packet.r_u32(m_flags);
	if (m_version > CSE_VERSION_0x39)
		packet.r_sz(m_ini_string);
	if (m_version > CSE_VERSION_0x3d)
		packet.r_u32(m_story_id);
	if (m_version > CSE_VERSION_0x6f)
		packet.r_u32(m_spawn_story_id);
}

void cse_alife_object::state_write(xr_packet& packet)
{
	packet.w_u16(m_graph_id);
	packet.w_float(m_distance);
	packet.w_u32(m_direct_control ? 1 : 0);
	packet.w_u32(m_node_id);
	packet.w_u32(m_flags);
	packet.w_sz(m_ini_string);
	packet.w_u32(m_story_id);
	if (m_version > CSE_VERSION_0x6f)
		packet.w_u32(m_spawn_story_id);
}

void cse_alife_object::update_read(xr_packet& packet) {}

void cse_alife_object::update_write(xr_packet& packet) {}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_dynamic_object::state_merge(xr_packet& packet, uint16_t version)
{
	cse_alife_object::state_merge(packet, version);
}

void cse_alife_dynamic_object::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_object::state_read(packet, size);
}

void cse_alife_dynamic_object::state_write(xr_packet& packet)
{
	cse_alife_object::state_write(packet);
}

void cse_alife_dynamic_object::update_read(xr_packet& packet) {}

void cse_alife_dynamic_object::update_write(xr_packet& packet) {}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_dynamic_object_visual::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object::state_read(packet, size);
	if (m_version > CSE_VERSION_0x1f)
		cse_visual::visual_read(packet, m_version);
}

void cse_alife_dynamic_object_visual::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object::state_write(packet);
	cse_visual::visual_write(packet, m_version);
}

void cse_alife_dynamic_object_visual::update_read(xr_packet& packet) {}

void cse_alife_dynamic_object_visual::update_write(xr_packet& packet) {}

cse_visual* cse_alife_dynamic_object_visual::visual()
{
	return static_cast<cse_visual*>(this);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_object_climable::cse_alife_object_climable(): m_flags(0), m_game_material("materials\\fake_ladders") {}

void cse_alife_object_climable::state_read(xr_packet& packet, uint16_t size)
{
	if (m_version > CSE_VERSION_0x63)
		cse_alife_dynamic_object::state_read(packet, size);
	cse_shape::cform_read(packet);

	if (m_version >= CSE_VERSION_0x80)
		packet.r_sz(m_game_material);
}

void cse_alife_object_climable::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object::state_write(packet);
	cse_shape::cform_write(packet);

	if (m_version >= CSE_VERSION_0x80)
		packet.w_sz(m_game_material);
}

void cse_alife_object_climable::update_read(xr_packet& packet) {}

void cse_alife_object_climable::update_write(xr_packet& packet) {}

cse_shape* cse_alife_object_climable::shape()
{
	return static_cast<cse_shape*>(this);
}

////////////////////////////////////////////////////////////////////////////////

cse_smart_cover::cse_smart_cover():
	m_cs_unk1_sz(""), m_cs_unk2_float(0.f),
	m_enter_min_enemy_distance(15.f), m_exit_min_enemy_distance(10.f),
	m_is_combat_cover(true), m_cs_unk3_u8(0) {}

void cse_smart_cover::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object::state_read(packet, size);
	cse_shape::cform_read(packet);
	packet.r_sz(m_cs_unk1_sz);
	packet.r_float(m_cs_unk2_float);
	if (m_version >= CSE_VERSION_0x78) {
		packet.r_float(m_enter_min_enemy_distance);
		packet.r_float(m_exit_min_enemy_distance);
	}
	if (m_version >= CSE_VERSION_0x7a)
		packet.r_bool(m_is_combat_cover);
	if (m_version >= CSE_VERSION_0x80)
		packet.r_u8(m_cs_unk3_u8);
}

void cse_smart_cover::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object::state_write(packet);
	cse_shape::cform_write(packet);
	packet.w_sz(m_cs_unk1_sz);
	packet.w_float(m_cs_unk2_float);
	if (m_version >= CSE_VERSION_0x78) {
		packet.w_float(m_enter_min_enemy_distance);
		packet.w_float(m_exit_min_enemy_distance);
	}
	if (m_version >= CSE_VERSION_0x7a)
		packet.w_bool(m_is_combat_cover);
	if (m_version >= CSE_VERSION_0x80)
		packet.w_u8(m_cs_unk3_u8);
}

void cse_smart_cover::update_read(xr_packet& packet) {}

void cse_smart_cover::update_write(xr_packet& packet) {}

cse_shape* cse_smart_cover::shape() { return static_cast<cse_shape*>(this); }

////////////////////////////////////////////////////////////////////////////////

cse_alife_object_physic::cse_alife_object_physic(): m_type(0), m_mass(10.f),
	m_num_items(0) {}

void cse_alife_object_physic::state_read(xr_packet& packet, uint16_t size)
{
	if (m_version >= CSE_VERSION_0x0e) {
		if (m_version < CSE_VERSION_0x10)
			cse_alife_dynamic_object::state_read(packet, size);
		else
			cse_alife_dynamic_object_visual::state_read(packet, size);
		if (m_version < CSE_VERSION_0x20)
			cse_visual::visual_read(packet, m_version);
	}
	if (m_version >= CSE_VERSION_0x40)
		cse_ph_skeleton::state_read(packet, size);
	packet.r_u32(m_type);
	packet.r_float(m_mass);
	if (m_version > CSE_VERSION_0x09)
		packet.r_sz(m_fixed_bones);
	if (m_version > CSE_VERSION_0x1c && m_version < CSE_VERSION_0x41)
		packet.r_sz(cse_visual::m_startup_animation);
	if (m_version < CSE_VERSION_0x40) {
		if (m_version > CSE_VERSION_0x27)
			packet.r_u8(cse_ph_skeleton::m_flags);
		if (m_version > CSE_VERSION_0x38)
			packet.r_u16(cse_ph_skeleton::m_source_id);
		if ((m_version > CSE_VERSION_0x3c) && (cse_ph_skeleton::m_flags & FL_SAVED_DATA) != 0)
			data_load(packet);
	}
}

void cse_alife_object_physic::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
	cse_ph_skeleton::state_write(packet);
	packet.w_u32(m_type);
	packet.w_float(m_mass);
	packet.w_sz(m_fixed_bones);
}

void cse_alife_object_physic::update_read(xr_packet& packet)
{
	cse_ph_skeleton::update_read(packet);
	if (!packet.r_eof()) {
		xr_assert(m_version >= CSE_VERSION_0x7a && m_version <= CSE_VERSION_COP);
		packet.r_u8(m_num_items);
		if (m_num_items) {
			packet.r_vec3(m_state.force);
			packet.r_vec3(m_state.torque);
			packet.r_vec3(m_state.position);
			packet.r_float(m_state.quaternion.x);
			packet.r_float(m_state.quaternion.y);
			packet.r_float(m_state.quaternion.z);
			packet.r_float(m_state.quaternion.w);
			uint_fast16_t flags = m_num_items >> 5;
			m_state.enabled = (flags & 1) != 0;
			if (flags & 0x2)
				m_state.angular_vel.set(0, 0, 0);
			else
				packet.r_vec3(m_state.angular_vel);
			if (flags & 0x4)
				m_state.linear_vel.set(0, 0, 0);
			else
				packet.r_vec3(m_state.linear_vel);
			if (!packet.r_eof())
				packet.r_bool();
		}
	}
}

void cse_alife_object_physic::update_write(xr_packet& packet)
{
	cse_ph_skeleton::update_write(packet);
	if (m_version >= CSE_VERSION_0x7a) {
		xr_assert(m_version <= CSE_VERSION_COP);
		if (m_num_items) {
			uint8_t num_items = uint8_t(m_num_items & 0x1f);
			if (m_state.enabled)
				num_items |= 0x20;
			if (m_state.angular_vel.square_magnitude() < 1e-7f)
				num_items |= 0x40;
			if (m_state.linear_vel.square_magnitude() < 1e-7f)
				num_items |= 0x80;
			packet.w_u8(num_items);
			packet.w_vec3(m_state.force);
			packet.w_vec3(m_state.torque);
			packet.w_vec3(m_state.position);
			packet.w_float(m_state.quaternion.x);
			packet.w_float(m_state.quaternion.y);
			packet.w_float(m_state.quaternion.z);
			packet.w_float(m_state.quaternion.w);
			if ((num_items & 0x40) == 0)
				packet.w_vec3(m_state.angular_vel);
			if ((num_items & 0x80) == 0)
				packet.w_vec3(m_state.linear_vel);
			packet.w_bool(true);
		} else {
			packet.w_u8(0);
		}
	}
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_object_hanging_lamp::cse_alife_object_hanging_lamp(): m_flags(0),
	m_cs_unk1_float(1.f), m_cs_unk2_float(1.f), m_cs_unk3_float(1.f) {}

void cse_alife_object_hanging_lamp::state_read(xr_packet& packet, uint16_t size)
{
	if (m_version > CSE_VERSION_0x14)
		cse_alife_dynamic_object_visual::state_read(packet, size);
	if (m_version >= CSE_VERSION_0x45)
		cse_ph_skeleton::state_read(packet, size);
	if (m_version < CSE_VERSION_0x20)
		cse_visual::visual_read(packet, m_version);
	if (m_version < CSE_VERSION_0x31) {
		packet.r_u32(m_color);
		packet.r_sz(m_color_animator);
		packet.skip_sz();
		packet.skip_sz();
		packet.r_float(m_range);
		packet.r_angle8();			// FIXME mb
		if (m_version > CSE_VERSION_0x0a)
			packet.r_float(m_brightness);
		if (m_version > CSE_VERSION_0x0b)
			packet.r_u16(m_flags);
		if (m_version > CSE_VERSION_0x0c)
			packet.r_u32();
		if (m_version > CSE_VERSION_0x11)
			packet.r_sz(cse_visual::m_startup_animation);
		if (m_version > CSE_VERSION_0x2a) {
			packet.skip_sz();
			packet.r_u32();
		}
		if (m_version > CSE_VERSION_0x2b)
			packet.r_sz(m_fixed_bones);
		if (m_version > CSE_VERSION_0x2c)
			packet.r_float(m_health);
	} else {
		packet.r_u32(m_color);
		packet.r_float(m_brightness);
		packet.r_sz(m_color_animator);
		packet.r_float(m_range);
		packet.r_u16(m_flags);
		packet.r_sz(cse_visual::m_startup_animation);
		packet.r_sz(m_fixed_bones);
		packet.r_float(m_health);
	}
	if (m_version > CSE_VERSION_0x37) {
		packet.r_float(m_virtual_size);
		packet.r_float(m_ambient_radius);
		packet.r_float(m_ambient_power);
		packet.r_sz(m_ambient_texture);
		packet.r_sz(m_light_texture);
		packet.r_sz(m_light_main_bone);
		packet.r_float(m_spot_cone_angle);
		packet.r_sz(m_glow_texture);
		packet.r_float(m_glow_radius);
	}
	if (m_version > CSE_VERSION_0x60)
		packet.r_sz(m_light_ambient_bone);
	if (m_version > CSE_VERSION_0x76) {
		packet.r_float(m_cs_unk1_float);
		packet.r_float(m_cs_unk2_float);
		packet.r_float(m_cs_unk3_float);
	}
}

void cse_alife_object_hanging_lamp::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
	cse_ph_skeleton::state_write(packet);
	packet.w_u32(m_color);
	packet.w_float(m_brightness);
	packet.w_sz(m_color_animator);
	packet.w_float(m_range);
	packet.w_u16(m_flags);
	packet.w_sz(cse_visual::m_startup_animation);
	packet.w_sz(m_fixed_bones);
	packet.w_float(m_health);
	packet.w_float(m_virtual_size);
	packet.w_float(m_ambient_radius);
	packet.w_float(m_ambient_power);
	packet.w_sz(m_ambient_texture);
	packet.w_sz(m_light_texture);
	packet.w_sz(m_light_main_bone);
	packet.w_float(m_spot_cone_angle);
	packet.w_sz(m_glow_texture);
	packet.w_float(m_glow_radius);
	packet.w_sz(m_light_ambient_bone);
	if (m_version > CSE_VERSION_0x76) {
		packet.w_float(m_cs_unk1_float);
		packet.w_float(m_cs_unk2_float);
		packet.w_float(m_cs_unk3_float);
	}
}

void cse_alife_object_hanging_lamp::update_read(xr_packet& packet)
{
	cse_ph_skeleton::update_read(packet);
}

void cse_alife_object_hanging_lamp::update_write(xr_packet& packet)
{
	cse_ph_skeleton::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_object_projector::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object_visual::state_read(packet, size);
}

void cse_alife_object_projector::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
}

void cse_alife_object_projector::update_read(xr_packet& packet) {}

void cse_alife_object_projector::update_write(xr_packet& packet) {}

////////////////////////////////////////////////////////////////////////////////

cse_inventory_box::cse_inventory_box():cse_alive_inventory_box__unk1_u8(1), cse_alive_inventory_box__unk2_u8(0), tip(""){};

void cse_inventory_box::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object_visual::state_read(packet, size);
}

void cse_inventory_box::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
}

void cse_inventory_box::update_read(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::update_read(packet);
}

void cse_inventory_box::update_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_object_breakable::cse_alife_object_breakable(): m_health(1.f) {}

void cse_alife_object_breakable::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object_visual::state_read(packet, size);
	packet.r_float(m_health);
}

void cse_alife_object_breakable::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
	packet.w_float(m_health);
}

void cse_alife_object_breakable::update_read(xr_packet& packet) {}

void cse_alife_object_breakable::update_write(xr_packet& packet) {}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_mounted_weapon::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object_visual::state_read(packet, size);
}

void cse_alife_mounted_weapon::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
}

void cse_alife_mounted_weapon::update_read(xr_packet& packet) {}

void cse_alife_mounted_weapon::update_write(xr_packet& packet) {}

////////////////////////////////////////////////////////////////////////////////

cse_alife_stationary_mgun::cse_alife_stationary_mgun(): m_working(false)
{
	m_dest_enemy_dir.set();
}

void cse_alife_stationary_mgun::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object_visual::state_read(packet, size);
}

void cse_alife_stationary_mgun::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
}

void cse_alife_stationary_mgun::update_read(xr_packet& packet)
{
	packet.r_bool(m_working);
	packet.r_vec3(m_dest_enemy_dir);
}

void cse_alife_stationary_mgun::update_write(xr_packet& packet)
{
	packet.w_bool(m_working);
	packet.w_vec3(m_dest_enemy_dir);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_ph_skeleton_object::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object_visual::state_read(packet, size);
	if (m_version >= CSE_VERSION_0x40)
		cse_ph_skeleton::state_read(packet, size);
}

void cse_alife_ph_skeleton_object::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
	cse_ph_skeleton::state_write(packet);
}

void cse_alife_ph_skeleton_object::update_read(xr_packet& packet)
{
	cse_ph_skeleton::update_read(packet);
}

void cse_alife_ph_skeleton_object::update_write(xr_packet& packet)
{
	cse_ph_skeleton::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_car::cse_alife_car(): m_health(1.f) {}

void cse_alife_car::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object_visual::state_read(packet, size);
	if (m_version > CSE_VERSION_0x41)
		cse_ph_skeleton::state_read(packet, size);
	if (m_version > CSE_VERSION_0x34 && m_version < CSE_VERSION_0x37)
		packet.r_float();
	if (m_version > CSE_VERSION_0x5c)
		packet.r_float(m_health);
	if (m_health > 1.f)
		m_health *= 0.01f;
}

void cse_alife_car::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
	cse_ph_skeleton::state_write(packet);
	packet.w_float(m_health);
}

void cse_alife_car::update_read(xr_packet& packet)
{
	cse_ph_skeleton::update_read(packet);
}

void cse_alife_car::update_write(xr_packet& packet)
{
	cse_ph_skeleton::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_helicopter::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object_visual::state_read(packet, size);
	cse_motion::motion_read(packet);
	if (m_version >= CSE_VERSION_0x45)
		cse_ph_skeleton::state_read(packet, size);
	packet.r_sz(cse_visual::m_startup_animation);
	packet.r_sz(m_engine_sound);
}

void cse_alife_helicopter::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
	cse_motion::motion_write(packet);
	cse_ph_skeleton::state_write(packet);
	packet.w_sz(cse_visual::m_startup_animation);
	packet.w_sz(m_engine_sound);
}

void cse_alife_helicopter::update_read(xr_packet& packet)
{
	cse_ph_skeleton::update_read(packet);
}

void cse_alife_helicopter::update_write(xr_packet& packet)
{
	cse_ph_skeleton::update_write(packet);
}

cse_motion* cse_alife_helicopter::motion()
{
	return static_cast<cse_motion*>(this);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_creature_abstract::cse_alife_creature_abstract():
	m_s_team(0), m_s_squad(0), m_s_group(0),
	m_health(1.f), m_timestamp(0),
	m_flags(0), m_o_model(0), m_killer_id(UNDEF16)
{
	m_o_torso.yaw = 0;
	m_o_torso.pitch = 0;
	m_o_torso.roll = 0;
}

void cse_alife_creature_abstract::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object_visual::state_read(packet, size);
	packet.r_u8(m_s_team);
	packet.r_u8(m_s_squad);
	packet.r_u8(m_s_group);
	if (m_version > CSE_VERSION_0x12)
		packet.r_float(m_health);
	if (m_version < CSE_VERSION_0x73)
		m_health *= 0.01f;
	if (m_version < CSE_VERSION_0x20)
		cse_visual::visual_read(packet, m_version);
	if (m_version > CSE_VERSION_0x57) {
		packet.r_seq(packet.r_u32(), m_dynamic_out_restrictions);
		packet.r_seq(packet.r_u32(), m_dynamic_in_restrictions);
	}
	if (m_version > CSE_VERSION_0x5e)
		packet.r_u16(m_killer_id);
	m_o_torso.yaw = m_o_angle.y;
	m_o_torso.pitch = m_o_angle.x;
	if (m_version > CSE_VERSION_0x73)
		packet.r_u64(m_game_death_time);
}

void cse_alife_creature_abstract::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
	packet.w_u8(m_s_team);
	packet.w_u8(m_s_squad);
	packet.w_u8(m_s_group);
	if (m_version < CSE_VERSION_0x73)
		packet.w_float(m_health*100.f);
	else
		packet.w_float(m_health);
	packet.w_size_u32(m_dynamic_out_restrictions.size());
	packet.w_seq(m_dynamic_out_restrictions);
	packet.w_size_u32(m_dynamic_in_restrictions.size());
	packet.w_seq(m_dynamic_in_restrictions);
	packet.w_u16(m_killer_id);
	if (m_version > CSE_VERSION_0x73)
		packet.w_u64(m_game_death_time);
}

void cse_alife_creature_abstract::update_read(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::update_read(packet);
	packet.r_float(m_health);
	if (m_version < CSE_VERSION_0x73)
		m_health *= 0.01f;
	packet.r_u32(m_timestamp);
	packet.r_u8(m_flags);
	packet.r_vec3(m_o_position);
	packet.r_float(m_o_model);
	packet.r_float(m_o_torso.yaw);
	packet.r_float(m_o_torso.pitch);
	packet.r_float(m_o_torso.roll);
	packet.r_u8(m_s_team);
	packet.r_u8(m_s_squad);
	packet.r_u8(m_s_group);
}

void cse_alife_creature_abstract::update_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::update_write(packet);
	if (m_version < CSE_VERSION_0x73)
		packet.w_float(m_health*100.f);
	else
		packet.w_float(m_health);
	packet.w_u32(m_timestamp);
	packet.w_u8(m_flags);
	packet.w_vec3(m_o_position);
	packet.w_float(m_o_model);
	packet.w_float(m_o_torso.yaw);
	packet.w_float(m_o_torso.pitch);
	packet.w_float(m_o_torso.roll);
	packet.w_u8(m_s_team);
	packet.w_u8(m_s_squad);
	packet.w_u8(m_s_group);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_creature_crow::state_read(xr_packet& packet, uint16_t size)
{
	if (m_version > CSE_VERSION_0x14) {
		cse_alife_creature_abstract::state_read(packet, size);
		if (m_version < CSE_VERSION_0x20)
			cse_visual::visual_read(packet, m_version);
	}
}

void cse_alife_creature_crow::state_write(xr_packet& packet)
{
	cse_alife_creature_abstract::state_write(packet);
}

void cse_alife_creature_crow::update_read(xr_packet& packet)
{
	cse_alife_creature_abstract::update_read(packet);
}

void cse_alife_creature_crow::update_write(xr_packet& packet)
{
	cse_alife_creature_abstract::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_creature_phantom::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_creature_abstract::state_read(packet, size);
}

void cse_alife_creature_phantom::state_write(xr_packet& packet)
{
	cse_alife_creature_abstract::state_write(packet);
}

void cse_alife_creature_phantom::update_read(xr_packet& packet)
{
	cse_alife_creature_abstract::update_read(packet);
}

void cse_alife_creature_phantom::update_write(xr_packet& packet)
{
	cse_alife_creature_abstract::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_monster_abstract::cse_alife_monster_abstract():
	m_next_graph_id(UNDEF16), m_prev_graph_id(UNDEF16),
	m_distance_from_point(0), m_distance_to_point(0),
	m_smart_terrain_id(UNDEF16), m_task_reached(false) {}

void cse_alife_monster_abstract::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_creature_abstract::state_read(packet, size);
	if (m_version > CSE_VERSION_0x48) {
		packet.r_sz(m_out_space_restrictors);
		if (m_version > CSE_VERSION_0x49)
			packet.r_sz(m_in_space_restrictors);
	}
	if (m_version > CSE_VERSION_0x6f)
		packet.r_u16(m_smart_terrain_id);
	if (m_version > CSE_VERSION_0x71)
		packet.r_bool(m_task_reached);
}

void cse_alife_monster_abstract::state_write(xr_packet& packet)
{
	cse_alife_creature_abstract::state_write(packet);
	packet.w_sz(m_out_space_restrictors);
	packet.w_sz(m_in_space_restrictors);
	if (m_version > CSE_VERSION_0x6f)
		packet.w_u16(m_smart_terrain_id);
	if (m_version > CSE_VERSION_0x71)
		packet.w_bool(m_task_reached);
}

void cse_alife_monster_abstract::update_read(xr_packet& packet)
{
	cse_alife_creature_abstract::update_read(packet);
	packet.r_u16(m_next_graph_id);
	packet.r_u16(m_prev_graph_id);
	packet.r_float(m_distance_from_point);
	packet.r_float(m_distance_to_point);
}

void cse_alife_monster_abstract::update_write(xr_packet& packet)
{
	cse_alife_creature_abstract::update_write(packet);
	packet.w_u16(m_next_graph_id);
	packet.w_u16(m_prev_graph_id);
	packet.w_float(m_distance_from_point);
	packet.w_float(m_distance_to_point);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_monster_zombie::cse_alife_monster_zombie() {}

void cse_alife_monster_zombie::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_monster_abstract::state_read(packet, size);
	packet.r_float(m_eye_fov);
	packet.r_float(m_eye_range);
	if (m_version <= CSE_VERSION_0x05)
		packet.r_float(m_health);
	packet.r_float(m_min_range);
	packet.r_float(m_max_range);
	packet.r_float(m_attack_speed);
	packet.r_float(m_max_pursuit_radius);
	packet.r_float(m_max_home_radius);
	packet.r_float(m_hit_power);
	packet.r_u16(m_hit_interval);
	packet.r_float(m_attack_distance);
	packet.r_float(m_attack_angle);
}

void cse_alife_monster_zombie::state_write(xr_packet& packet)
{
	cse_alife_monster_abstract::state_write(packet);
	packet.w_float(m_eye_fov);
	packet.w_float(m_eye_range);
	packet.w_float(m_min_range);
	packet.w_float(m_max_range);
	packet.w_float(m_attack_speed);
	packet.w_float(m_max_pursuit_radius);
	packet.w_float(m_max_home_radius);
	packet.w_float(m_hit_power);
	packet.w_u16(m_hit_interval);
	packet.w_float(m_attack_distance);
	packet.w_float(m_attack_angle);
}

void cse_alife_monster_zombie::update_read(xr_packet& packet)
{
	cse_alife_monster_abstract::update_read(packet);
}

void cse_alife_monster_zombie::update_write(xr_packet& packet)
{
	cse_alife_monster_abstract::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_monster_base::cse_alife_monster_base(): m_spec_object_id(UNDEF16) {}

void cse_alife_monster_base::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_monster_abstract::state_read(packet, size);
	if (m_version >= CSE_VERSION_0x44)
		cse_ph_skeleton::state_read(packet, size);
	if (m_version >= CSE_VERSION_0x6d)
		packet.r_u16(m_spec_object_id);
}

void cse_alife_monster_base::state_write(xr_packet& packet)
{
	cse_alife_monster_abstract::state_write(packet);
	cse_ph_skeleton::state_write(packet);
	if (m_version >= CSE_VERSION_0x6d)
		packet.w_u16(m_spec_object_id);
}

void cse_alife_monster_base::update_read(xr_packet& packet)
{
	cse_alife_monster_abstract::update_read(packet);
	cse_ph_skeleton::update_read(packet);
}

void cse_alife_monster_base::update_write(xr_packet& packet)
{
	cse_alife_monster_abstract::update_write(packet);
	cse_ph_skeleton::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_psy_dog_phantom::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_monster_base::state_read(packet, size);
}

void cse_alife_psy_dog_phantom::state_write(xr_packet& packet)
{
	cse_alife_monster_base::state_write(packet);
}

void cse_alife_psy_dog_phantom::update_read(xr_packet& packet)
{
	cse_alife_monster_base::update_read(packet);
}

void cse_alife_psy_dog_phantom::update_write(xr_packet& packet)
{
	cse_alife_monster_base::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_trader_abstract::cse_alife_trader_abstract():
	m_money(0), m_trader_flags(FLAG_INFINITE_AMMO),
	m_community_index(-1), m_reputation(0), m_rank(0),
	m_unk1_u8(1), m_unk2_u8(0) {}

void cse_alife_trader_abstract::state_merge(xr_packet& packet, uint16_t version)
{
	if (version > CSE_VERSION_0x13) {
		if (version < CSE_VERSION_0x6c) {
			size_t n = packet.r_u32();
			xr_assert(n == 0);
		}
		if (version < CSE_VERSION_0x24)
			xr_not_implemented();
		if (version > CSE_VERSION_0x3e)
			packet.r_u32();
		if (version > CSE_VERSION_0x4b && version < CSE_VERSION_0x62)
			xr_not_implemented();
		packet.skip_sz();
		if (version > CSE_VERSION_0x4d)
			packet.r_u32();
		if (version > CSE_VERSION_0x51 && version < CSE_VERSION_0x60)
			xr_not_implemented();
		if (version > CSE_VERSION_0x5f)
			packet.skip_sz();
		if (version > CSE_VERSION_0x55)
			packet.r_s32();
		if (version > CSE_VERSION_0x56) {
			packet.r_s32();
			packet.r_s32();
		}
		if (version > CSE_VERSION_0x68)
			packet.skip_sz();

		if (version >= CSE_VERSION_0x80)
		{
			packet.r_u8();
			packet.r_u8();
		}
	}
}

void cse_alife_trader_abstract::state_read(xr_packet& packet, uint16_t size)
{
	uint16_t version = base()->version();
	if (version > CSE_VERSION_0x13) {
		if (version < CSE_VERSION_0x6c) {
			size_t n = packet.r_u32();
			xr_assert(n == 0);
		}
		if (version < CSE_VERSION_0x24)
			xr_not_implemented();
		if (version > CSE_VERSION_0x3e)
			packet.r_u32(m_money);
		if (version > CSE_VERSION_0x4b && version < CSE_VERSION_0x62)
			xr_not_implemented();
		if (version > CSE_VERSION_0x2e)
			packet.r_sz(m_specific_character);
		if (version > CSE_VERSION_0x4d)
			packet.r_u32(m_trader_flags);
		if (version > CSE_VERSION_0x51 && version < CSE_VERSION_0x60)
			xr_not_implemented();
		if (version > CSE_VERSION_0x5f)
			packet.r_sz(m_character_profile);
		if (version > CSE_VERSION_0x55)
			packet.r_s32(m_community_index);
		if (version > CSE_VERSION_0x56) {
			packet.r_s32(m_rank);
			packet.r_s32(m_reputation);
		}
		if (version > CSE_VERSION_0x68)
			packet.r_sz(m_character_name);

		if (version >= CSE_VERSION_0x80)
		{
			m_unk1_u8 = packet.r_u8();
			m_unk2_u8 = packet.r_u8();
		}
	}
}

void cse_alife_trader_abstract::state_write(xr_packet& packet)
{
	uint16_t version = base()->version();
	if (version < CSE_VERSION_0x6c)
		packet.w_u32(0);
	packet.w_u32(m_money);
	packet.w_sz(m_specific_character);
	packet.w_u32(m_trader_flags);
	packet.w_sz(m_character_profile);
	packet.w_s32(m_community_index);
	packet.w_s32(m_rank);
	packet.w_s32(m_reputation);
	if (version > CSE_VERSION_0x68)
		packet.w_sz(m_character_name);
	
	if (version >= CSE_VERSION_0x80)
	{
		packet.w_u8(m_unk1_u8);
		packet.w_u8(m_unk2_u8);
	}
}

void cse_alife_trader_abstract::update_read(xr_packet& packet) {}

void cse_alife_trader_abstract::update_write(xr_packet& packet) {}

////////////////////////////////////////////////////////////////////////////////

cse_abstract* cse_alife_trader::base() { return cse_abstract::base(); }

void cse_alife_trader::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object_visual::state_read(packet, size);
	cse_alife_trader_abstract::state_read(packet, size);
	if (m_version > CSE_VERSION_0x23 && m_version < CSE_VERSION_0x76)	// m_tOrdID
		packet.r_u32();
	if (m_version > CSE_VERSION_0x1d && m_version < CSE_VERSION_0x76) {	// m_tpOrderedArtefacts
		for (uint_fast32_t n = packet.r_u32(); n; --n) {
			packet.skip_sz();
			packet.r_u32();
			for (size_t k = packet.r_u32(); k; --k) {
				packet.skip_sz();
				packet.r_u32();
				packet.r_u32();
			}
		}
	}
	if (m_version > CSE_VERSION_0x1e && m_version < CSE_VERSION_0x76) {	// m_tSupplies
		for (uint_fast32_t n = packet.r_u32(); n; --n) {
			packet.skip_sz();
			packet.r_u32();
			packet.r_float();
			packet.r_float();
		}
	}
}

void cse_alife_trader::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
	cse_alife_trader_abstract::state_write(packet);
	if (m_version < CSE_VERSION_0x76) {
		packet.w_u32(UNDEF32);	// m_tOrdID
		packet.w_u32(0);	// m_tpOrderedArtefacts
		packet.w_u32(0);	// m_tSupplies
	}
}

void cse_alife_trader::update_read(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::update_read(packet);
}

void cse_alife_trader::update_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

cse_abstract* cse_alife_human_abstract::base() { return cse_abstract::base(); }

void cse_alife_human_abstract::state_merge(xr_packet& packet, uint16_t version)
{
	cse_alife_trader_abstract::state_merge(packet, version);
	cse_alife_monster_abstract::state_merge(packet, version);
}

void cse_alife_human_abstract::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_trader_abstract::state_read(packet, size);
	cse_alife_monster_abstract::state_read(packet, size);
	if (m_version > CSE_VERSION_0x13) {
		if (m_version < CSE_VERSION_0x6e) {
			for (uint_fast32_t n = packet.r_u32(); n; --n)	// m_tpPath
				packet.r_u32();
			packet.r_u32();				// m_baVisitedVertices
		}
		if (m_version > CSE_VERSION_0x23) {
			if (m_version < CSE_VERSION_0x6e)
				packet.skip_sz();		// m_caKnownCustomers
			if (m_version < CSE_VERSION_0x76) {
				for (uint_fast32_t n = packet.r_u32(); n; --n)	// m_tpKnownCustomers
					packet.r_u32();
			}
		}
		if (!packet.is_ini()) {
			packet.r_seq(packet.r_u32(), m_equipment_preferences);
			if (m_equipment_preferences.size() != 5)
				msg("wrong size equipment_preferences %" PRIuSIZET ", expected %d %s (%s)", m_equipment_preferences.size(), 5, name_replace().c_str(),
					name().c_str());
			packet.r_seq(packet.r_u32(), m_main_weapon_preferences);
			if (m_main_weapon_preferences.size() != 4)
				msg("wrong size main_weapon_preferences %" PRIuSIZET ", expected %d %s (%s)", m_main_weapon_preferences.size(), 4, name_replace().c_str(),
					name().c_str());
		}
	}
	if (m_version >= CSE_VERSION_0x6e && m_version < 0x70)
		packet.r_u16(m_smart_terrain_id);
}

void cse_alife_human_abstract::state_write(xr_packet& packet)
{
	cse_alife_trader_abstract::state_write(packet);
	cse_alife_monster_abstract::state_write(packet);
	if (m_version < CSE_VERSION_0x6e) {
		packet.w_u32(0);	// m_tpPath
		packet.w_u32(0);	// m_baVisitedVertices
		packet.w_sz("");	// m_caKnownCustomers
	}
	if (m_version < CSE_VERSION_0x76)
		packet.w_u32(0);	// m_tpKnownCustomers
	if (!packet.is_ini()) {
		packet.w_size_u32(m_equipment_preferences.size());
		packet.w_seq(m_equipment_preferences);
		packet.w_size_u32(m_main_weapon_preferences.size());
		packet.w_seq(m_main_weapon_preferences);
	}
	if (m_version >= CSE_VERSION_0x6e && m_version < 0x70)
		packet.w_u16(m_smart_terrain_id);
}

void cse_alife_human_abstract::update_read(xr_packet& packet)
{
	cse_alife_monster_abstract::update_read(packet);
}

void cse_alife_human_abstract::update_write(xr_packet& packet)
{
	cse_alife_monster_abstract::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_human_stalker::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_human_abstract::state_read(packet, size);
	if (m_version > CSE_VERSION_0x43)
		cse_ph_skeleton::state_read(packet, size);
	if (m_version > CSE_VERSION_0x5a && m_version < CSE_VERSION_0x6f)
		packet.r_bool();
}

void cse_alife_human_stalker::state_write(xr_packet& packet)
{
	cse_alife_human_abstract::state_write(packet);
	cse_ph_skeleton::state_write(packet);
	if (m_version < CSE_VERSION_0x6f)
		packet.w_bool(false);		// m_demo_mode?
}

void cse_alife_human_stalker::update_read(xr_packet& packet)
{
	cse_alife_human_abstract::update_read(packet);
	cse_ph_skeleton::update_read(packet);
	packet.r_sz(m_start_dialog);
}

void cse_alife_human_stalker::update_write(xr_packet& packet)
{
	cse_alife_human_abstract::update_write(packet);
	cse_ph_skeleton::update_write(packet);
	packet.w_sz(m_start_dialog);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_creature_actor::cse_alife_creature_actor():
	m_state(0xd20), m_radiation(0), m_weapon(0x6e), m_holder_id(UNDEF16), m_num_items(0)
{
	m_accel.set();
	m_velocity.set();
}

cse_abstract* cse_alife_creature_actor::base() { return cse_abstract::base(); }

void cse_alife_creature_actor::state_read(xr_packet& packet, uint16_t size)
{
	if (m_version < CSE_VERSION_0x15) {
		packet.r_u8(m_s_team);
		packet.r_u8(m_s_squad);
		packet.r_u8(m_s_group);
		if (m_version > CSE_VERSION_0x12)
			packet.r_float(m_health);
		if (m_version >= CSE_VERSION_0x03)
			cse_visual::visual_read(packet, m_version);
	} else {
		cse_alife_creature_abstract::state_read(packet, size);
		cse_alife_trader_abstract::state_read(packet, size);
		if (m_version < CSE_VERSION_0x20)
			cse_visual::visual_read(packet, m_version);
	}
	if (m_version > CSE_VERSION_0x5b)
		cse_ph_skeleton::state_read(packet, size);
	if (m_version > CSE_VERSION_0x58)
		packet.r_u16(m_holder_id);
}

void cse_alife_creature_actor::state_write(xr_packet& packet)
{
	cse_alife_creature_abstract::state_write(packet);
	cse_alife_trader_abstract::state_write(packet);
	cse_ph_skeleton::state_write(packet);
	packet.w_u16(m_holder_id);
}

void cse_alife_creature_actor::update_read(xr_packet& packet)
{
	cse_alife_creature_abstract::update_read(packet);
	packet.r_u16(m_state);
	packet.r_sdir(m_accel);
	packet.r_sdir(m_velocity);
	packet.r_float(m_radiation);
	packet.r_u8(m_weapon);
	packet.r_u16(m_num_items);
	xr_assert(m_num_items == 0);
}

void cse_alife_creature_actor::update_write(xr_packet& packet)
{
	cse_alife_creature_abstract::update_write(packet);
	packet.w_u16(m_state);
	packet.w_sdir(m_accel);
	packet.w_sdir(m_velocity);
	packet.w_float(m_radiation);
	packet.w_u8(m_weapon);
	packet.w_u16(m_num_items);
	xr_assert(m_num_items == 0);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_space_restrictor::cse_alife_space_restrictor():
	m_space_restrictor_type(DEFAULT_RESTRICTOR_TYPE_NONE) {}

void cse_alife_space_restrictor::state_merge(xr_packet& packet, uint16_t version)
{
	cse_alife_dynamic_object::state_merge(packet, version);
	cse_shape::cform_merge(packet);
	if (version > CSE_VERSION_0x4a)
		packet.r_u8();
}

void cse_alife_space_restrictor::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object::state_read(packet, size);
	cse_shape::cform_read(packet);
	if (m_version > CSE_VERSION_0x4a)
		packet.r_u8(m_space_restrictor_type);
}

void cse_alife_space_restrictor::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object::state_write(packet);
	cse_shape::cform_write(packet);
	packet.w_s8(m_space_restrictor_type);
}

void cse_alife_space_restrictor::update_read(xr_packet& packet) {}

void cse_alife_space_restrictor::update_write(xr_packet& packet) {}

cse_shape* cse_alife_space_restrictor::shape()
{
	return static_cast<cse_shape*>(this);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_smart_zone::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_space_restrictor::state_read(packet, size);
}

void cse_alife_smart_zone::state_write(xr_packet& packet)
{
	cse_alife_space_restrictor::state_write(packet);
}

void cse_alife_smart_zone::update_read(xr_packet& packet) {}

void cse_alife_smart_zone::update_write(xr_packet& packet) {}

////////////////////////////////////////////////////////////////////////////////

cse_alife_team_base_zone::cse_alife_team_base_zone(): m_team(0) {}

void cse_alife_team_base_zone::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_space_restrictor::state_read(packet, size);
	packet.r_u8(m_team);
}

void cse_alife_team_base_zone::state_write(xr_packet& packet)
{
	cse_alife_space_restrictor::state_write(packet);
	packet.w_u8(m_team);
}

void cse_alife_team_base_zone::update_read(xr_packet& packet) {}

void cse_alife_team_base_zone::update_write(xr_packet& packet) {}

////////////////////////////////////////////////////////////////////////////////

cse_alife_level_changer::cse_alife_level_changer():
	m_next_graph_id(UNDEF16), m_next_node_id(UNDEF32),
	m_silent_mode(false)
{
	m_next_position.set();
	m_angles.set();
}

void cse_alife_level_changer::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_space_restrictor::state_read(packet, size);
	if (m_version < CSE_VERSION_0x22) {
		packet.r_s32();
		packet.r_s32();
	} else {
		packet.r_u16(m_next_graph_id);
		packet.r_u32(m_next_node_id);
		packet.r_float(m_next_position.x);
		packet.r_float(m_next_position.y);
		packet.r_float(m_next_position.z);
		if (m_version > CSE_VERSION_0x35) {
			packet.r_vec3(m_angles);
		} else {
			m_angles.x = 0;
			packet.r_float(m_angles.y);
			m_angles.z = 0;
		}
	}
	packet.r_sz(m_level_to_change);
	packet.r_sz(m_level_point_to_change);
	if (m_version > CSE_VERSION_0x74)
		packet.r_bool(m_silent_mode);
}

void cse_alife_level_changer::state_write(xr_packet& packet)
{
	cse_alife_space_restrictor::state_write(packet);
	packet.w_u16(m_next_graph_id);
	packet.w_u32(m_next_node_id);
	packet.w_float(m_next_position.x);
	packet.w_float(m_next_position.y);
	packet.w_float(m_next_position.z);
	packet.w_vec3(m_angles);
	packet.w_sz(m_level_to_change);
	packet.w_sz(m_level_point_to_change);
	if (m_version > CSE_VERSION_0x74)
		packet.w_bool(m_silent_mode);
}

void cse_alife_level_changer::update_read(xr_packet& packet) {}

void cse_alife_level_changer::update_write(xr_packet& packet) {}

////////////////////////////////////////////////////////////////////////////////

cse_alife_custom_zone::cse_alife_custom_zone():
	m_max_power(0), m_owner_id(UNDEF32),
	m_enabled_time(0), m_disabled_time(0), m_start_time_shift(0) {}

void cse_alife_custom_zone::state_merge(xr_packet& packet, uint16_t version)
{
	cse_alife_space_restrictor::state_merge(packet, version);
	packet.r_float();
	if (version < CSE_VERSION_0x71) {
		packet.r_float();
		packet.r_u32();
	}
	if (version > CSE_VERSION_0x42 && version < CSE_VERSION_0x76)
		packet.r_u32();
	if (version > CSE_VERSION_0x66)
		packet.r_u32();
	if (version > CSE_VERSION_0x69) {
		packet.r_u32();
		packet.r_u32();
	}
	if (version > CSE_VERSION_0x6a)
		packet.r_u32();
}

void cse_alife_custom_zone::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_space_restrictor::state_read(packet, size);
	packet.r_float(m_max_power);
	if (m_version < CSE_VERSION_0x71) {
		packet.r_float();
		packet.r_u32();
	}
	if (m_version > CSE_VERSION_0x42 && m_version < CSE_VERSION_0x76)
		packet.r_u32();
	if (m_version > CSE_VERSION_0x66)
		packet.r_u32(m_owner_id);
	if (m_version > CSE_VERSION_0x69) {
		packet.r_u32(m_enabled_time);
		packet.r_u32(m_disabled_time);
	}
	if (m_version > CSE_VERSION_0x6a)
		packet.r_u32(m_start_time_shift);
}

void cse_alife_custom_zone::state_write(xr_packet& packet)
{
	cse_alife_space_restrictor::state_write(packet);
	packet.w_float(m_max_power);
	if (m_version < CSE_VERSION_0x71) {
		packet.w_float(0);		// FIXME
		packet.w_u32(1000);		// FIXME
	}
	if (m_version < CSE_VERSION_0x76)
		packet.w_u32(0);		// FIXME
	if (m_version > CSE_VERSION_0x66)
		packet.w_u32(m_owner_id);
	if (m_version > CSE_VERSION_0x69) {
		packet.w_u32(m_enabled_time);
		packet.w_u32(m_disabled_time);
	}
	if (m_version > CSE_VERSION_0x6a)
		packet.w_u32(m_start_time_shift);
}

void cse_alife_custom_zone::update_read(xr_packet& packet)
{
	cse_alife_space_restrictor::update_read(packet);
	if (m_version > CSE_VERSION_0x66 && (m_version < CSE_VERSION_0x76 ||
			(m_version == CSE_VERSION_0x76 && m_script_version < 6))) {
		packet.r_u32(m_owner_id);
	}
}

void cse_alife_custom_zone::update_write(xr_packet& packet)
{
	cse_alife_space_restrictor::update_write(packet);
	if (m_version > CSE_VERSION_0x66 && (m_version < CSE_VERSION_0x76 ||
			(m_version == CSE_VERSION_0x76 && m_script_version < 6))) {
		packet.w_u32(m_owner_id);
	}
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_anomalous_zone::cse_alife_anomalous_zone(): m_offline_interactive_radius(0),
	m_artefact_position_offset(0), m_artefact_spawn_count(0) {}

void cse_alife_anomalous_zone::state_merge(xr_packet& packet, uint16_t version)
{
	cse_alife_custom_zone::state_merge(packet, version);
	if (version > CSE_VERSION_0x15) {
		packet.r_float(m_offline_interactive_radius);	// do we need to pick it?
		if (version < CSE_VERSION_0x71) {
			packet.r_float();
			for (uint_fast32_t n = packet.r_u16(); n; --n) {
				packet.skip_sz();
				if (version > CSE_VERSION_0x1a)
					packet.r_float();
				else
					packet.r_u32();
			}
		}
	}
	if (version > CSE_VERSION_0x19) {
		packet.r_u16(m_artefact_spawn_count);
		packet.r_u32(m_artefact_position_offset);
	}
}

void cse_alife_anomalous_zone::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_custom_zone::state_read(packet, size);
	if (m_version > CSE_VERSION_0x15) {
		packet.r_float(m_offline_interactive_radius);
		if (m_version < CSE_VERSION_0x71) {
			packet.r_float();
			for (uint_fast32_t n = packet.r_u16(); n; --n) {
				packet.skip_sz();
				if (m_version > CSE_VERSION_0x1a)
					packet.r_float();
				else
					packet.r_u32();
			}
		}
	}
	if (m_version > CSE_VERSION_0x19) {
		packet.r_u16(m_artefact_spawn_count);
		packet.r_u32(m_artefact_position_offset);
	}
	if (m_version > CSE_VERSION_0x1b && m_version < CSE_VERSION_0x43)
		packet.r_u32();
	if (m_version > CSE_VERSION_0x26 && m_version < CSE_VERSION_0x71)
		packet.r_float();
	if (m_version > CSE_VERSION_0x4e && m_version < CSE_VERSION_0x71) {
		packet.r_float();
		packet.r_float();
		packet.r_float();
	}
	if (m_version == CSE_VERSION_0x66)
		packet.r_u32();
}

void cse_alife_anomalous_zone::state_write(xr_packet& packet)
{
	cse_alife_custom_zone::state_write(packet);
	packet.w_float(m_offline_interactive_radius);
	if (m_version < CSE_VERSION_0x71) {
		// FIXME
		packet.w_float(0);
		size_t n = 0;
		packet.w_size_u16(n);
		for (; n; --n) {
			packet.w_sz("");
			packet.w_float(0);	// FIXME
		}
	}
	packet.w_u16(m_artefact_spawn_count);
	packet.w_u32(m_artefact_position_offset);
	if (m_version < CSE_VERSION_0x71) {
		packet.w_float(0);

		packet.w_float(0);
		packet.w_float(0);
		packet.w_float(0);
	}
	if (m_version == CSE_VERSION_0x66)
		packet.w_u32(0);		// FIXME
}

void cse_alife_anomalous_zone::update_read(xr_packet& packet)
{
	cse_alife_custom_zone::update_read(packet);
}

void cse_alife_anomalous_zone::update_write(xr_packet& packet)
{
	cse_alife_custom_zone::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_zone_visual::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_anomalous_zone::state_read(packet, size);
	cse_visual::visual_read(packet, m_version);
	packet.r_sz(cse_visual::m_startup_animation);
	packet.r_sz(m_attack_animation);
}

void cse_alife_zone_visual::state_write(xr_packet& packet)
{
	cse_alife_anomalous_zone::state_write(packet);
	cse_visual::visual_write(packet, m_version);
	packet.w_sz(cse_visual::m_startup_animation);
	packet.w_sz(m_attack_animation);
}

void cse_alife_zone_visual::update_read(xr_packet& packet)
{
	cse_alife_anomalous_zone::update_read(packet);
}

void cse_alife_zone_visual::update_write(xr_packet& packet)
{
	cse_alife_anomalous_zone::update_write(packet);
}

cse_visual* cse_alife_zone_visual::visual()
{
	return static_cast<cse_visual*>(this);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_torrid_zone::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_custom_zone::state_read(packet, size);
	cse_motion::motion_read(packet);
}

void cse_alife_torrid_zone::state_write(xr_packet& packet)
{
	cse_alife_custom_zone::state_write(packet);
	cse_motion::motion_write(packet);
}

void cse_alife_torrid_zone::update_read(xr_packet& packet)
{
	cse_alife_custom_zone::update_read(packet);
}

void cse_alife_torrid_zone::update_write(xr_packet& packet)
{
	cse_alife_custom_zone::update_write(packet);
}

cse_motion* cse_alife_torrid_zone::motion()
{
	return static_cast<cse_motion*>(this);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_online_offline_group::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object::state_read(packet, size);
	packet.r_seq(packet.r_u32(), m_members);
}

void cse_alife_online_offline_group::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object::state_write(packet);
	packet.w_size_u32(m_members.size());
	packet.w_seq(m_members);
}

void cse_alife_online_offline_group::update_read(xr_packet& packet)
{
	cse_alife_dynamic_object::update_read(packet);
}

void cse_alife_online_offline_group::update_write(xr_packet& packet)
{
	cse_alife_dynamic_object::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_inventory_item::cse_alife_inventory_item():
	m_condition(1.f), m_timestamp(0), m_num_items(FLAG_NO_POSITION)
{
	std::memset(&m_state, 0, sizeof(m_state));
}

cse_alife_inventory_item::~cse_alife_inventory_item() {}

void cse_alife_inventory_item::state_read(xr_packet& packet, uint16_t size)
{
	uint32_t version = base()->version();
	if (version > CSE_VERSION_0x34)
		packet.r_float(m_condition);
	if (version > CSE_VERSION_0x7b) {
		if (size_t n = packet.r_u32()) {
			m_upgrades.reserve(n);
			for (; n > 0; --n)
				m_upgrades.push_back(packet.skip_sz());
		}
	}
	m_state.position = base()->position();
}

void cse_alife_inventory_item::state_write(xr_packet& packet)
{
	packet.w_float(m_condition);
	if (base()->version() > CSE_VERSION_0x7b) {
		packet.w_size_u32(m_upgrades.size());
		for (std::vector<std::string>::const_iterator it = m_upgrades.begin(),
				end = m_upgrades.end(); it != end; ++it) {
			packet.w_sz(*it);
		}
	}
	m_state.position = base()->position();
}

void cse_alife_inventory_item::update_read(xr_packet& packet)
{
	if (base()->version() >= CSE_VERSION_0x7a) {
		xr_assert(base()->version() <= CSE_VERSION_COP);
		m_num_items = packet.r_u8();
		if (m_num_items) {
			packet.r_vec3(m_state.force);
			packet.r_vec3(m_state.torque);
			packet.r_vec3(m_state.position);
			base()->position() = m_state.position;
			packet.r_quat(m_state.quaternion);
			uint_fast16_t flags = m_num_items >> 5;
			m_state.enabled = (flags & 1) != 0;
			if (flags & 0x2)
				m_state.angular_vel.set(0, 0, 0);
			else
				packet.r_vec3(m_state.angular_vel);
			if (flags & 0x4)
				m_state.linear_vel.set(0, 0, 0);
			else
				packet.r_vec3(m_state.linear_vel);
			if (!packet.r_eof())
				packet.r_bool();
		}
	} else if (base()->script_version() > 5) {
		xr_assert(base()->version() == CSE_VERSION_SOC);
		m_num_items = packet.r_u8();
		if (m_num_items) {
			packet.r_vec3(m_state.position);
			base()->position() = m_state.position;
			packet.r_float_q8(m_state.quaternion.x);
			packet.r_float_q8(m_state.quaternion.y);
			packet.r_float_q8(m_state.quaternion.z);
			packet.r_float_q8(m_state.quaternion.w);
			uint_fast16_t flags = m_num_items >> 5;
			m_state.enabled = (flags & 1) != 0;
			if (flags & 0x2) {
				m_state.angular_vel.set(0, 0, 0);
			} else {
				packet.r_float_q8(m_state.angular_vel.x, 0, float(2*M_PI));
				packet.r_float_q8(m_state.angular_vel.y, 0, float(2*M_PI));
				packet.r_float_q8(m_state.angular_vel.z, 0, float(2*M_PI));
			}
			if (flags & 0x4) {
				m_state.linear_vel.set(0, 0, 0);
			} else {
				packet.r_float_q8(m_state.linear_vel.x, -32.f, +32.f);
				packet.r_float_q8(m_state.linear_vel.y, -32.f, +32.f);
				packet.r_float_q8(m_state.linear_vel.z, -32.f, +32.f);
			}
		}
	} else {
		packet.r_float(m_condition);
		packet.r_u32(m_timestamp);
		packet.r_u16(m_num_items);
		if ((m_num_items & FLAG_NO_POSITION) == 0) {
			packet.r_vec3(m_state.position);
			base()->position() = m_state.position;
		}
		if (m_num_items & ~FLAG_NO_POSITION) {
			packet.r_bool(m_state.enabled);
			packet.r_vec3(m_state.angular_vel);
			packet.r_vec3(m_state.linear_vel);
			packet.r_vec3(m_state.force);
			packet.r_vec3(m_state.torque);
			packet.r_quat(m_state.quaternion);
		}
	}
}

void cse_alife_inventory_item::update_write(xr_packet& packet)
{
	if (base()->version() >= CSE_VERSION_0x7a) {
		xr_assert(base()->version() <= CSE_VERSION_COP);
		if (m_num_items & ~FLAG_NO_POSITION) {
			uint8_t num_items = uint8_t(m_num_items & 0x1f);
			if (m_state.enabled)
				num_items |= 0x20;
			if (m_state.angular_vel.square_magnitude() < 1e-7f)
				num_items |= 0x40;
			if (m_state.linear_vel.square_magnitude() < 1e-7f)
				num_items |= 0x80;
			packet.w_u8(num_items);
			packet.w_vec3(m_state.force);
			packet.w_vec3(m_state.torque);
			packet.w_vec3(m_state.position);
			packet.w_quat(m_state.quaternion);
			if ((num_items & 0x40) == 0)
				packet.w_vec3(m_state.angular_vel);
			if ((num_items & 0x80) == 0)
				packet.w_vec3(m_state.linear_vel);
			packet.w_bool(true);
		} else {
			packet.w_u8(0);
		}
	} else if (base()->script_version() > 5) {
		xr_assert(base()->version() == CSE_VERSION_SOC);
		if (m_num_items & ~FLAG_NO_POSITION) {
			uint8_t num_items = uint8_t(m_num_items & 0x1f);
			if (m_state.enabled)
				num_items |= 0x20;
			if (m_state.angular_vel.square_magnitude() < 1e-7f)
				num_items |= 0x40;
			if (m_state.linear_vel.square_magnitude() < 1e-7f)
				num_items |= 0x80;
			packet.w_u8(num_items);
			packet.w_vec3(m_state.position);
			// FIXME: check if it really exist in xrGame.dll
			base()->position() = m_state.position;
			packet.w_float_q8(m_state.quaternion.x);
			packet.w_float_q8(m_state.quaternion.y);
			packet.w_float_q8(m_state.quaternion.z);
			packet.w_float_q8(m_state.quaternion.w);
			if ((num_items & 0x40) == 0) {
				packet.w_float_q8(m_state.angular_vel.x, 0, float(2*M_PI));
				packet.w_float_q8(m_state.angular_vel.y, 0, float(2*M_PI));
				packet.w_float_q8(m_state.angular_vel.z, 0, float(2*M_PI));
			}
			if ((num_items & 0x80) == 0) {
				packet.w_float_q8(m_state.linear_vel.x, -32.f, +32.f);
				packet.w_float_q8(m_state.linear_vel.y, -32.f, +32.f);
				packet.w_float_q8(m_state.linear_vel.z, -32.f, +32.f);
			}
		} else {
			packet.w_u8(0);
		}
	} else {
		packet.w_float(m_condition);
		packet.w_u32(m_timestamp);
		packet.w_u16(m_num_items);
		if ((m_num_items & FLAG_NO_POSITION) == 0) {
			packet.w_vec3(m_state.position);
			base()->position() = m_state.position;
		}
		if (m_num_items & ~FLAG_NO_POSITION) {
			packet.w_bool(m_state.enabled);
			packet.w_vec3(m_state.angular_vel);
			packet.w_vec3(m_state.linear_vel);
			packet.w_vec3(m_state.force);
			packet.w_vec3(m_state.torque);
			packet.w_quat(m_state.quaternion);
		}
	}
}

////////////////////////////////////////////////////////////////////////////////

cse_abstract* cse_alife_item::base() { return cse_abstract::base(); }

void cse_alife_item::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_dynamic_object_visual::state_read(packet, size);
	if (m_clsid == "W_BINOC" && m_version < CSE_VERSION_0x25) {
		packet.r_s16();
		packet.r_s16();
		packet.r_s8();
	}
	cse_alife_inventory_item::state_read(packet, size);
}

void cse_alife_item::state_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::state_write(packet);
	cse_alife_inventory_item::state_write(packet);
}

void cse_alife_item::update_read(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::update_read(packet);
	cse_alife_inventory_item::update_read(packet);
}

void cse_alife_item::update_write(xr_packet& packet)
{
	cse_alife_dynamic_object_visual::update_write(packet);
	cse_alife_inventory_item::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_item_torch::cse_alife_item_torch():
	m_active(false), m_nightvision_active(false), m_unknown(false) {}

void cse_alife_item_torch::state_read(xr_packet& packet, uint16_t size)
{
	if (m_version > CSE_VERSION_0x14)
		cse_alife_item::state_read(packet, size);

	if (m_version >= CSE_VERSION_0x28 && m_version <= CSE_VERSION_0x2e) {
		packet.r_u32();
		packet.skip_sz();
		packet.skip_sz();
		packet.r_float();
		packet.r_angle8();
		packet.r_float();

		if (m_version > 0x28u ) {
			packet.skip_sz();
			packet.r_float();
		}
		if (m_version > 0x29u )
			packet.r_u16();
	}
}

void cse_alife_item_torch::state_write(xr_packet& packet)
{
	cse_alife_item::state_write(packet);
}

void cse_alife_item_torch::update_read(xr_packet& packet)
{
	cse_alife_item::update_read(packet);
	uint8_t flags = packet.r_u8();
	m_active = (flags & FL_TORCH_ACTIVE) != 0;
	m_nightvision_active = (flags & FL_NIGHT_VISION_ACTIVE) != 0;
	m_unknown = (flags & FL_UNKNOWN) != 0;
}

void cse_alife_item_torch::update_write(xr_packet& packet)
{
	cse_alife_item::update_write(packet);
	uint8_t flags = 0;
	if (m_active)
		flags |= FL_TORCH_ACTIVE;
	if (m_nightvision_active)
		flags |= FL_NIGHT_VISION_ACTIVE;
	if (m_unknown)
		flags |= FL_UNKNOWN;
	packet.w_u8(flags);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_item_detector::state_read(xr_packet& packet, uint16_t size)
{
	if (m_version > CSE_VERSION_0x14)
		cse_alife_item::state_read(packet, size);
}

void cse_alife_item_detector::state_write(xr_packet& packet)
{
	cse_alife_item::state_write(packet);
}

void cse_alife_item_detector::update_read(xr_packet& packet)
{
	cse_alife_item::update_read(packet);
}

void cse_alife_item_detector::update_write(xr_packet& packet)
{
	cse_alife_item::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_item_artefact::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_item::state_read(packet, size);
}

void cse_alife_item_artefact::state_write(xr_packet& packet)
{
	cse_alife_item::state_write(packet);
}

void cse_alife_item_artefact::update_read(xr_packet& packet)
{
	cse_alife_item::update_read(packet);
}

void cse_alife_item_artefact::update_write(xr_packet& packet)
{
	cse_alife_item::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_item_grenade::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_item::state_read(packet, size);
}

void cse_alife_item_grenade::state_write(xr_packet& packet)
{
	cse_alife_item::state_write(packet);
}

void cse_alife_item_grenade::update_read(xr_packet& packet)
{
	cse_alife_item::update_read(packet);
}

void cse_alife_item_grenade::update_write(xr_packet& packet)
{
	cse_alife_item::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_item_explosive::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_item::state_read(packet, size);
}

void cse_alife_item_explosive::state_write(xr_packet& packet)
{
	cse_alife_item::state_write(packet);
}

void cse_alife_item_explosive::update_read(xr_packet& packet)
{
	cse_alife_item::update_read(packet);
}

void cse_alife_item_explosive::update_write(xr_packet& packet)
{
	cse_alife_item::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_item_bolt::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_item::state_read(packet, size);
}

void cse_alife_item_bolt::state_write(xr_packet& packet)
{
	cse_alife_item::state_write(packet);
}

void cse_alife_item_bolt::update_read(xr_packet& packet)
{
	cse_alife_item::update_read(packet);
}

void cse_alife_item_bolt::update_write(xr_packet& packet)
{
	cse_alife_item::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_item_custom_outfit::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_item::state_read(packet, size);
}

void cse_alife_item_custom_outfit::state_write(xr_packet& packet)
{
	cse_alife_item::state_write(packet);
}

void cse_alife_item_custom_outfit::update_read(xr_packet& packet)
{
	cse_alife_item::update_read(packet);
	if (m_version >= CSE_VERSION_0x76 && m_script_version > 5)
		packet.r_float_q8(m_condition);
}

void cse_alife_item_custom_outfit::update_write(xr_packet& packet)
{
	cse_alife_item::update_write(packet);
	if (m_version >= CSE_VERSION_0x76 && m_script_version > 5)
		packet.w_float_q8(m_condition);
}


////////////////////////////////////////////////////////////////////////////////

void cse_alife_item_helmet::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_item::state_read(packet, size);
}

void cse_alife_item_helmet::state_write(xr_packet& packet)
{
	cse_alife_item::state_write(packet);
}

void cse_alife_item_helmet::update_read(xr_packet& packet)
{
	cse_alife_item::update_read(packet);
	if (m_version >= CSE_VERSION_0x76 && m_script_version > 5)
		packet.r_float_q8(m_condition);
}

void cse_alife_item_helmet::update_write(xr_packet& packet)
{
	cse_alife_item::update_write(packet);
	if (m_version >= CSE_VERSION_0x76 && m_script_version > 5)
		packet.w_float_q8(m_condition);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_item_pda::cse_alife_item_pda(): m_original_owner(UNDEF16) {}

void cse_alife_item_pda::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_item::state_read(packet, size);
	if (m_version > CSE_VERSION_0x3a)
		packet.r_u16(m_original_owner);
	if (m_version > CSE_VERSION_0x59) {
		if (m_version < CSE_VERSION_0x62) {
			packet.r_s32();
			packet.r_s32();
			m_specific_character.clear();
			m_info_portion.clear();
		} else {
			packet.r_sz(m_specific_character);
			packet.r_sz(m_info_portion);
		}
	}
}

void cse_alife_item_pda::state_write(xr_packet& packet)
{
	cse_alife_item::state_write(packet);
	packet.w_u16(m_original_owner);
	packet.w_sz(m_specific_character);
	packet.w_sz(m_info_portion);
}

void cse_alife_item_pda::update_read(xr_packet& packet)
{
	cse_alife_item::update_read(packet);
}

void cse_alife_item_pda::update_write(xr_packet& packet)
{
	cse_alife_item::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_item_document::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_item::state_read(packet, size);
	if (m_version < CSE_VERSION_0x62)
		packet.r_s16();
	else
		packet.r_sz(m_doc);
}

void cse_alife_item_document::state_write(xr_packet& packet)
{
	cse_alife_item::state_write(packet);
	packet.w_sz(m_doc);
}

void cse_alife_item_document::update_read(xr_packet& packet)
{
	cse_alife_item::update_read(packet);
}

void cse_alife_item_document::update_write(xr_packet& packet)
{
	cse_alife_item::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_item_ammo::cse_alife_item_ammo(): m_elapsed(30) {}

void cse_alife_item_ammo::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_item::state_read(packet, size);
	packet.r_u16(m_elapsed);
}

void cse_alife_item_ammo::state_write(xr_packet& packet)
{
	cse_alife_item::state_write(packet);
	packet.w_u16(m_elapsed);
}

void cse_alife_item_ammo::update_read(xr_packet& packet)
{
	cse_alife_item::update_read(packet);
	packet.r_u16(m_elapsed);
}

void cse_alife_item_ammo::update_write(xr_packet& packet)
{
	cse_alife_item::update_write(packet);
	packet.w_u16(m_elapsed);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_item_weapon::cse_alife_item_weapon():
	m_scope(ADDON_DISABLED), m_silencer(ADDON_DISABLED), m_grenade_launcher(ADDON_DISABLED),
	m_flags(0), m_state(0), m_ammo_type(0), m_ammo_current(30), m_ammo_elapsed(30),
	m_addon_flags(0), m_zoom(0), m_cs_unk1_u8(0) {}

void cse_alife_item_weapon::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_item::state_read(packet, size);
	packet.r_u16(m_ammo_current);
	packet.r_u16(m_ammo_elapsed);
	packet.r_u8(m_state);
	if (m_version >= CSE_VERSION_0x28)
		packet.r_u8(m_addon_flags);
	if (m_version > CSE_VERSION_0x2e)
		packet.r_u8(m_ammo_type);
	if (m_version > CSE_VERSION_0x7a)
		packet.r_u8(m_cs_unk1_u8);
}

void cse_alife_item_weapon::state_write(xr_packet& packet)
{
	cse_alife_item::state_write(packet);
	packet.w_u16(m_ammo_current);
	packet.w_u16(m_ammo_elapsed);
	packet.w_u8(m_state);
	packet.w_u8(m_addon_flags);
	packet.w_u8(m_ammo_type);
	if (m_version > CSE_VERSION_0x7a)
		packet.w_u8(m_cs_unk1_u8);
}

void cse_alife_item_weapon::update_read(xr_packet& packet)
{
	cse_alife_item::update_read(packet);
	if (m_version >= CSE_VERSION_SOC && m_script_version > 5)
		packet.r_float_q8(m_condition);
	packet.r_u8(m_flags);
	packet.r_u16(m_ammo_elapsed);
	packet.r_u8(m_addon_flags);
	packet.r_u8(m_ammo_type);
	packet.r_u8(m_state);
// FIXME: mar_wpn_svd from 3120 has non-zero m_bZoom (or whatever it is?)
//	packet.r_bool(m_zoom);
	packet.r_u8(m_zoom);
}

void cse_alife_item_weapon::update_write(xr_packet& packet)
{
	cse_alife_item::update_write(packet);
	if (m_version >= CSE_VERSION_SOC && m_script_version > 5)
		packet.w_float_q8(m_condition);
	packet.w_u8(m_flags);
	packet.w_u16(m_ammo_elapsed);
	packet.w_u8(m_addon_flags);
	packet.w_u8(m_ammo_type);
	packet.w_u8(m_state);
// see above
//	packet.w_bool(m_zoom);
	packet.w_u8(m_zoom);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_item_weapon_magazined::cse_alife_item_weapon_magazined(): m_cur_fire_mode(0) {}

void cse_alife_item_weapon_magazined::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_item_weapon::state_read(packet, size);
}

void cse_alife_item_weapon_magazined::state_write(xr_packet& packet)
{
	cse_alife_item_weapon::state_write(packet);
}

void cse_alife_item_weapon_magazined::update_read(xr_packet& packet)
{
	cse_alife_item_weapon::update_read(packet);
	packet.r_u8(m_cur_fire_mode);
}

void cse_alife_item_weapon_magazined::update_write(xr_packet& packet)
{
	cse_alife_item_weapon::update_write(packet);
	packet.w_u8(m_cur_fire_mode);
}

////////////////////////////////////////////////////////////////////////////////

cse_alife_item_weapon_magazined_w_gl::cse_alife_item_weapon_magazined_w_gl(): m_grenade_mode(false) {}

void cse_alife_item_weapon_magazined_w_gl::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_item_weapon_magazined::state_read(packet, size);
}

void cse_alife_item_weapon_magazined_w_gl::state_write(xr_packet& packet)
{
	cse_alife_item_weapon_magazined::state_write(packet);
}

void cse_alife_item_weapon_magazined_w_gl::update_read(xr_packet& packet)
{
	packet.r_bool(m_grenade_mode);
	cse_alife_item_weapon_magazined::update_read(packet);
}

void cse_alife_item_weapon_magazined_w_gl::update_write(xr_packet& packet)
{
	packet.w_bool(m_grenade_mode);
	cse_alife_item_weapon_magazined::update_write(packet);
}

////////////////////////////////////////////////////////////////////////////////

void cse_alife_item_weapon_shotgun::state_read(xr_packet& packet, uint16_t size)
{
	cse_alife_item_weapon_magazined::state_read(packet, size);
}

void cse_alife_item_weapon_shotgun::state_write(xr_packet& packet)
{
	cse_alife_item_weapon_magazined::state_write(packet);
}

void cse_alife_item_weapon_shotgun::update_read(xr_packet& packet)
{
	cse_alife_item_weapon_magazined::update_read(packet);
	packet.r_seq(packet.r_u8(), m_ammo_ids);
}

void cse_alife_item_weapon_shotgun::update_write(xr_packet& packet)
{
	cse_alife_item_weapon_magazined::update_write(packet);
	packet.w_size_u8(m_ammo_ids.size());
	packet.w_seq(m_ammo_ids);
}
