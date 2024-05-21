#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_ENTITY_H__
#define __XR_ENTITY_H__

#include <string>
#include <vector>
#include <stack>
#include "xr_shape.h"
#include "xr_color.h"
#include "xr_quaternion.h"
#include "xr_packet.h"
#include "xr_clsid.h"

namespace xray_re {

enum {
	M_UPDATE	= 0,
	M_SPAWN		= 1,
};

enum {
	CSE_VERSION_0x01	= 0x01,
	CSE_VERSION_0x03	= 0x03,
	CSE_VERSION_0x04	= 0x04,
	CSE_VERSION_0x05	= 0x05,
	CSE_VERSION_0x08	= 0x08,
	CSE_VERSION_0x09	= 0x09,
	CSE_VERSION_0x0a	= 0x0a,
	CSE_VERSION_0x0b	= 0x0b,
	CSE_VERSION_0x0c	= 0x0c,
	CSE_VERSION_0x0e	= 0x0e,
	CSE_VERSION_0x10	= 0x10,
	CSE_VERSION_0x11	= 0x11,
	CSE_VERSION_0x12	= 0x12,
	CSE_VERSION_0x13	= 0x13,
	CSE_VERSION_0x14	= 0x14,
	CSE_VERSION_0x15	= 0x15,
	CSE_VERSION_0x16	= 0x16,
	CSE_VERSION_0x17	= 0x17,
	CSE_VERSION_0x18	= 0x18,
	CSE_VERSION_0x19	= 0x19,
	CSE_VERSION_0x1a	= 0x1a,
	CSE_VERSION_0x1b	= 0x1b,
	CSE_VERSION_0x1c	= 0x1c,
	CSE_VERSION_0x1d	= 0x1d,
	CSE_VERSION_0x1e	= 0x1e,
	CSE_VERSION_0x1f	= 0x1f,
	CSE_VERSION_0x20	= 0x20,
	CSE_VERSION_0x21	= 0x21,
	CSE_VERSION_0x22	= 0x22,
	CSE_VERSION_0x23	= 0x23,
	CSE_VERSION_0x24	= 0x24,
	CSE_VERSION_0x25	= 0x25,
	CSE_VERSION_0x26	= 0x26,
	CSE_VERSION_0x27	= 0x27,
	CSE_VERSION_0x28	= 0x28,
	CSE_VERSION_0x2a	= 0x2a,
	CSE_VERSION_0x2b	= 0x2b,
	CSE_VERSION_0x2c	= 0x2c,
	CSE_VERSION_0x2e	= 0x2e,//build 1829
	CSE_VERSION_0x31	= 0x31,
	CSE_VERSION_0x34	= 0x34,
	CSE_VERSION_0x35	= 0x35,
	CSE_VERSION_0x37	= 0x37,
	CSE_VERSION_0x38	= 0x38,
	CSE_VERSION_0x39	= 0x39,
	CSE_VERSION_0x3a	= 0x3a,
	CSE_VERSION_0x3c	= 0x3c,
	CSE_VERSION_0x3d	= 0x3d,
	CSE_VERSION_0x3e	= 0x3e,
	CSE_VERSION_0x40	= 0x40,
	CSE_VERSION_0x41	= 0x41,
	CSE_VERSION_0x42	= 0x42,
	CSE_VERSION_0x43	= 0x43,
	CSE_VERSION_0x44	= 0x44,
	CSE_VERSION_0x45	= 0x45,
	CSE_VERSION_0x46	= 0x46,
	CSE_VERSION_0x48	= 0x48,
	CSE_VERSION_0x49	= 0x49,
	CSE_VERSION_0x4a	= 0x4a,
	CSE_VERSION_0x4b	= 0x4b,
	CSE_VERSION_0x4e	= 0x4e,
	CSE_VERSION_0x4d	= 0x4d,
	CSE_VERSION_0x4f	= 0x4f,
	CSE_VERSION_0x51	= 0x51,
	CSE_VERSION_0x52	= 0x52,
	CSE_VERSION_0x53	= 0x53,
	CSE_VERSION_0x54	= 0x54,
	CSE_VERSION_0x55	= 0x55,
	CSE_VERSION_0x56	= 0x56,
	CSE_VERSION_0x57	= 0x57,
	CSE_VERSION_0x58	= 0x58,
	CSE_VERSION_0x59	= 0x59,
	CSE_VERSION_0x5a	= 0x5a,
	CSE_VERSION_0x5b	= 0x5b,
	CSE_VERSION_0x5c	= 0x5c,
	CSE_VERSION_0x5d	= 0x5d,
	CSE_VERSION_0x5e	= 0x5e,
	CSE_VERSION_0x5f	= 0x5f,
	CSE_VERSION_0x60	= 0x60,
	CSE_VERSION_0x62	= 0x62,
	CSE_VERSION_0x63	= 0x63,
	CSE_VERSION_0x65	= 0x65,
	CSE_VERSION_0x66	= 0x66,
	CSE_VERSION_0x67	= 0x67,
	CSE_VERSION_0x68	= 0x68,
	CSE_VERSION_0x69	= 0x69,
	CSE_VERSION_0x6a	= 0x6a,
	CSE_VERSION_0x6c	= 0x6c,
	CSE_VERSION_0x6d	= 0x6d,
	CSE_VERSION_0x6e	= 0x6e,
	CSE_VERSION_0x6f	= 0x6f,
	CSE_VERSION_0x70	= 0x70,
	CSE_VERSION_0x71	= 0x71,
	CSE_VERSION_0x73	= 0x73,
	CSE_VERSION_0x74	= 0x74,
	CSE_VERSION_0x75	= 0x75,
	CSE_VERSION_0x76	= 0x76,
	CSE_VERSION_0x78	= 0x78,
	CSE_VERSION_0x7a	= 0x7a,
	CSE_VERSION_0x7b	= 0x7b,
	CSE_VERSION_0x7c	= 0x7c,
	CSE_VERSION_0x80	= 0x80,

	// build 1580
	CSE_VERSION_1580 = 0x27,

	// patch 04 introduced following changes:
	// 1) se_level_changer instead of cse_alife_level_changer (0x7b)
	// 2) save markers (0x7b)
	// 3) persistent upgrades for cse_alife_item (0x7c)
	// so let's use this hack and broke builds 3456-3487.
	CSE_VERSION_0x7c_HACK	= 0x7b,

	// build 2215: version=0x65, script_version=2
	CSE_VERSION_2215	= CSE_VERSION_0x65,

	// build 22xx-2571: version=0x6a(dunno, where to look this?), script_version=3
	CSE_VERSION_2232	= CSE_VERSION_0x68, //guest it.. lol

	// build 2571: version 0x75 script_version = 4
	CSE_VERSION_2571	= CSE_VERSION_0x75,

	// builds 2945, 2939, 2947: version=0x76, script_version=5
	// builds 2942, 3006+, 3120: version=0x76, script_version=6
	// builds 3188, 3191+: version=0x76, script_version=7
	CSE_VERSION_SOC		= CSE_VERSION_0x76,

	// build 3456: version=0x7b, script_version=8 (some level.spawn files are at version=0x7a)
	// build 3487: version=0x7b, script_version=8
	// build 3502: version=0x7c, script_version=8
	CSE_VERSION_CS		= CSE_VERSION_0x7c,
	
	// build 3870: CoP 1.6.00
	// build 3912: CoP 1.6.01
	//·build·3967:·COP·1.6.02·version·0x80,·script_version=12
	CSE_VERSION_COP		= CSE_VERSION_0x80,
};

struct ph_net_state {
	fvector3	linear_vel;
	fvector3	angular_vel;
	fvector3	force;
	fvector3	torque;
	fvector3	position;
	fvector3	prev_position;
	union {
		fquaternion	quaternion;
		fvector3	accel;
		float		max_velocity;
	};
	fquaternion	prev_quaternion;
	bool		enabled;
};

class cse_motion {
public:
				cse_motion();
	virtual			~cse_motion();
	void			motion_read(xr_packet& packet);
	void			motion_write(xr_packet& packet);
	virtual cse_motion*	motion() = 0;
private:
	std::string		m_motion_name;
};

class cse_visual {
public:
				cse_visual();
	virtual			~cse_visual();
	void			visual_read(xr_packet& packet, uint16_t version);
	void			visual_write(xr_packet& packet, uint16_t version);
	virtual cse_visual*	visual() = 0;

	enum {
		FL_OBSTACLE,
	};
protected:
	std::string	m_visual_name;
	std::string	m_startup_animation;
	uint8_t		m_flags;
};

class cse_shape {
public:
				cse_shape();
	virtual			~cse_shape();
	void			cform_merge(xr_packet& packet);
	void			cform_read(xr_packet& packet);
	void			cform_write(xr_packet& packet);
	virtual cse_shape*	shape() = 0;
	shape_def_vec&		shapes();

protected:
	shape_def_vec		m_shapes;
};

class cse_ph_skeleton {
public:
			cse_ph_skeleton();
	virtual		~cse_ph_skeleton();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
	virtual void	data_load(xr_packet& packet);
	virtual void	data_save(xr_packet& packet);
	enum {
		FL_ACTIVE	= 0x01,
		FL_SPAWN_COPY	= 0x02,
		FL_SAVED_DATA	= 0x04,
		FL_NOT_SAVE	= 0x08,
	};
protected:
	std::string	m_root_bone;
	uint8_t		m_flags;
	uint16_t	m_source_id;
};

class cse_abstract {
public:
				cse_abstract();
	virtual			~cse_abstract();
	void			spawn_merge(xr_packet& packet);
	void			spawn_read(xr_packet& packet);
	void			spawn_write(xr_packet& packet, bool local);
	virtual void		state_merge(xr_packet& packet, uint16_t version) = 0;
	virtual void		state_read(xr_packet& packet, uint16_t size) = 0;
	virtual void		state_write(xr_packet& packet) = 0;
	virtual void		update_read(xr_packet& packet) = 0;
	virtual void		update_write(xr_packet& packet) = 0;
	virtual cse_shape*	shape();
	virtual cse_visual*	visual();
	virtual cse_motion*	motion();
	virtual cse_abstract*	base();

	enum esm_mode {
		SM_SAVE,
		SM_LOAD,
	};
	void			set_save_marker(xr_packet& packet, esm_mode mode,
						bool check, const char* name);

	std::string&		name();
	const std::string&	name() const;
	std::string&		name_replace();
	const std::string&	name_replace() const;
	xr_clsid&		clsid();
	const xr_clsid&		clsid() const;
	const uint8_t		game_id() const;
	fvector3&		position();
	const fvector3&		position() const;
	fvector3&		rotation();
	const fvector3&		rotation() const;
	uint16_t&		version();
	uint16_t		version() const;
	uint16_t&		script_version();
	uint16_t		script_version() const;
	uint16_t&		spawn_id();
	uint16_t		spawn_id() const;

	enum game_id {
		GAME_ANY		= 0,
		GAME_SINGLE		= 1,
		GAME_DEATHMATCH		= 2,
		GAME_CTF		= 3,
		GAME_ASSAULT		= 4,
		GAME_CS			= 5,
		GAME_TEAMDEATHMATCH	= 6,
		GAME_ARTEFACTHUNT	= 7,
		GAME_LASTSTANDING	= 100,
		GAME_DUMMY		= 255,
	};
	enum {
		FL_SPAWN_ENABLED		= 0x01,
		FL_SPAWN_ON_SURGE_ONLY		= 0x02,
		FL_SPAWN_SINGLE_ITEM_ONLY	= 0x04,
		FL_SPAWN_IF_DESTROYED_ONLY	= 0x08,
		FL_SPAWN_INFINITE_COUNT		= 0x10,
		FL_SPAWN_DESTROY_ON_SPAWN	= 0x20,
	};

protected:
	std::string	m_s_name_replace;

	uint16_t	m_version;
	uint16_t	m_script_version;
	uint16_t	m_respawn_time;

	uint16_t	m_id;
	uint16_t	m_id_parent;
	uint16_t	m_id_phantom;

	std::string	m_s_name;
	uint8_t		m_s_game_id;
	uint8_t		m_s_rp;
	uint16_t	m_s_flags;

	uint16_t	m_cs_unk1_u16;		// introduced in clear sky

	fvector3	m_o_position;
	fvector3	m_o_angle;

	std::string	m_ini_string;

	uint16_t	m_spawn_id;

	xr_clsid	m_clsid;

private:
	struct sm_record {
				sm_record(const char* _name, size_t _offset);
		const char*	name;
		size_t		offset;
	};
	std::stack<sm_record>	m_markers;
};

typedef std::vector<cse_abstract*> xr_entity_vec;
typedef std::vector<cse_abstract*>::iterator xr_entity_vec_it;
typedef std::vector<cse_abstract*>::const_iterator xr_entity_vec_cit;

inline std::string& cse_abstract::name() { return m_s_name; }
inline const std::string& cse_abstract::name() const { return m_s_name; }

inline std::string& cse_abstract::name_replace() { return m_s_name_replace; }
inline const std::string& cse_abstract::name_replace() const { return m_s_name_replace; }

inline fvector3& cse_abstract::position() { return m_o_position; }
inline const fvector3& cse_abstract::position() const { return m_o_position; }

inline fvector3& cse_abstract::rotation() { return m_o_angle; }
inline const fvector3& cse_abstract::rotation() const { return m_o_angle; }

inline xr_clsid& cse_abstract::clsid() { return m_clsid; }
inline const xr_clsid& cse_abstract::clsid() const { return m_clsid; }

inline const uint8_t cse_abstract::game_id() const { return m_s_game_id; }

inline uint16_t& cse_abstract::version() { return m_version; }
inline uint16_t cse_abstract::version() const { return m_version; }

inline uint16_t& cse_abstract::script_version() { return m_script_version; }
inline uint16_t cse_abstract::script_version() const { return m_script_version; }

inline uint16_t& cse_abstract::spawn_id() { return m_spawn_id; }
inline uint16_t cse_abstract::spawn_id() const { return m_spawn_id; }

class cse_alife_graph_point: public cse_abstract {
public:
			cse_alife_graph_point();
	virtual void	state_merge(xr_packet& packet, uint16_t version);
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);

	std::string&		connection_level();
	const std::string&	connection_level() const;
	std::string&		connection_point();
	const std::string&	connection_point() const;

protected:
	std::string	m_connection_level_name;
	std::string	m_connection_point_name;
	uint8_t		m_locations[4];
};

inline std::string& cse_alife_graph_point::connection_level() { return m_connection_level_name; }
inline const std::string& cse_alife_graph_point::connection_level() const { return m_connection_level_name; }

inline std::string& cse_alife_graph_point::connection_point() { return m_connection_point_name; }
inline const std::string& cse_alife_graph_point::connection_point() const { return m_connection_point_name; }

class cse_alife_object: public cse_abstract {
public:
				cse_alife_object();
	virtual void		state_merge(xr_packet& packet, uint16_t version);
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);

	uint16_t&		graph_id();
	uint16_t		graph_id() const;
	float&			distance();
	float			distance() const;
	uint32_t&		node_id();
	uint32_t		node_id() const;
	uint32_t&		flags();
	uint32_t		flags() const;

	enum {
		FL_USE_SWITCHES		= 0x0001,
		FL_SWITCH_ONLINE	= 0x0002,
		FL_SWITCH_OFFLINE	= 0x0004,
		FL_INTERACTIVE		= 0x0008,
		FL_VISIBLE_FOR_AI	= 0x0010,
		FL_USEFUL_FOR_AI	= 0x0020,
		FL_OFFLINE_NO_MOVE	= 0x0040,
		FL_USED_AI_LOCATIONS	= 0x0080,
		FL_GROUP_BEHAVIOUR	= 0x0100,
		FL_CAN_SAVE		= 0x0200,
		FL_VISIBLE_FOR_MAP	= 0x0400,
		FL_USE_SMART_TERRAINS	= 0x0800,
		FL_CHECK_FOR_SEPARATOR	= 0x1000,
		FL_CORPSE_REMOVAL	= 0x2000,
		FL_CUSTOM		= 0x800000,
	};
protected:
	uint16_t	m_graph_id;
	float		m_distance;
	bool		m_direct_control;
	uint32_t	m_node_id;
	uint32_t	m_flags;
	uint32_t	m_story_id;
	uint32_t	m_spawn_story_id;
};

inline uint16_t& cse_alife_object::graph_id() { return m_graph_id; }
inline uint16_t cse_alife_object::graph_id() const { return m_graph_id; }

inline float& cse_alife_object::distance() { return m_distance; }
inline float cse_alife_object::distance() const { return m_distance; }

inline uint32_t& cse_alife_object::node_id() { return m_node_id; }
inline uint32_t cse_alife_object::node_id() const { return m_node_id; }

inline uint32_t& cse_alife_object::flags() { return m_flags; }
inline uint32_t cse_alife_object::flags() const { return m_flags; }

class cse_alife_dynamic_object: public cse_alife_object {
public:
	virtual void	state_merge(xr_packet& packet, uint16_t version);
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
};

class cse_alife_dynamic_object_visual: public cse_alife_dynamic_object, public cse_visual {
public:
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);
	virtual cse_visual*	visual();
};

class cse_alife_object_climable: public cse_shape, public cse_alife_dynamic_object {
public:
				cse_alife_object_climable();
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);
	virtual cse_shape*	shape();
protected:
	uint16_t		m_flags;

	// CoP addition
	std::string		m_game_material;
};

class cse_smart_cover: public cse_shape, public cse_alife_dynamic_object {
public:
				cse_smart_cover();
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);
	virtual cse_shape*	shape();
protected:
	std::string		m_cs_unk1_sz;
	float			m_cs_unk2_float;
	float			m_enter_min_enemy_distance;
	float			m_exit_min_enemy_distance;
	bool			m_is_combat_cover;

	// CoP addition
	uint8_t		m_cs_unk3_u8;
};

class cse_alife_object_physic: public cse_alife_dynamic_object_visual, public cse_ph_skeleton {
public:
			cse_alife_object_physic();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	uint32_t	m_type;
	float		m_mass;
	std::string	m_fixed_bones;

	// clear sky addition
	uint8_t		m_num_items;
	ph_net_state	m_state;
};

class cse_alife_object_hanging_lamp: public cse_alife_dynamic_object_visual, public cse_ph_skeleton {
public:
			cse_alife_object_hanging_lamp();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
	enum {
		FL_PHYSIC	= 0x01,
		FL_CAST_SHADOW	= 0x02,
		FL_R1		= 0x04,
		FL_R2		= 0x08,
		FL_TYPE_SPOT	= 0x10,
		FL_POINT_AMBIENT= 0x20,
	};
protected:
	uint16_t	m_flags;
	rgba32		m_color;
	float		m_brightness;
	std::string	m_color_animator;
	std::string	m_light_texture;
	float		m_range;
	float		m_virtual_size;
	std::string	m_light_ambient_bone;
	std::string	m_light_main_bone;
	std::string	m_fixed_bones;
	float		m_spot_cone_angle;
	float		m_ambient_radius;
	float		m_ambient_power;
	std::string	m_ambient_texture;
	std::string	m_glow_texture;
	float		m_glow_radius;
	float		m_health;

	// clear sky additions
	float		m_cs_unk1_float;
	float		m_cs_unk2_float;
	float		m_cs_unk3_float;
};

class cse_alife_object_projector: public cse_alife_dynamic_object_visual {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_inventory_box: public cse_alife_dynamic_object_visual {
public:
	cse_inventory_box();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);

	uint8_t cse_alive_inventory_box__unk1_u8;
	uint8_t cse_alive_inventory_box__unk2_u8;
	std::string tip;
};

class cse_alife_object_breakable: public cse_alife_dynamic_object_visual {
public:
			cse_alife_object_breakable();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	float		m_health;
};

class cse_alife_mounted_weapon: public cse_alife_dynamic_object_visual {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_alife_stationary_mgun: public cse_alife_dynamic_object_visual {
public:
			cse_alife_stationary_mgun();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	bool		m_working;
	fvector3	m_dest_enemy_dir;
};

class cse_alife_ph_skeleton_object: public cse_alife_dynamic_object_visual, public cse_ph_skeleton {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_alife_car: public cse_alife_dynamic_object_visual, public cse_ph_skeleton {
public:
			cse_alife_car();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	float		m_health;
};

class cse_alife_helicopter: public cse_alife_dynamic_object_visual, public cse_motion, public cse_ph_skeleton {
public:
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);
	virtual cse_motion*	motion();

protected:
	std::string	m_engine_sound;
};

struct s_rotation {
	float	yaw;
	float	pitch;
	float	roll;
};

class cse_alife_creature_abstract: public cse_alife_dynamic_object_visual {
public:
			cse_alife_creature_abstract();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	uint8_t		m_s_team;
	uint8_t		m_s_squad;
	uint8_t		m_s_group;
	float		m_health;
	uint32_t	m_timestamp;
	uint8_t		m_flags;
	float		m_o_model;
	s_rotation	m_o_torso;
	std::vector<uint16_t>	m_dynamic_out_restrictions;
	std::vector<uint16_t>	m_dynamic_in_restrictions;
	uint16_t	m_killer_id;
	uint64_t	m_game_death_time;
};

class cse_alife_creature_crow: public cse_alife_creature_abstract {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_alife_creature_phantom: public cse_alife_creature_abstract {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_alife_monster_abstract: public cse_alife_creature_abstract {
public:
			cse_alife_monster_abstract();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	uint16_t	m_next_graph_id;
	uint16_t	m_prev_graph_id;
	float		m_distance_from_point;
	float		m_distance_to_point;
	std::string	m_out_space_restrictors;
	std::string	m_in_space_restrictors;
	uint16_t	m_smart_terrain_id;
	bool		m_task_reached;
};

class cse_alife_monster_zombie: public cse_alife_monster_abstract {
public:
			cse_alife_monster_zombie();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	float		m_eye_fov;
	float		m_eye_range;
	float		m_min_range;
	float		m_max_range;
	float		m_attack_speed;
	float		m_max_pursuit_radius;
	float		m_max_home_radius;
	float		m_hit_power;
	uint16_t	m_hit_interval;
	float		m_attack_distance;
	float		m_attack_angle;
};

class cse_alife_monster_base: public cse_alife_monster_abstract, public cse_ph_skeleton {
public:
			cse_alife_monster_base();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	uint16_t	m_spec_object_id;
};

class cse_alife_psy_dog_phantom: public cse_alife_monster_base {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_alife_trader_abstract {
public:
				cse_alife_trader_abstract();
	virtual void		state_merge(xr_packet& packet, uint16_t version);
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);
	virtual cse_abstract*	base() = 0;

	enum {
		FLAG_INFINITE_AMMO	= 0x1,
	};
protected:
	uint32_t	m_money;
	uint32_t	m_trader_flags;
	int32_t		m_community_index;
	int32_t		m_reputation;
	int32_t		m_rank;
	std::string	m_character_profile;
	std::string	m_specific_character;
	std::string	m_character_name;

	// CoP additions
	uint8_t		m_unk1_u8;
	uint8_t		m_unk2_u8;
};

class cse_alife_trader: public cse_alife_dynamic_object_visual, public cse_alife_trader_abstract {
public:
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);
	virtual cse_abstract*	base();
};

class cse_alife_human_abstract: public cse_alife_trader_abstract, public cse_alife_monster_abstract {
public:
	virtual void		state_merge(xr_packet& packet, uint16_t version);
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);
	virtual cse_abstract*	base();
protected:
	// from CALifeHumanBrain
	std::vector<uint8_t>	m_equipment_preferences;
	std::vector<uint8_t>	m_main_weapon_preferences;
};

class cse_alife_human_stalker: public cse_alife_human_abstract, public cse_ph_skeleton {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	std::string	m_start_dialog;
};

class cse_alife_creature_actor: public cse_alife_creature_abstract, public cse_alife_trader_abstract, cse_ph_skeleton {
public:
				cse_alife_creature_actor();
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);
	virtual cse_abstract*	base();
protected:
	uint16_t	m_state;
	fvector3	m_accel;
	fvector3	m_velocity;
	float		m_radiation;
	uint8_t		m_weapon;
	uint16_t	m_holder_id;
	uint16_t	m_num_items;
};

class cse_alife_space_restrictor: public cse_alife_dynamic_object, public cse_shape {
public:
				cse_alife_space_restrictor();
	virtual void		state_merge(xr_packet& packet, uint16_t version);
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);
	virtual cse_shape*	shape();
	enum {
		DEFAULT_RESTRICTOR_TYPE_NONE	= 0,
	};
protected:
	uint8_t		m_space_restrictor_type;
};

class cse_alife_smart_zone: public cse_alife_space_restrictor {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_alife_team_base_zone: public cse_alife_space_restrictor {
public:
			cse_alife_team_base_zone();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	uint8_t		m_team;
};

class cse_alife_level_changer: public cse_alife_space_restrictor {
public:
			cse_alife_level_changer();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	uint16_t	m_next_graph_id;
	uint32_t	m_next_node_id;
	fvector3	m_next_position;
	fvector3	m_angles;
	std::string	m_level_to_change;
	std::string	m_level_point_to_change;
	bool		m_silent_mode;
};

class cse_alife_custom_zone: public cse_alife_space_restrictor {
public:
			cse_alife_custom_zone();
	virtual void	state_merge(xr_packet& packet, uint16_t version);
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	float		m_max_power;
	uint32_t	m_owner_id;
	uint32_t	m_enabled_time;
	uint32_t	m_disabled_time;
	uint32_t	m_start_time_shift;
};

class cse_alife_anomalous_zone: public cse_alife_custom_zone {
public:
			cse_alife_anomalous_zone();
	virtual void	state_merge(xr_packet& packet, uint16_t version);
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	float		m_offline_interactive_radius;
	uint32_t	m_artefact_position_offset;
	uint16_t	m_artefact_spawn_count;
};

class cse_alife_zone_visual: public cse_alife_anomalous_zone, public cse_visual {
public:
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);
	virtual cse_visual*	visual();
protected:
	std::string		m_attack_animation;
};

class cse_alife_torrid_zone: public cse_alife_custom_zone, public cse_motion {
public:
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);
	virtual cse_motion*	motion();
};



class cse_alife_online_offline_group: public cse_alife_dynamic_object {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	std::vector<uint16_t>	m_members;
};

class cse_alife_inventory_item {
public:
				cse_alife_inventory_item();
	virtual			~cse_alife_inventory_item();
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);
	virtual cse_abstract*	base() = 0;
	enum {
		FLAG_NO_POSITION = 0x8000,
	};
protected:
	float		m_condition;
	uint32_t	m_timestamp;
	uint16_t	m_num_items;
	ph_net_state	m_state;

	// clear sky additions
	std::vector<std::string>	m_upgrades;
};

class cse_alife_item: public cse_alife_dynamic_object_visual, public cse_alife_inventory_item {
public:
	virtual void		state_read(xr_packet& packet, uint16_t size);
	virtual void		state_write(xr_packet& packet);
	virtual void		update_read(xr_packet& packet);
	virtual void		update_write(xr_packet& packet);
	virtual cse_abstract*	base();
};

class cse_alife_item_torch: public cse_alife_item {
public:
			cse_alife_item_torch();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
	enum {
		FL_TORCH_ACTIVE		= 0x01,
		FL_NIGHT_VISION_ACTIVE	= 0x02,
		FL_UNKNOWN		= 0x03,
	};
protected:
	bool		m_active;
	bool		m_nightvision_active;
	bool		m_unknown;
};

class cse_alife_item_detector: public cse_alife_item {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_alife_item_artefact: public cse_alife_item {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_alife_item_grenade: public cse_alife_item {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_alife_item_explosive: public cse_alife_item {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_alife_item_bolt: public cse_alife_item {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_alife_item_helmet: public cse_alife_item {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_alife_item_custom_outfit: public cse_alife_item {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
};

class cse_alife_item_pda: public cse_alife_item {
public:
			cse_alife_item_pda();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	uint16_t	m_original_owner;
	std::string	m_specific_character;
	std::string	m_info_portion;
};

class cse_alife_item_document: public cse_alife_item {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	std::string	m_doc;
};

class cse_alife_item_ammo: public cse_alife_item {
public:
			cse_alife_item_ammo();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	uint16_t	m_elapsed;
};

class cse_alife_item_weapon: public cse_alife_item {
public:
			cse_alife_item_weapon();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
	enum addon_status {
		ADDON_DISABLED	= 0,
		ADDON_PERMANENT	= 1,
		ADDON_ATTACHABLE= 2,
	};
protected:
	addon_status	m_scope;
	addon_status	m_silencer;
	addon_status	m_grenade_launcher;
	uint8_t		m_flags;
	uint8_t		m_state;
	uint8_t		m_ammo_type;
	uint16_t	m_ammo_current;
	uint16_t	m_ammo_elapsed;
	uint8_t		m_addon_flags;
// FIXME: see update_{read|write}() implementation
//	bool		m_zoom;
	uint8_t		m_zoom;

	// clear sky addition
	uint8_t		m_cs_unk1_u8;
};

class cse_alife_item_weapon_magazined: public cse_alife_item_weapon {
public:
			cse_alife_item_weapon_magazined();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	uint8_t		m_cur_fire_mode;
};

class cse_alife_item_weapon_magazined_w_gl: public cse_alife_item_weapon_magazined {
public:
			cse_alife_item_weapon_magazined_w_gl();
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	bool		m_grenade_mode;
};

class cse_alife_item_weapon_shotgun: public cse_alife_item_weapon_magazined {
public:
	virtual void	state_read(xr_packet& packet, uint16_t size);
	virtual void	state_write(xr_packet& packet);
	virtual void	update_read(xr_packet& packet);
	virtual void	update_write(xr_packet& packet);
protected:
	std::vector<uint8_t>	m_ammo_ids;
};

} // end of namespace xray_re

#endif
