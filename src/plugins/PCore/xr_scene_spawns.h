#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_SPAWNS_H__
#define __XR_SCENE_SPAWNS_H__

#include "xr_scene_objects.h"
#include "xr_color.h"

namespace xray_re {

class cse_abstract;

// CSpawnPoint
const uint16_t SPAWNPOINT_VERSION = 0x14;
const uint16_t SPAWNPOINT_VERSION_V12 = 0x17;

enum {
	SPAWNPOINT_TYPE_RPOINT	= 0,
	SPAWNPOINT_TYPE_ENV_MOD	= 1,
	SPAWNPOINT_TYPE_ENTITY	= 2,
};

enum {
	SPAWNPOINT_CHUNK_VERSION	= 0xe411,
	SPAWNPOINT_CHUNK_RPOINT_PARAMS	= 0xe413,
	SPAWNPOINT_CHUNK_TYPE		= 0xe417,
	SPAWNPOINT_CHUNK_SECTION	= 0xe419,
	SPAWNPOINT_CHUNK_SPAWNDATA	= 0xe420,	// just like STATE_Read/STATE_Write
	SPAWNPOINT_CHUNK_ATTACHED_OBJECT= 0xe421,
	SPAWNPOINT_CHUNK_ENV_MOD_PARAMS	= 0xe422,
	SPAWNPOINT_CHUNK_ENV_MOD_HEMI	= 0xe423,
};

class xr_spawn_object: public xr_custom_object {
public:
			xr_spawn_object(xr_scene& scene, const char* section = 0);
	virtual		~xr_spawn_object();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&	type();

	uint8_t&	team();
	uint8_t&	respawn();
	uint8_t&	game();

	float&		radius();
	float&		power();
	float&		view_distance();
	rgba32&		fog_color();
	float&		fog_density();
	rgba32&		ambient_color();
	rgba32&		sky_color();
	rgba32&		hemi_color();

	cse_abstract*&	entity();

	xr_custom_object*&	attached_object();
	const xr_custom_object*	attached_object() const;

protected:
	uint32_t		m_type;
	union {
		struct {
			uint8_t	m_team;
			uint8_t	m_respawn;
			uint8_t	m_game;
		};
		struct {
			float	m_radius;
			float	m_power;
			float	m_view_distance;
			rgba32	m_fog_color;
			float	m_fog_density;
			rgba32	m_ambient_color;
			rgba32	m_sky_color;
			rgba32	m_hemi_color;
		};
		cse_abstract*	m_entity;
	};
	xr_custom_object*	m_attached_object;
};

inline uint32_t& xr_spawn_object::type() { return m_type; }

inline uint8_t& xr_spawn_object::team() { return m_team; }
inline uint8_t& xr_spawn_object::respawn() { return m_respawn; }
inline uint8_t& xr_spawn_object::game() { return m_game; }
inline float& xr_spawn_object::radius() { return m_radius; }
inline float& xr_spawn_object::power() { return m_power; }
inline float& xr_spawn_object::view_distance() { return m_view_distance; }
inline rgba32& xr_spawn_object::fog_color() { return m_fog_color; }
inline float& xr_spawn_object::fog_density() { return m_fog_density; }
inline rgba32& xr_spawn_object::ambient_color() { return m_ambient_color; }
inline rgba32& xr_spawn_object::sky_color() { return m_sky_color; }
inline rgba32& xr_spawn_object::hemi_color() { return m_hemi_color; }
inline cse_abstract*& xr_spawn_object::entity() { return m_entity; }
inline xr_custom_object*& xr_spawn_object::attached_object() { return m_attached_object; }
inline const xr_custom_object* xr_spawn_object::attached_object() const { return m_attached_object; }


// ESceneSpawnTools
enum {
	SPAWNS_CHUNK_COMMON_FLAGS	= 0x1002,
};

class xr_scene_spawns: public xr_scene_objects {
public:
			xr_scene_spawns(xr_scene& scene);
	virtual		~xr_scene_spawns();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&	flags();
	uint32_t	flags() const;

protected:
	uint32_t	m_flags;	// SPAWNS_CHUNK_COMMON_FLAGS
};

inline uint32_t& xr_scene_spawns::flags() { return m_flags; }
inline uint32_t xr_scene_spawns::flags() const { return m_flags; }

} // end of namespace xray_re

#endif
