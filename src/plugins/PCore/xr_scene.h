#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_H__
#define __XR_SCENE_H__

#include <vector>
#include "xr_scene_common.h"
#include "xr_scene_revision.h"
#include "xr_vector3.h"
#include "xr_guid.h"

namespace xray_re {

enum {
	XRLC_QUALITY_DRAFT	= 0,
	XRLC_QUALITY_HIGH	= 1,
	XRLC_QUALITY_CUSTOM	= 2,
};

const uint32_t SCENE_VERSION = 5;

// editor scene top chunks
enum {
	SCENE_CHUNK_VERSION		= 0x9df3,
	SCENE_CHUNK_OBJECTS		= 0x7708,
	SCENE_CHUNK_CAMERA		= 0x7709,
	SCENE_CHUNK_SNAP_OBJECTS	= 0x7710,
	SCENE_CHUNK_OPTIONS		= 0x7711,
	SCENE_CHUNK_COUNT		= 0x7712,	// FIXME: not sure
};

const uint32_t SCENE_LO_VERSION = 8;
const uint32_t SCENE_LO_BPARAMS_VERSION = 9;

// scene options subchunks
enum {
	SCENE_CHUNK_LO_VERSION		= 0x7801,
	SCENE_CHUNK_LO_NAMES		= 0x7802,
	SCENE_CHUNK_LO_BOP		= 0x7803,	// custom data (level.ltx)
	SCENE_CHUNK_LO_NAME_PREFIX	= 0x7804,
	SCENE_CHUNK_LO_BPARAMS_VERSION	= 0x7849,
	SCENE_CHUNK_LO_BPARAMS		= 0x7850,
	SCENE_CHUNK_LO_QUALITY		= 0x7851,
};

struct b_params {
	void		init();
	void		set_debug();
	void		set_release();
	void		save_v12(xr_ini_writer *w);

	float		sm_angle;
	float		weld_distance;
	float		lm_pixels_per_meter;
	int32_t		lm_jitter_samples;
	uint32_t	lm_rms_zero;
	uint32_t	lm_rms;
	int32_t		convert_progressive;
	float		pm_uv;
	float		pm_pos;
	float		pm_curv;
	float		pm_border_h_angle;
	float		pm_border_h_distance;
	float		pm_heuristic;
};

class xr_scene_ai_map;
class xr_scene_details;
class xr_scene_glows;
class xr_scene_groups;
class xr_scene_lights;
class xr_scene_visuals;
class xr_scene_portals;
class xr_scene_particles;
class xr_scene_sectors;
class xr_scene_shapes;
class xr_scene_sound_envs;
class xr_scene_sound_srcs;
class xr_scene_spawns;
class xr_scene_wallmarks;
class xr_scene_ways;
class xr_custom_object;
class xr_scene_part;

class xr_scene {
public:
				xr_scene();
	virtual			~xr_scene();
	bool			load(const char* name);
	bool			save(const char* name);

	bool			save_v12(const char* name);

	void			load_objects(xr_reader& r, uint32_t chunk_id, std::vector<xr_custom_object*>& objects);
	void			save_objects(xr_writer& w, uint32_t chunk_id, const std::vector<xr_custom_object*>& objects) const;
	void			save_objects(xr_ini_writer* w, const std::vector<xr_custom_object*>& objects, const char* prefix = "object") const;

	xr_custom_object*	create_object(tools_class_id class_id);

	xr_scene_ai_map*	ai_map();
	xr_scene_details*	details();
	xr_scene_glows*		glows();
	xr_scene_groups*	groups();
	xr_scene_lights*	lights();
	xr_scene_visuals*	visuals();
	xr_scene_portals*	portals();
	xr_scene_particles*	particles();
	xr_scene_sectors*	sectors();
	xr_scene_shapes*	shapes();
	xr_scene_sound_envs*	sound_envs();
	xr_scene_sound_srcs*	sound_srcs();
	xr_scene_spawns*	spawns();
	xr_scene_wallmarks*	wallmarks();
	xr_scene_ways*		ways();

	xr_scene_revision&	revision();
	std::string&		name();
	const std::string&	name() const;
	std::string&		name_prefix();
	std::string&		custom_data();

	void			set_quality(unsigned xrlc_quality);

	xr_scene_part*		part(scene_chunk_id chunk_id);

	void			write_guid(xr_ini_writer* w);
	void			write_revision(xr_ini_writer *w, bool scene_part = true);

protected:
	void			load_options(xr_reader& r);
	void			save_options(xr_writer& w);

private:
	xr_guid			m_guid;			// main scene (<name>.level)
	fvector3		m_camera_pos;
	fvector3		m_camera_orient;
	xr_scene_revision	m_revision;

	// N.B.: this is a _target_ level name, not scene's file name
	std::string		m_name;			// SCENE_CHUNK_LO_NAMES
	std::string		m_name_prefix;		// SCENE_CHUNK_LO_NAME_PREFIX
	std::string		m_custom_data;
	uint8_t			m_hemi_quality;
	uint8_t			m_sun_quality;
	b_params		m_bparams;		// SCENE_CHUNK_LO_BPARAMS

	std::vector<xr_scene_part*>	m_parts;
	std::vector<xr_custom_object*>	m_objects;		// unused really
	std::vector<std::string>	m_snap_objects;
};

inline std::string& xr_scene::name() { return m_name; }
inline const std::string& xr_scene::name() const { return m_name; }
inline std::string& xr_scene::name_prefix() { return m_name_prefix; }
inline std::string& xr_scene::custom_data() { return m_custom_data; }

} // end of namespace xray_re

#endif
