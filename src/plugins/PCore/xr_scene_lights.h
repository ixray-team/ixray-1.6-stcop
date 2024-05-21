#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_LIGHTS_H__
#define __XR_SCENE_LIGHTS_H__

#include "xr_scene_objects.h"
#include "xr_vector2.h"
#include "xr_vector4.h"
#include "xr_color.h"

namespace xray_re {

struct xr_token {
			xr_token();
	explicit	xr_token(const char* _name, uint32_t _id);

	std::string	name;
	uint32_t	id;
};
TYPEDEF_STD_VECTOR(xr_token)

// CLight
const uint16_t LIGHT_VERSION_16	= 0x10;
const uint16_t LIGHT_VERSION = 0x11;

// CLight flags
enum {
	LIGHT_FLAG_LIGHTMAP	= 0x01,
	LIGHT_FLAG_DYNAMIC	= 0x02,
	LIGHT_FLAG_ANIMATED	= 0x04,
	LIGHT_FLAG_FUZZY	= 0x10,
};

enum {
	LIGHT_CHUNK_VERSION	= 0xb411,
	LIGHT_CHUNK_FLAGS	= 0xb413,
	LIGHT_CHUNK_BRIGHTNESS	= 0xb425,
	LIGHT_CHUNK_D3D_PARAMS	= 0xb435,
	LIGHT_CHUNK_USE_IN_D3D	= 0xb436,
	LIGHT_CHUNK_ROTATION	= 0xb437,
	LIGHT_CHUNK_ANIMATION	= 0xb438,
	LIGHT_CHUNK_TEXTURE	= 0xb439,
	LIGHT_CHUNK_FUZZY_DATA	= 0xb440,
	LIGHT_CHUNK_CONTROL	= 0xb441,
	LIGHT_CHUNK_PARAMS	= 0xb442,
};

class xr_light_object: public xr_custom_object {
public:
			xr_light_object(xr_scene& scene);
	virtual		~xr_light_object();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&	type();
	fcolor&		color();
	float&		brightness();
	float&		range();
	float&		attenuation_constant();
	float&		attenuation_linear();
	float&		attenuation_quadratic();
	float&		cone_angle();
	float&		unknown();

	uint32_t&	use_in_d3d();
	uint32_t&	flags();
	uint32_t&	control();

	enum {
		FUZZY_SPHERE	= 0,
		FUZZY_BOX	= 1,
	};
	struct fuzzy_shape {
		uint8_t		type;
		fvector4	fixme;
	};

private:
	uint32_t	m_type;			// LIGHT_CHUNK_PARAMS or LIGHT_CHUNK_D3D_PARAMS+LIGHT_CHUNK_BRIGHTNESS
	fcolor		m_color;
	float		m_brightness;
	float		m_range;
	float		m_attenuation_constant;
	float		m_attenuation_linear;
	float		m_attenuation_quadratic;
	float		m_cone_angle;
	float		m_unknown;

	uint32_t	m_use_in_d3d;		// LIGHT_CHUNK_USE_IN_D3D
	uint32_t	m_flags;		// LIGHT_CHUNK_FLAGS
	uint32_t	m_control;		// LIGHT_CHUNK_CONTROL
	std::string	m_animation;		// LIGHT_CHUNK_ANIMATION
	std::string	m_texture;		// LIGHT_CHUNK_TEXTURE

	fuzzy_shape		m_shape;	// LIGHT_CHUNK_FUZZY_DATA
	std::vector<fvector3>	m_vertices;
};

inline uint32_t& xr_light_object::type() { return m_type; }
inline fcolor& xr_light_object::color() { return m_color; }
inline float& xr_light_object::brightness() { return m_brightness; }
inline float& xr_light_object::range() { return m_range; }
inline float& xr_light_object::attenuation_constant() { return m_attenuation_constant; }
inline float& xr_light_object::attenuation_linear() { return m_attenuation_linear; }
inline float& xr_light_object::attenuation_quadratic() { return m_attenuation_quadratic; }
inline float& xr_light_object::cone_angle() { return m_cone_angle; }
inline float& xr_light_object::unknown() { return m_unknown; }

inline uint32_t& xr_light_object::use_in_d3d() { return m_use_in_d3d; }
inline uint32_t& xr_light_object::flags() { return m_flags; }
inline uint32_t& xr_light_object::control() { return m_control; }


// ESceneLightTools
enum {
	LIGHTS_FLAG_DRAW_NAME		= 0x40,
};

enum {
	LIGHTS_CHUNK_COMMON_CONTROLS	= 0x1002,
	LIGHTS_CHUNK_COMMON_COUNT	= 0x1003,
	LIGHTS_CHUNK_COMMON_FLAGS	= 0x1004,
	LIGHTS_CHUNK_COMMON_SUN		= 0x1006,
};

class xr_scene_lights: public xr_scene_objects {
public:
			xr_scene_lights(xr_scene& scene);
	virtual		~xr_scene_lights();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&	flags();
	xr_token_vec&	controls();
	fvector2&	sun();

private:
	uint32_t	m_flags;	// LIGHTS_CHUNK_COMMON_FLAGS
	xr_token_vec	m_controls;	// LIGHTS_CHUNK_COMMON_COUNT + LIGHTS_CHUNK_COMMON_CONTROLS
	fvector2	m_sun;		// LIGHTS_CHUNK_COMMON_SUN (altitude then longitude)
};

inline uint32_t& xr_scene_lights::flags() { return m_flags; }
inline xr_token_vec& xr_scene_lights::controls() { return m_controls; }
inline fvector2& xr_scene_lights::sun() { return m_sun; }

} // end of namespace xray_re

#endif
