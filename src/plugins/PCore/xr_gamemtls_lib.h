#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_GAMEMTLS_LIB_H__
#define __XR_GAMEMTLS_LIB_H__

#include <string>
#include <vector>
#include "xr_types.h"

namespace xray_re {

const uint16_t GAMEMTLS_VERSION = 1;

enum {
	GAMEMTLS_CHUNK_VERSION		= 0x1000,
	GAMEMTLS_CHUNK_AUTOINC		= 0x1001,
	GAMEMTLS_CHUNK_MATERIALS	= 0x1002,
	GAMEMTLS_CHUNK_MATERIAL_PAIRS	= 0x1003,
};

enum {
	GAMEMTL_CHUNK_MAIN		= 0x1000,
	GAMEMTL_CHUNK_FLAGS		= 0x1001,
	GAMEMTL_CHUNK_PHYSICS		= 0x1002,
	GAMEMTL_CHUNK_FACTORS		= 0x1003,
	GAMEMTL_CHUNK_FLOTATION		= 0x1004,
	GAMEMTL_CHUNK_DESC		= 0x1005,
	GAMEMTL_CHUNK_INJURY		= 0x1006,
};

enum {
	GAMEMTLPAIR_CHUNK_PAIR		= 0x1000,
	GAMEMTLPAIR_CHUNK_BREAKING	= 0x1002,
	GAMEMTLPAIR_CHUNK_STEP		= 0x1003,
	GAMEMTLPAIR_CHUNK_COLLIDE	= 0x1005,
};

class xr_reader;

struct xr_gamemtl {
	enum {
		MF_BREAKABLE		= 0x00000001,
		MF_BOUNCEABLE		= 0x00000004,
		MF_SKIDMARK		= 0x00000008,
		MF_BLOODMARK		= 0x00000010,
		MF_CLIMABLE		= 0x00000020,
		MF_PASSABLE		= 0x00000080,
		MF_DYNAMIC		= 0x00000100,
		MF_LIQUID		= 0x00000200,
		MF_SUPPRESS_SHADOWS	= 0x00000400,
		MF_SUPPRESS_WALLMARKS	= 0x00000800,
		MF_ACTOR_OBSTACLE	= 0x00001000,
		MF_INJURIOUS		= 0x10000000,
		MF_SHOOTABLE		= 0x20000000,
		MF_TRANSPARENT		= 0x40000000,
		MF_SLOW_DOWN		= 0x80000000,
	};

	unsigned	id;
	std::string	name;
	std::string	desc;

	uint32_t	flags;

	float		friction;
	float		damping;
	float		spring;
	float		bouncing_start_velocity;
	float		bounce;

	float		shoot_factor;
	float		bounce_damage_factor;
	float		vis_transparency_factor;
	float		snd_occlusion_factor;
	float		flotation_factor;

	float		injurious_speed;

	void		load(xr_reader& r);
};

TYPEDEF_STD_VECTOR_PTR(xr_gamemtl)

struct xr_gamemtlpair {
	enum {
		MPF_BREAKING_SOUNDS	= 0x02,
		MPF_STEP_SOUNDS		= 0x04,
		MPF_COLLIDE_SOUNDS	= 0x10,
		MPF_COLLIDE_PARTICLES	= 0x20,
		MPF_COLLIDE_MARKS	= 0x40,
	};

	unsigned	mtl0;
	unsigned	mtl1;
	unsigned	id;
	unsigned	id_parent;
	uint32_t	own_props;

	std::string	breaking_sounds;
	std::string	step_sounds;
	std::string	collide_sounds;
	std::string	collide_particles;
	std::string	collide_marks;

	void		load(xr_reader& r);
};

TYPEDEF_STD_VECTOR_PTR(xr_gamemtlpair)

class xr_gamemtls_lib {
public:
				xr_gamemtls_lib();
				xr_gamemtls_lib(xr_reader& r);
	virtual			~xr_gamemtls_lib();

	bool			load(const char* path, const char* name);
	bool			load(const std::string& path);
	const xr_gamemtl*	get_material(uint32_t id) const;
	const xr_gamemtl*	get_material(const char* name) const;

	const xr_gamemtl_vec&		materials() const;
	const xr_gamemtlpair_vec&	material_pairs() const;

protected:
	void			load(xr_reader& r);

private:
	unsigned		m_material_index;
	unsigned		m_material_pair_index;

	xr_gamemtl_vec		m_materials;
	xr_gamemtlpair_vec	m_material_pairs;
};

inline xr_gamemtls_lib::xr_gamemtls_lib() {}
inline xr_gamemtls_lib::xr_gamemtls_lib(xr_reader& r) { load (r); }
inline const xr_gamemtl_vec& xr_gamemtls_lib::materials() const { return m_materials; }
inline const xr_gamemtlpair_vec& xr_gamemtls_lib::material_pairs() const { return m_material_pairs; }

} // end of namespace xray_re

#endif
