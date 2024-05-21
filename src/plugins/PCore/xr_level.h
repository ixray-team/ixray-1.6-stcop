#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_H__
#define __XR_LEVEL_H__

#include <vector>
#include "xr_types.h"
#include "xr_level_version.h"

namespace xray_re {

class xr_level_ltx;
class xr_level_geom;
class xr_level_visuals;
class xr_level_shaders;
class xr_level_sectors;
class xr_level_portals;
class xr_level_lights;
class xr_level_glows;
class xr_level_cform;
class xr_level_hom;
class xr_level_details;
class xr_level_ai;
class xr_level_game;
class xr_level_spawn;
class xr_level_wallmarks;
class xr_level_som;
class xr_level_snd_env;
class xr_level_snd_static;
class xr_level_ps_static;
class xr_level_env_mod;
class xr_level_fog_vol;
class xr_build_lights;
class xr_image;
class xr_ogf;
class xr_gamemtls_lib;

class xr_reader;
class xr_writer;

class xr_level {
public:
			xr_level();
	virtual		~xr_level();

	bool		load(const char* game_data_path, const char* level_path);

	uint32_t			xrlc_version() const;
	uint32_t			xrlc_quality() const;
	const xr_level_ltx*		ltx() const;
	const xr_level_geom*		geom() const;
	const xr_level_geom*		geomx() const;
	xr_level_visuals*		visuals();
	const xr_level_visuals*		visuals() const;
	xr_level_shaders*		shaders();
	const xr_level_shaders*		shaders() const;
	const xr_level_sectors*		sectors() const;
	const xr_level_portals*		portals() const;
	const xr_level_lights*		lights() const;
	const xr_level_glows*		glows() const;
	xr_level_cform*			cform();
	const xr_level_cform*		cform() const;
	const xr_level_hom*		hom() const;
	xr_level_details*		details();
	const xr_level_details*		details() const;
	const xr_level_ai*		ai() const;
	const xr_level_game*		game() const;
	xr_level_spawn*			spawn();
	const xr_level_spawn*		spawn() const;
	const xr_level_wallmarks*	wallmarks() const;
	const xr_level_som*		som() const;
	const xr_level_snd_env*		snd_env() const;
	const xr_level_snd_static*	snd_static() const;
	const xr_level_ps_static*	ps_static() const;
	const xr_level_env_mod*		env_mod() const;
	const xr_level_fog_vol*		fog_vol() const;
	const xr_build_lights*		build_lights() const;
	const xr_image*			lods() const;
	const xr_image*			lods_nm() const;
	const std::vector<xr_ogf*>&	brkbl_meshes() const;
	const xr_gamemtls_lib*		gamemtls_lib() const;

	void	clear_ltx();
	void	clear_geom();
	void	clear_geomx();
	void	clear_visuals();
	void	clear_shaders();
	void	clear_sectors();
	void	clear_portals();
	void	clear_lights();
	void	clear_glows();
	void	clear_cform();
	void	clear_hom();
	void	clear_details();
	void	clear_ai();
	void	clear_game();
	void	clear_spawn();
	void	clear_wallmarks();
	void	clear_som();
	void	clear_snd_env();
	void	clear_snd_static();
	void	clear_ps_static();
	void	clear_env_mod();
	void	clear_fog_vol();
	void	clear_build_lights();
	void	clear_lods();
	void	clear_lods_nm();
	void	clear_brkbl_meshes();
	void	clear_gamemtls_lib();

protected:
	void	load(uint32_t xrlc_version, const char* game_data_path,
				const char* level_path, xr_reader& r);
	void	save(xr_writer& w) const;

private:
	uint32_t		m_xrlc_version;
	uint32_t		m_xrlc_quality;

	xr_level_ltx*		m_ltx;

	xr_level_geom*		m_geom;
	xr_level_geom*		m_geomx;
	xr_level_visuals*	m_visuals;
	xr_level_shaders*	m_shaders;

	xr_level_sectors*	m_sectors;
	xr_level_portals*	m_portals;

	xr_level_lights*	m_lights;
	xr_level_glows*		m_glows;

	xr_level_cform*		m_cform;

	xr_level_hom*		m_hom;

	xr_level_details*	m_details;

	xr_level_ai*		m_ai;
	xr_level_game*		m_game;
	xr_level_spawn*		m_spawn;

	xr_level_wallmarks*	m_wallmarks;

	xr_level_som*		m_som;
	xr_level_snd_env*	m_snd_env;
	xr_level_snd_static*	m_snd_static;

	xr_level_ps_static*	m_ps_static;

	xr_level_env_mod*	m_env_mod;
	xr_level_fog_vol*	m_fog_vol;

	xr_build_lights*	m_build_lights;

	xr_image*		m_lods;
	xr_image*		m_lods_nm;

	std::vector<xr_ogf*>	m_brkbl_meshes;	// meshes\brkbl#N.ogf

	xr_gamemtls_lib*	m_gamemtls_lib;
};

inline uint32_t xr_level::xrlc_version() const { return m_xrlc_version; }
inline uint32_t xr_level::xrlc_quality() const { return m_xrlc_quality; }
inline const xr_level_ltx* xr_level::ltx() const { return m_ltx; }
inline const xr_level_geom* xr_level::geom() const { return m_geom; }
inline const xr_level_geom* xr_level::geomx() const { return m_geomx; }
inline xr_level_visuals* xr_level::visuals() { return m_visuals; }
inline const xr_level_visuals* xr_level::visuals() const { return m_visuals; }
inline xr_level_shaders* xr_level::shaders() { return m_shaders; }
inline const xr_level_shaders* xr_level::shaders() const { return m_shaders; }
inline const xr_level_sectors* xr_level::sectors() const { return m_sectors; }
inline const xr_level_portals* xr_level::portals() const { return m_portals; }
inline const xr_level_lights* xr_level::lights() const { return m_lights; }
inline const xr_level_glows* xr_level::glows() const { return m_glows; }
inline xr_level_cform* xr_level::cform() { return m_cform; }
inline const xr_level_cform* xr_level::cform() const { return m_cform; }
inline const xr_level_hom* xr_level::hom() const { return m_hom; }
inline xr_level_details* xr_level::details() { return m_details; }
inline const xr_level_details* xr_level::details() const { return m_details; }
inline const xr_level_ai* xr_level::ai() const { return m_ai; }
inline const xr_level_game* xr_level::game() const { return m_game; }
inline xr_level_spawn* xr_level::spawn() { return m_spawn; }
inline const xr_level_spawn* xr_level::spawn() const { return m_spawn; }
inline const xr_level_wallmarks* xr_level::wallmarks() const { return m_wallmarks; }
inline const xr_level_som* xr_level::som() const { return m_som; }
inline const xr_level_snd_env* xr_level::snd_env() const { return m_snd_env; }
inline const xr_level_snd_static* xr_level::snd_static() const { return m_snd_static; }
inline const xr_level_ps_static* xr_level::ps_static() const { return m_ps_static; }
inline const xr_level_env_mod* xr_level::env_mod() const { return m_env_mod; }
inline const xr_level_fog_vol* xr_level::fog_vol() const { return m_fog_vol; }
inline const xr_build_lights* xr_level::build_lights() const { return m_build_lights; }
inline const xr_image* xr_level::lods() const { return m_lods; }
inline const xr_image* xr_level::lods_nm() const { return m_lods_nm; }
inline const std::vector<xr_ogf*>& xr_level::brkbl_meshes() const { return m_brkbl_meshes; }
inline const xr_gamemtls_lib* xr_level::gamemtls_lib() const { return m_gamemtls_lib; }

} // end of namespace xray_re

#endif
