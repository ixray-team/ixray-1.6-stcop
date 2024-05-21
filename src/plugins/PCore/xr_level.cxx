#include "xr_level_version.h"
#include "xr_level.h"
#include "xr_level_ltx.h"
#include "xr_level_geom.h"
#include "xr_level_visuals.h"
#include "xr_level_shaders.h"
#include "xr_level_sectors.h"
#include "xr_level_portals.h"
#include "xr_level_lights.h"
#include "xr_level_glows.h"
#include "xr_level_cform.h"
#include "xr_level_hom.h"
#include "xr_level_details.h"
#include "xr_level_dm.h"
#include "xr_level_ai.h"
#include "xr_level_game.h"
#include "xr_level_spawn.h"
#include "xr_level_wallmarks.h"
#include "xr_level_som.h"
#include "xr_level_snd_env.h"
#include "xr_level_snd_static.h"
#include "xr_level_ps_static.h"
#include "xr_level_env_mod.h"
#include "xr_level_fog_vol.h"
#include "xr_build_lights.h"
#include "xr_image.h"
#include "xr_gamemtls_lib.h"
#include "xr_entity.h"
#include "xr_ogf_v4.h"
#include "xr_file_system.h"
#include "xr_utils.h"

using namespace xray_re;

xr_level::xr_level():
	m_ltx(0),
	m_geom(0), m_geomx(0), m_visuals(0), m_shaders(0),
	m_sectors(0), m_portals(0),
	m_lights(0), m_glows(0),
	m_cform(0),
	m_hom(0),
	m_details(0),
	m_ai(0), m_game(0), m_spawn(0),
	m_wallmarks(0),
	m_som(0), m_snd_env(0), m_snd_static(0),
	m_ps_static(0),
	m_env_mod(0),
	m_fog_vol(0),
	m_build_lights(0),
	m_lods(0), m_lods_nm(0),
	m_gamemtls_lib(0) {}

xr_level::~xr_level()
{
	delete m_ltx;
	delete m_geom;
	delete m_geomx;
	delete m_visuals;
	delete m_shaders;
	delete m_sectors;
	delete m_portals;
	delete m_lights;
	delete m_glows;
	delete m_hom;
	delete m_cform;
	delete m_details;
	delete m_ai;
	delete m_game;
	delete m_spawn;
	delete m_wallmarks;
	delete m_som;
	delete m_snd_env;
	delete m_snd_static;
	delete m_ps_static;
	delete m_env_mod;
	delete m_fog_vol;
	delete m_build_lights;
	delete m_lods;
	delete m_lods_nm;
	delete_elements(m_brkbl_meshes);
	delete m_gamemtls_lib;
}

bool xr_level::load(const char* game_data_path, const char* level_path)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(level_path, "level");
	if (r == 0)
		return false;
	bool status = false;
	if (r->find_chunk(FSL_HEADER)) {
		uint32_t xrlc_version = r->r_u16();
		m_xrlc_quality = r->r_u16();
		switch (xrlc_version) {
		case XRLC_VERSION_5:
		case XRLC_VERSION_8:
			{
				char buf[124];
				r->r_cseq(sizeof(buf), buf);
			}
			// fall through
		case XRLC_VERSION_9:
		case XRLC_VERSION_10:
		case XRLC_VERSION_11:
		case XRLC_VERSION_12:
		case XRLC_VERSION_13:
		case XRLC_VERSION_14:
			r->debug_find_chunk();
			load(xrlc_version, game_data_path, level_path, *r);
			status = true;
			break;
		default:
			msg("unknown xrLC version %" PRIu32, xrlc_version);
			break;
		}
	}
	fs.r_close(r);
	return status;
}

template<typename T> static T* load(const char* path, const char* name, bool required = false)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r) {
		msg("loading %s", name);
		T* instance = new T(*r);
		fs.r_close(r);
		return instance;
	} else if (required) {
		msg("can't load %s", name);
		xr_not_expected();
	}
	return 0;
}

template<> xr_image* load(const char* path, const char* name, bool required)
{
	xr_file_system& fs = xr_file_system::instance();
	if (fs.file_exist(path, name)) {
		msg("loading %s", name);
		xr_image* image = new xr_image;
		if (!image->load_dds(path, name))
			xr_not_expected();
		return image;
	}
	return 0;
}

void xr_level::load(uint32_t xrlc_version, const char* game_data_path, const char* level_path, xr_reader& r)
{
	m_ltx = ::load<xr_level_ltx>(level_path, "level.ltx", true);

	if (xrlc_version >= XRLC_VERSION_13) {
		m_geom = ::load<xr_level_geom>(level_path, "level.geom", true);
//		m_geomx = ::load<xr_level_geom>(level_path, "level.geomx");
	}
	msg("loading %s", "level");
	if (xrlc_version <= XRLC_VERSION_12) {
		msg("...geom");
		m_geom = new xr_level_geom(xrlc_version, r);
	}
	msg("...visuals");
	m_visuals = new xr_level_visuals(xrlc_version, r, m_geom);
	msg("...shaders/textures");
	m_shaders = new xr_level_shaders(xrlc_version, r);

	msg("...sectors");
	m_sectors = new xr_level_sectors(xrlc_version, r);
	msg("...portals");
	m_portals = new xr_level_portals(xrlc_version, r);

	msg("...lights");
	m_lights = new xr_level_lights(xrlc_version, r);
	msg("...glows");
	m_glows = new xr_level_glows(xrlc_version, r);

	if (xrlc_version <= XRLC_VERSION_9) {
		msg("...cform");
		m_cform = new xr_level_cform(xrlc_version, r);
	} else {
		m_cform = ::load<xr_level_cform>(level_path, "level.cform", true);
	}

	m_hom = ::load<xr_level_hom>(level_path, "level.hom");

	m_details = ::load<xr_level_details>(level_path, "level.details");
	if (m_details && xrlc_version >= XRLC_VERSION_12) {
		msg("...texture");
		m_details->load_texture(level_path);
	}

	if (xrlc_version >= XRLC_VERSION_9 && xrlc_version <= XRLC_VERSION_12) {
		m_ai = ::load<xr_level_ai>(level_path, "level.ai");
		m_game = ::load<xr_level_game>(level_path, "level.game");
		m_spawn = ::load<xr_level_spawn>(level_path, "level.spawn");
		//m_snd_static = ::load<xr_level_snd_static>(level_path, "level.sound_static");
		m_ps_static = ::load<xr_level_ps_static>(level_path, "level.ps_static");

		//unknown format old level.wallmarks
		//m_wallmarks = ::load<xr_level_wallmarks>(level_path, "level.wallmarks");
		
		m_env_mod = ::load<xr_level_env_mod>(level_path, "level.env_mod");
		m_snd_env = ::load<xr_level_snd_env>(level_path, "level.sound_environment");
	} else if (xrlc_version >= XRLC_VERSION_13) {
		m_ai = ::load<xr_level_ai>(level_path, "level.ai");
		m_game = ::load<xr_level_game>(level_path, "level.game");
		m_spawn = ::load<xr_level_spawn>(level_path, "level.spawn");

		m_wallmarks = ::load<xr_level_wallmarks>(level_path, "level.wallmarks");

		m_som = ::load<xr_level_som>(level_path, "level.som");
		m_snd_env = ::load<xr_level_snd_env>(level_path, "level.snd_env");
		m_snd_static = ::load<xr_level_snd_static>(level_path, "level.snd_static");

		m_ps_static = ::load<xr_level_ps_static>(level_path, "level.ps_static");

		m_env_mod = ::load<xr_level_env_mod>(level_path, "level.env_mod");

		m_fog_vol = ::load<xr_level_fog_vol>(level_path, "level.fog_vol");

		if (xrlc_version >= XRLC_VERSION_14)
			m_build_lights = ::load<xr_build_lights>(level_path, "build.lights");
	} else {
		m_snd_static = new xr_level_snd_static(*m_ltx->ini());
	}

	m_lods = ::load<xr_image>(level_path, "level_lods.dds");
	m_lods_nm = ::load<xr_image>(level_path, "level_lods_nm.dds");

	if (m_spawn && xrlc_version >= XRLC_VERSION_13) {
		for (xr_entity_vec_cit it = m_spawn->spawns().begin(),
				end = m_spawn->spawns().end(); it != end; ++it) {
			const cse_abstract* entity = *it;
			if (entity->name() == "breakable_object") {
				xr_ogf_v4* ogf = new xr_ogf_v4;
				const char* name = entity->name_replace().c_str();
				msg("loading %s", name);
				if (!ogf->xr_ogf::load_ogf(level_path, name))
					xr_not_expected();
				m_brkbl_meshes.push_back(ogf);
			}
		}
	}

	if (xrlc_version >= XRLC_VERSION_9)
		m_gamemtls_lib = ::load<xr_gamemtls_lib>(game_data_path, "gamemtl.xr", true);

	m_xrlc_version = xrlc_version;
}

void xr_level::clear_ltx() { delete m_ltx; m_ltx = 0; }
void xr_level::clear_geom() { delete m_geom; m_geom = 0; }
void xr_level::clear_geomx() { delete m_geomx; m_geomx = 0; }
void xr_level::clear_visuals() { delete m_visuals; m_visuals = 0; }
void xr_level::clear_shaders() { delete m_shaders; m_shaders = 0; }
void xr_level::clear_sectors() { delete m_sectors; m_sectors = 0; }
void xr_level::clear_portals() { delete m_portals; m_portals = 0; }
void xr_level::clear_lights() { delete m_lights; m_lights = 0; }
void xr_level::clear_glows() { delete m_glows; m_glows = 0; }
void xr_level::clear_cform() { delete m_cform; m_cform = 0; }
void xr_level::clear_hom() { delete m_hom; m_hom = 0; }
void xr_level::clear_details() { delete m_details; m_details = 0; }
void xr_level::clear_ai() { delete m_ai; m_ai = 0; }
void xr_level::clear_game() { delete m_game; m_game = 0; }
void xr_level::clear_spawn() { delete m_spawn; m_spawn = 0; }
void xr_level::clear_wallmarks() { delete m_wallmarks; m_wallmarks = 0; }
void xr_level::clear_som() { delete m_som; m_som = 0; }
void xr_level::clear_snd_env() { delete m_snd_env; m_snd_env = 0; }
void xr_level::clear_snd_static() { delete m_snd_static; m_snd_static = 0; }
void xr_level::clear_ps_static() { delete m_ps_static; m_ps_static = 0; }
void xr_level::clear_env_mod() { delete m_env_mod; m_env_mod = 0; }
void xr_level::clear_fog_vol() { delete m_fog_vol; m_fog_vol = 0; }
void xr_level::clear_build_lights() { delete m_build_lights; m_build_lights = 0; }
void xr_level::clear_lods() { delete m_lods; m_lods = 0; }
void xr_level::clear_lods_nm() { delete m_lods_nm; m_lods_nm = 0; }
void xr_level::clear_brkbl_meshes() { std::vector<xr_ogf*>().swap(m_brkbl_meshes); }
void xr_level::clear_gamemtls_lib() { delete m_gamemtls_lib; m_gamemtls_lib = 0; }
