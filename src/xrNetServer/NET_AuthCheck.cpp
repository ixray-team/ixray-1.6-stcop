#include "stdafx.h"
#include "NET_AuthCheck.h"

void XRNETSERVER_API fill_auth_check_params(xr_auth_strings_t & ignore,
											xr_auth_strings_t & check)
{
	string_path				config;
	const char* pth				= FS.get_path("$app_data_root$")->m_Path;
	ignore.push_back		(shared_str(pth));
	ignore.push_back		(shared_str(FS.update_path(config, _game_config_, "localization.ltx")));
	ignore.push_back		(shared_str(FS.update_path(config, _game_config_, "fonts.ltx")));
	ignore.push_back		(shared_str(FS.update_path(config, _game_config_, "items.ltx")));
	ignore.push_back		(shared_str(FS.update_path(config, _game_config_, "text")));
	ignore.push_back		(shared_str(FS.update_path(config, _game_config_, "gameplay")));
	ignore.push_back		(shared_str(FS.update_path(config, _game_config_, "ui")));
	ignore.push_back		(shared_str(FS.update_path(config, _game_config_, "scripts")));
	ignore.push_back		(shared_str(FS.update_path(config, _game_config_, "misc\\script_sound_pripyat.ltx")));		
	ignore.push_back		(shared_str(FS.update_path(config, "$game_scripts$", "state_mgr_pri_a15.script")));

	check.push_back			(shared_str(FS.update_path(config, _game_config_, "")));
	check.push_back			(shared_str(FS.update_path(config, "$game_scripts$", "")));
	check.push_back			(shared_str(FS.update_path(config, _game_shaders_, "")));
	//sounds 
	check.push_back			(shared_str(FS.update_path(config, _game_sounds_, "material")));
	check.push_back			(shared_str(FS.update_path(config, _game_sounds_, "weapons")));

	// check scopes
	check.push_back			(shared_str(FS.update_path(config, _game_textures_, "wpn\\wpn_crosshair.dds")));
	check.push_back			(shared_str(FS.update_path(config, _game_textures_, "wpn\\wpn_crosshair_bino.dds")));
	check.push_back			(shared_str(FS.update_path(config, _game_textures_, "wpn\\wpn_crosshair_g36.dds")));
	check.push_back			(shared_str(FS.update_path(config, _game_textures_, "wpn\\wpn_crosshair_l85.dds")));
	check.push_back			(shared_str(FS.update_path(config, _game_textures_, "wpn\\wpn_crosshair_rpg.dds")));

	check.push_back			(shared_str("xrd3d9-null.dll"));
	check.push_back			(shared_str("ode.dll"));
	check.push_back			(shared_str("xrcdb.dll"));
	check.push_back			(shared_str("xrcore.dll"));
//	check.push_back			(shared_str("xrcpu_pipe.dll"));
//	check.push_back			(shared_str("xrgame.dll"));
	check.push_back			(shared_str("xrgamespy.dll"));
	check.push_back			(shared_str("xrlua.dll"));
	check.push_back			(shared_str("xrnetserver.dll"));
	check.push_back			(shared_str("xrparticles.dll"));
	check.push_back			(shared_str("xrrender_r1.dll"));
	check.push_back			(shared_str("xrrender_r2.dll"));
	check.push_back			(shared_str("xrsound.dll"));
	check.push_back			(shared_str("xrxmlparser.dll"));
//	check.push_back			(shared_str("xrEngine.exe"));
}

bool XRNETSERVER_API allow_to_include_path	(xr_auth_strings_t const & ignore,
											 const char* path)
{
	for (xr_auth_strings_t::const_iterator i = ignore.begin(),
		ie = ignore.end(); i != ie; ++i)
	{
		if (!strncmp(i->c_str(), path, i->size()))
			return false;
	}
	return true;
}