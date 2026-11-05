#include "../kernel/Stdafx.h"
#pragma hdrstop

#include "BlenderListLoader.h"
#include <xr_shaders_xrlc_lib.h>
#include <xr_gamemtls_lib.h>
#include <algorithm>

IC bool str_pred(LPCSTR x, LPCSTR y)
{
	return strcmp(x,y)<0;
}

void ClearList(LPSTR_vec& lst)
{
	for (LPSTR_vec_it it=lst.begin(); it!=lst.end(); it++)
		free(*it);
}

int LoadBlenderList(LPSTR_vec& lst)
{
	ClearList(lst);

	// Load blenders
	char shaders[256];
	string buff;

	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(_game_data_, "shaders.xr");

	xr_reader* ch = r->open_chunk(3);
	xr_assert(ch);
	
	lst.resize(ch->r_u32());
	for (LPSTR_vec_it it = lst.begin(); it != lst.end(); it++)
	{
		ch->r_sz(buff);
		*it = strdup(buff.c_str());
	}
	r->close_chunk(ch);
	fs.r_close(r);

	std::sort(lst.begin(), lst.end(), str_pred);

	return lst.size();
}

int LoadShaderLCList(LPSTR_vec& lst)
{
	ClearList(lst);

	xr_shaders_xrlc_lib LIB;

	LIB.load(_game_data_, "shaders_xrlc.xr");

	lst.resize(LIB.shaders().size());
	LPSTR_vec_it s_it = lst.begin();
	for (xr_shader_xrlc_vec_cit l_it = LIB.shaders().begin(); l_it != LIB.shaders().end(); l_it++, s_it++)
	{
		*s_it = strdup((*l_it)->name.c_str());
	}

	std::sort(lst.begin(), lst.end(), str_pred);

	return lst.size();
}

int LoadGameMtlList(LPSTR_vec& lst)
{
	ClearList(lst);

	xr_gamemtls_lib LIB;

	LIB.load(_game_data_, "gamemtl.xr");

	lst.resize(LIB.materials().size());
	LPSTR_vec_it s_it = lst.begin();
	for (xr_gamemtl_vec_cit l_it = LIB.materials().begin(); l_it != LIB.materials().end(); l_it++, s_it++)
	{
		*s_it = strdup((*l_it)->name.c_str());
	}

	std::sort(lst.begin(), lst.end(), str_pred);

	return lst.size();
}
