////////////////////////////////////////////////////////////////////////////
//	Module 		: script_ini_file.cpp
//	Created 	: 21.05.2004
//  Modified 	: 21.05.2004
//	Author		: Dmitriy Iassenev
//	Description : Script ini file class
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "script_ini_file.h"
#include "../xrScripts/script_engine.h"
#include "../xrSound/ai_sounds.h"
#include "../xrEngine/IGame_ObjectFactory.h"

CScriptIniFile::CScriptIniFile(IReader *F, const char* path) :
	inherited	(F,path)
{
}

CScriptIniFile::CScriptIniFile(const char* szFileName, bool ReadOnly, bool bLoadAtStart, bool SaveAtEnd, const char* path) :
	inherited(path ? path : update(szFileName), ReadOnly, bLoadAtStart, SaveAtEnd)
{
}

CScriptIniFile::CScriptIniFile(bool read, const char* filepath, const char* game_path, bool loadOnStart)
    : inherited(update_custom_path(game_path, filepath), read, loadOnStart)
{
}

CScriptIniFile::~CScriptIniFile		()
{
}

const char*	CScriptIniFile::update		(const char* file_name)
{
	string_path			S1;
	FS.update_path		(S1,_game_config_,file_name);
	return				(*shared_str(S1));
}

bool CScriptIniFile::line_exist		(const char* S, const char* L)
{
	return		(!!inherited::line_exist(S,L));
}

bool CScriptIniFile::section_exist	(const char* S)
{
	return		(!!inherited::section_exist(S));
}

int	 CScriptIniFile::r_clsid		(const char* S, const char* L)
{
	return		(g_object_factory->script_clsid(inherited::r_clsid(S,L)));
}

bool CScriptIniFile::r_bool			(const char* S, const char* L)
{
	return		(!!inherited::r_bool(S,L));
}

int	 CScriptIniFile::r_token		(const char* S, const char* L, const CScriptTokenList &token_list)
{
	return		(inherited::r_token(S,L,&*token_list.tokens().begin()));
}

const char* CScriptIniFile::r_string_wb	(const char* S, const char* L)
{
	return		(*inherited::r_string_wb(S,L));
}

u32	 CScriptIniFile::line_count			(const char* S)
{
	VERIFY3		(inherited::section_exist(S),"Cannot find section",S);
	return		(inherited::line_count(S));
}

const char* CScriptIniFile::r_string			(const char* S, const char* L)
{
	VERIFY3		(inherited::section_exist(S),"Cannot find section",S);
	VERIFY3		(inherited::line_exist(S,L),"Cannot find line",L);
	return		(inherited::r_string(S,L));
}

u32	 CScriptIniFile::r_u32				(const char* S, const char* L)
{
	VERIFY3		(inherited::section_exist(S),"Cannot find section",S);
	VERIFY3		(inherited::line_exist(S,L),"Cannot find line",L);
	return		(inherited::r_u32(S,L));
}

int	 CScriptIniFile::r_s32				(const char* S, const char* L)
{
	VERIFY3		(inherited::section_exist(S),"Cannot find section",S);
	VERIFY3		(inherited::line_exist(S,L),"Cannot find line",L);
	return		(inherited::r_s32(S,L));
}

float CScriptIniFile::r_float			(const char* S, const char* L)
{
	VERIFY3		(inherited::section_exist(S),"Cannot find section",S);
	VERIFY3		(inherited::line_exist(S,L),"Cannot find line",L);
	return		(inherited::r_float(S,L));
}

Fvector CScriptIniFile::r_fvector3		(const char* S, const char* L)
{
	VERIFY3		(inherited::section_exist(S),"Cannot find section",S);
	VERIFY3		(inherited::line_exist(S,L),"Cannot find line",L);
	return		(inherited::r_fvector3(S,L));
}

void CScriptIniFile::set_override_names(bool b)
{
	inherited::set_override_names(b);
}

bool CScriptIniFile::save_as(const char* new_fname)
{
	VERIFY2(new_fname, "File name is null");
	return(inherited::save_as(new_fname));
}

void CScriptIniFile::set_readonly(bool b)
{
	inherited::m_flags.set(eReadOnly, b);
}

void CScriptIniFile::w_bool(const char* S, const char* L, bool V, const char* comment)
{
    VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_bool(S, L, V, comment);
}

void CScriptIniFile::w_color(const char* S, const char* L, u32 V, const char* comment)
{
    VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_color(S, L, V, comment);
}

void CScriptIniFile::w_fcolor(const char* S, const char* L, const Fcolor& V, const char* comment)
{
    VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_fcolor(S, L, V, comment);
}

void CScriptIniFile::w_float(const char* S, const char* L, float V, const char* comment)
{
    VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_float(S, L, V, comment);
}

void CScriptIniFile::w_fvector2(const char* S, const char* L, const Fvector2& V, const char* comment)
{
    VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_fvector2(S, L, V, comment);
}

void CScriptIniFile::w_fvector3(const char* S, const char* L, const Fvector3& V, const char* comment)
{
   // VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    //VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_fvector3(S, L, V, comment);
}

void CScriptIniFile::w_fvector4(const char* S, const char* L, const Fvector4& V, const char* comment)
{
    //VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    //VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_fvector4(S, L, V, comment);
}

void CScriptIniFile::w_s16(const char* S, const char* L, s16 V, const char* comment)
{
    //VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    //VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_s16(S, L, V, comment);
}

void CScriptIniFile::w_s32(const char* S, const char* L, s32 V, const char* comment)
{
    //VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    //VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_s32(S, L, V, comment);
}

void CScriptIniFile::w_s64(const char* S, const char* L, s64 V, const char* comment)
{
    //VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    //VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_s64(S, L, V, comment);
}

void CScriptIniFile::w_s8(const char* S, const char* L, s8 V, const char* comment)
{
	//VERIFY3(inherited::section_exist(S), "Cannot find section", S);
	//VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_s8(S, L, V, comment);
}

void CScriptIniFile::w_string(const char* S, const char* L, const char* V, const char* comment)
{
    //VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    //VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_string(S, L, V, comment);
}

void CScriptIniFile::w_u16(const char* S, const char* L, u16 V, const char* comment)
{
   // VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    //VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_u16(S, L, V, comment);
}

void CScriptIniFile::w_u32(const char* S, const char* L, u32 V, const char* comment)
{
    //VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    //VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_u32(S, L, V, comment);
}

void CScriptIniFile::w_u64(const char* S, const char* L, u64 V, const char* comment)
{
    VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_u64(S, L, V, comment);
}

void CScriptIniFile::w_u8(const char* S, const char* L, u8 V, const char* comment)
{
    VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::w_u8(S, L, V, comment);
}

void CScriptIniFile::save_at_end(bool b)
{
    inherited::save_at_end(b);
}

void CScriptIniFile::remove_line(const char* S, const char* L)
{
    VERIFY3(inherited::section_exist(S), "Cannot find section", S);
    VERIFY3(inherited::line_exist(S, L), "Cannot find line", L);
    inherited::remove_line(S, L);
}

void CScriptIniFile::close()
{
    inherited::Destroy(this);
}

u32 CScriptIniFile::section_count()
{
    return (inherited::section_count());
}

const char* CScriptIniFile::update_custom_path(const char* path, const char* file_name)
{
    string_path S1 = {};
    FS.update_path(S1, path, file_name);
    return (*shared_str(S1));
}