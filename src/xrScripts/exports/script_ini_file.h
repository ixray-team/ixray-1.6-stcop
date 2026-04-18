////////////////////////////////////////////////////////////////////////////
//	Module 		: script_ini_file.h
//	Created 	: 21.05.2004
//  Modified 	: 21.05.2004
//	Author		: Dmitriy Iassenev
//	Description : Script ini file class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "script_token_list.h"
#include "../xrScripts/script_export_space.h"

class SCRIPTS_API CScriptIniFile :
	public CInifile 
{
protected:
	typedef CInifile inherited;

public:
						CScriptIniFile		(IReader *F, const char* path=0);
						CScriptIniFile		(const char* szFileName, bool ReadOnly=TRUE, bool bLoadAtStart=TRUE, bool SaveAtEnd=TRUE, const char* path=nullptr);
						CScriptIniFile		(bool read, const char* filepath, const char* game_path, bool loadOnStart = true);

	virtual 			~CScriptIniFile		();
			bool		line_exist			(const char* S, const char* L);
			bool		section_exist		(const char* S);
			int			r_clsid				(const char* S, const char* L);
			bool		r_bool				(const char* S, const char* L);
			int			r_token				(const char* S, const char* L, const CScriptTokenList &token_list);
			const char*		r_string_wb			(const char* S, const char* L);
			const char*		update				(const char* file_name);
			u32			line_count			(const char* S);
			const char*		r_string			(const char* S, const char* L);
			u32			r_u32				(const char* S, const char* L);
			int			r_s32				(const char* S, const char* L);
			float		r_float				(const char* S, const char* L);
			Fvector		r_fvector3			(const char* S, const char* L);

			const char*      update_custom_path(const char* path, const char* file_name);

			void w_bool(const char* S, const char* L, bool V, const char* comment /* = 0 */);
			void w_color(const char* S, const char* L, u32 V, const char* comment /* = 0 */);
			void w_fcolor(const char* S, const char* L, const Fcolor& V, const char* comment /* = 0 */);
			void w_float(const char* S, const char* L, float V, const char* comment /* = 0 */);
			void w_fvector2(const char* S, const char* L, const Fvector2& V, const char* comment /* = 0 */);
			void w_fvector3(const char* S, const char* L, const Fvector3& V, const char* comment /* = 0 */);
			void w_fvector4(const char* S, const char* L, const Fvector4& V, const char* comment /* = 0 */);
			void w_s16(const char* S, const char* L, s16 V, const char* comment /* = 0 */);
			void w_s32(const char* S, const char* L, s32 V, const char* comment /* = 0 */);
			void w_s64(const char* S, const char* L, s64 V, const char* comment /* = 0 */);
			void w_s8(const char* S, const char* L, s8 V, const char* comment /* = 0 */);
			void w_string(const char* S, const char* L, const char* V, const char* comment /* = 0 */);
			void w_u16(const char* S, const char* L, u16 V, const char* comment /* = 0 */);
			void w_u32(const char* S, const char* L, u32 V, const char* comment /* = 0 */);
			void w_u64(const char* S, const char* L, u64 V, const char* comment /* = 0 */);
			void w_u8(const char* S, const char* L, u8 V, const char* comment /* = 0 */);
			bool save_as(const char* new_fname /* = 0 */);
			void save_at_end(bool b);
			void remove_line(const char* S, const char* L);
			void set_override_names(bool b);
			u32 section_count();
			void set_readonly(bool b);
			void close();

			DECLARE_SCRIPT_REGISTER_FUNCTION
};

#include "script_ini_file_inline.h"