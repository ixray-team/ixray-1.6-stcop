////////////////////////////////////////////////////////////////////////////
//	Module 		: script_reader_script.cpp
//	Created 	: 05.10.2004
//  Modified 	: 05.10.2004
//	Author		: Dmitriy Iassenev
//	Description : Script reader
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "script_reader.h"
#include <script_engine.h>

using namespace luabind;

static bool r_eof_semi(IReader *self_)
{
	return			(!!self_->eof());
}

static LPCSTR r_stringZ_semi(IReader *self_)
{
	shared_str		temp;
	self_->r_stringZ	(temp);
	return			(*temp);
}

static bool r_bool_semi(IReader *self_)
{
	return			(!!self_->r_u8());
}

static void r_fvector3_semi(IReader *self_, Fvector *arg0)
{
	self_->r_fvector3(*arg0);
}

static luabind::internal_string r_file_as_string(const char* path)
{
	IReader* reader = FS.r_open(path);

	luabind::internal_string result;
	result.resize(reader->length());
	std::memcpy(result.data(), reader->pointer(), sizeof(char) * reader->length());
	FS.r_close(reader);

	return std::move(result);
}

static void w_file_from_string(const char* path, const char* buffer)
{
	auto fileIter = FS.exist(path);
	if (fileIter == nullptr)
	{
		lua_pushfstring(g_pScriptEngine->lua(), "Not found file: %s", path);
		lua_error(g_pScriptEngine->lua());

		return;
	}

	shared_str newPath = fileIter->wrap ? fileIter->wrap : fileIter->name;
	IWriter* writer = FS.w_open(*newPath);
	writer->w(buffer, xr_strlen(buffer));
	FS.w_close(writer);
}

#pragma optimize("s",on)
void CScriptReader::script_register(lua_State *L)
{
	module(L)
	[
		class_<IReaderBase>("reader_base")
			.def("r_float",			&IReader::r_float		)
			.def("r_u64",			&IReader::r_u64			)
			.def("r_s64",			&IReader::r_s64			)
			.def("r_u32",			&IReader::r_u32			)
			.def("r_s32",			&IReader::r_s32			)
			.def("r_u16",			&IReader::r_u16			)
			.def("r_s16",			&IReader::r_s16			)
			.def("r_u8",			&IReader::r_u8			)
			.def("r_s8",			&IReader::r_s8			)
			.def("r_float_q16",		&IReader::r_float_q16	)
			.def("r_float_q8",		&IReader::r_float_q8	)
			.def("r_angle16",		&IReader::r_angle16		)
			.def("r_angle8",		&IReader::r_angle8		)
			.def("r_dir",			&IReader::r_dir			)
			.def("r_sdir",			&IReader::r_sdir		),

		class_<IReader, IReaderBase>("reader")
			.def("r_seek",			&IReader::seek			)
			.def("r_tell",			&IReader::tell			)
			.def("r_vec3",			&::r_fvector3_semi		)
			.def("r_bool",			&r_bool_semi			)
			.def("r_stringZ",		&r_stringZ_semi			)
			.def("r_elapsed",		&IReader::elapsed		)
			.def("r_advance",		&IReader::advance		)
			.def("r_eof",			&r_eof_semi				),

		def("r_file_as_string",			&r_file_as_string)
		//def("w_file_from_string",		&w_file_from_string)
	];
}
