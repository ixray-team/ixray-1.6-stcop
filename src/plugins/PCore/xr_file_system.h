#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_FILE_SYSTEM_H__
#define __XR_FILE_SYSTEM_H__

#include <string>
#include <vector>
#include "xr_types.h"
#include "xr_reader.h"
#include "xr_writer.h"

namespace xray_re {

class xr_file_system {
public:
	enum {
		FSF_READ_ONLY	= 0x1,
	};

			xr_file_system();
			~xr_file_system();

	static xr_file_system&	instance();

	bool		initialize(const char* fs_spec, unsigned flags = 0);

	bool		read_only() const;

	xr_reader*	r_open(const char* path) const;
	xr_reader*	r_open(const std::string& path) const;
	xr_reader*	r_open(const char* path, const char* name) const;
	xr_reader*	r_open(const char* path, const std::string& name) const;
	void		r_close(xr_reader*& r) const;

	xr_writer*	w_open(const char* path, bool ignore_ro = false) const;
	xr_writer*	w_open(const std::string& path) const;
	xr_writer*	w_open(const char* path, const char* name) const;
	xr_writer*	w_open(const char* path, const std::string& name) const;
	void		w_close(xr_writer*& w) const;

	bool		copy_file(const char* src_path, const char* src_name,
					const char* tgt_path, const char* tgt_name = 0) const;
	bool		copy_file(const char* src_path, const char* tgt_path) const;
	bool		copy_file(const std::string& src_path, const std::string& tgt_path) const;

	static size_t	file_length(const char* path);
	static size_t	file_length(const std::string& path);
	size_t		file_length(const char* path, const char* name) const;
	size_t		file_length(const char* path, const std::string& name) const;

	static uint32_t	file_age(const char* path);
	static uint32_t	file_age(const std::string& path);
	uint32_t	file_age(const char* path, const char* name) const;
	uint32_t	file_age(const char* path, const std::string& name) const;

	static bool	file_exist(const char* path);
	static bool	file_exist(const std::string& path);
	bool		file_exist(const char* path, const char* name) const;
	bool		file_exist(const char* path, const std::string& name) const;

	static bool	folder_exist(const char* path);
	static bool	folder_exist(const std::string& path);
	bool		folder_exist(const char* path, const char* name) const;
	bool		folder_exist(const char* path, const std::string& name) const;

	bool		create_path(const char* path) const;
	bool		create_path(const std::string& path) const;

	bool		create_folder(const char* path) const;
	bool		create_folder(const std::string& path) const;
	bool		create_folder(const char* path, const char* name) const;
	bool		create_folder(const char* path, const std::string& name) const;

	const char*	resolve_path(const char* path) const;
	bool		resolve_path(const char* path, const char* name, std::string& full_path) const;
	bool		resolve_path(const char* path, const std::string& name, std::string& full_path) const;

	void		update_path(const char* path, const std::string& root, const std::string& add);
	void		update_path(const char* path, const char* root, const std::string& add);
	void		update_path(const char* path, const char* root, const char* add);

	static void	append_path_separator(std::string& path);
	static void	split_path(const std::string& path, std::string* folder = 0,
					std::string* name = 0, std::string* extension = 0);
	static void	split_path(const char* path, std::string* folder = 0,
					std::string* name = 0, std::string* extension = 0);

protected:
	struct path_alias {
		std::string	path;
		std::string	root;
		std::string	filter;
		std::string	caption;
	};
	TYPEDEF_STD_VECTOR_PTR(path_alias)

	const path_alias*	find_path_alias(const char* path) const;
	path_alias*		add_path_alias(const std::string& path,
					const std::string& root, const std::string& add);
	bool			parse_fs_spec(xr_reader& r);

	void			working_folder(std::string& folder);

private:
	path_alias_vec	m_aliases;
	unsigned	m_flags;
};

const char PA_FS_ROOT[] = "$fs_root$";
const char PA_SDK_ROOT[] = "$sdk_root$";
const char PA_GAME_DATA[] = "$game_data$";
const char PA_GAME_CONFIG[] = "$game_config$";
const char PA_GAME_SCRIPTS[] = "$game_scripts$";
const char PA_GAME_MESHES[] = "$game_meshes$";
const char PA_GAME_TEXTURES[] = "$game_textures$";
const char PA_GAME_LEVELS[] = "$game_levels$";
const char PA_GAME_SPAWN[] = "$game_spawn$";
const char PA_GAME_SOUNDS[] = "$game_sounds$";
const char PA_LEVEL[] = "$level$";
const char PA_LOGS[] = "$logs$";
const char PA_SOUNDS[] = "$sounds$";
const char PA_TEXTURES[] = "$textures$";
const char PA_OBJECTS[] = "$objects$";
const char PA_CLIPS[] = "$clips$";
const char PA_MAPS[] = "$maps$";
const char PA_GROUPS[] = "$groups$";
const char PA_TEMP[] = "$temp$";
const char PA_IMPORT[] = "$import$";
const char PA_DETAIL_OBJECTS[] = "$detail_objects$";

inline xr_file_system& xr_file_system::instance()
{
	static xr_file_system instance0;
	return instance0;
}

inline bool xr_file_system::read_only() const { return !!(m_flags & FSF_READ_ONLY); }
inline xr_reader* xr_file_system::r_open(const std::string& path) const { return r_open(path.c_str()); }
inline xr_reader* xr_file_system::r_open(const char* path, const std::string& name) const
{
	return r_open(path, name.c_str());
}
inline xr_writer* xr_file_system::w_open(const std::string& path) const { return w_open(path.c_str()); }
inline xr_writer* xr_file_system::w_open(const char* path, const std::string& name) const
{
	return w_open(path, name.c_str());
}
inline void xr_file_system::r_close(xr_reader*& r) const { delete r; r = 0; }
inline void xr_file_system::w_close(xr_writer*& w) const { delete w; w = 0; }

inline size_t xr_file_system::file_length(const std::string& path) { return file_length(path.c_str()); }

inline uint32_t xr_file_system::file_age(const std::string& path) { return file_age(path.c_str()); }
inline uint32_t xr_file_system::file_age(const char* path, const std::string& name) const
{
	return file_age(path, name.c_str());
}

inline bool xr_file_system::folder_exist(const std::string& path) { return folder_exist(path.c_str()); }
inline bool xr_file_system::folder_exist(const char* path, const std::string& name) const
{
	return folder_exist(path, name.c_str());
}

inline bool xr_file_system::file_exist(const std::string& path) { return file_exist(path.c_str()); }
inline bool xr_file_system::file_exist(const char* path, const std::string& name) const
{
	return file_exist(path, name.c_str());
}

inline bool xr_file_system::create_folder(const std::string& path) const
{
	return create_folder(path.c_str());
}
inline bool xr_file_system::create_folder(const char* path, const std::string& name) const
{
	return create_folder(path, name.c_str());
}

inline bool xr_file_system::create_path(const std::string& path) const
{
	return create_path(path.c_str());
}

inline bool xr_file_system::copy_file(const std::string& src_path, const std::string& tgt_path) const
{
	return copy_file(src_path.c_str(), tgt_path.c_str());
}

inline bool xr_file_system::resolve_path(const char* path, const std::string& name, std::string& full_path) const
{
	return resolve_path(path, name.c_str(), full_path);
}

inline void xr_file_system::split_path(const std::string& path, std::string* folder,
		std::string* name, std::string* extension)
{
	split_path(path.c_str(), folder, name, extension);
}

inline void xr_file_system::update_path(const char* path, const std::string& root, const std::string& add)
{
	update_path(path, root.c_str(), add.c_str());
}

inline void xr_file_system::update_path(const char* path, const char* root, const std::string& add)
{
	update_path(path, root, add.c_str());
}

} // end of namespace xray_re

#endif
