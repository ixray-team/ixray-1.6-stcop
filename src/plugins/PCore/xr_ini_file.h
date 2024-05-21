#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_INI_FILE_H__
#define __XR_INI_FILE_H__

#include <vector>
#include <string>
#include "xr_types.h"

namespace xray_re {

class xr_reader;

class xr_ini_file {
public:
			xr_ini_file();
			xr_ini_file(const char* path);
			xr_ini_file(const char* path, const char* name);
			~xr_ini_file();

	bool		load(xr_reader& r);
	bool		load(const char* path);
	bool		load(const std::string& path);
	bool		load(const char* path, const char* name);

	void		clear();

	static bool	is_true(const char* s);

	bool		line_exist(const char* sname, const char* lname) const;
	size_t		line_count(const char* sname) const;
	bool		section_exist(const char* sname) const;
	uint64_t	r_clsid(const char* sname, const char* lname) const;
	const char*	r_string(const char* sname, const char* lname) const;
	bool		r_bool(const char* sname, const char* lname) const;
	float		r_float(const char* sname, const char* lname) const;
	bool		r_line(const char* sname, size_t lindex, const char** lname, const char** lvalue) const;

	bool		empty() const;

private:
	struct ini_item {
					ini_item(std::string& _name);
		bool			operator<(const ini_item& right) const;

		std::string		name;
		std::string		value;
	};
	TYPEDEF_STD_VECTOR_PTR(ini_item)

	struct ini_section {
					ini_section(const char* _name);
					~ini_section();

		bool			line_exist(const char* lname, const char** lvalue) const;
		void			merge(const ini_section* section);
		size_t			size() const;
		ini_item_vec_it		begin();
		ini_item_vec_cit	begin() const;
		ini_item_vec_it		end();
		ini_item_vec_cit	end() const;

		std::string		name;
		ini_item_vec		items;
	};
	TYPEDEF_STD_VECTOR_PTR(ini_section)

	struct ini_item_pred;
	struct ini_section_pred;

	const ini_section*	r_section(const char* sname) const;
	bool			parse(const char* p, const char* end, const char* path);
	bool			load_include(const char* path);

private:
	ini_section_vec		m_sections;
};

inline xr_ini_file::xr_ini_file() {}
inline xr_ini_file::xr_ini_file(const char* path) { load(path); }
inline xr_ini_file::xr_ini_file(const char* path, const char* name) { load(path, name); }

inline bool xr_ini_file::empty() const { return m_sections.empty(); }

inline bool xr_ini_file::load(const std::string& path) { return load(path.c_str()); }

inline xr_ini_file::ini_item::ini_item(std::string& _name) { name.swap(_name); }

inline xr_ini_file::ini_section::ini_section(const char* _name): name(_name) {}
inline size_t xr_ini_file::ini_section::size() const { return items.size(); }
inline xr_ini_file::ini_item_vec_it xr_ini_file::ini_section::begin() { return items.begin(); }
inline xr_ini_file::ini_item_vec_cit xr_ini_file::ini_section::begin() const { return items.begin(); }
inline xr_ini_file::ini_item_vec_it xr_ini_file::ini_section::end() { return items.end(); }
inline xr_ini_file::ini_item_vec_cit xr_ini_file::ini_section::end() const { return items.end(); }

} // end of namespace xray_re

#endif
