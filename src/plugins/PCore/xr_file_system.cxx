#include <cctype>
#include "xr_file_system.h"
#include "xr_utils.h"

using namespace xray_re;

xr_file_system::xr_file_system(): m_flags(0) {}

xr_file_system::~xr_file_system()
{
	delete_elements(m_aliases);
}

const xr_file_system::path_alias* xr_file_system::find_path_alias(const char* path) const
{
	for (path_alias_vec_cit it = m_aliases.begin(), end = m_aliases.end(); it != end; ++it) {
		if ((*it)->path == path)
			return *it;
	}
	return 0;
}

void xr_file_system::append_path_separator(std::string& path)
{
	if (!path.empty() && *(path.end()-1) != '\\')
		path += '\\';
}

void xr_file_system::update_path(const char* path, const char* root, const char* add)
{
	path_alias* new_pa;
	for (path_alias_vec_cit it = m_aliases.begin(), end = m_aliases.end(); it != end; ++it) {
		if ((*it)->path == path) {
			new_pa = *it;
			goto found_or_created;
		}
	}
	new_pa = new path_alias;
	new_pa->path = path;
	m_aliases.push_back(new_pa);

found_or_created:
	const path_alias* pa = find_path_alias(root);
	if (pa) {
		new_pa->root = pa->root;
	} else {
		new_pa->root = root;
		append_path_separator(new_pa->root);
	}
	new_pa->root += add;
	append_path_separator(new_pa->root);
}

xr_file_system::path_alias* xr_file_system::add_path_alias(const std::string& path,
		const std::string& root, const std::string& add)
{
	const path_alias* pa = find_path_alias(path.c_str());
	assert(pa == 0);
	if (pa != 0)
		return 0;

	path_alias* new_pa = new path_alias;
	m_aliases.push_back(new_pa);
	new_pa->path = path;
	pa = find_path_alias(root.c_str());
	if (pa) {
		new_pa->root = pa->root;
	} else {
		new_pa->root = root;
		append_path_separator(new_pa->root);
	}
	new_pa->root += add;
	append_path_separator(new_pa->root);
	return new_pa;
}

static inline const char* next_line(const char* p, const char* end)
{
	while (p < end && *p++ != '\n') {}
	return p;
}

static inline const char* read_alias(const char* p, const char* end)
{
	if (p >= end || *p++ != '$')
		return 0;
	if (p >= end || (!std::isalnum(*p) && *p != '_'))
		return 0;
	for (++p; p < end;) {
		int c = *p;
		if (c == '$')
			return p + 1;
		else if (!std::isalnum(c) && c != '_')
			break;
		++p;
	}
	return 0;
}

static inline const char* skip_ws(const char* p, const char* end)
{
	while (p < end) {
		int c = *p;
		if (c != ' ' && c != '\t')
			break;
		++p;
	}
	return p;
}

static inline const char* read_value(const char*& _p, const char* end)
{
	const char* p = skip_ws(_p, end);
	_p = p;
	const char* last_ws = 0;
	while (p < end) {
		int c = *p;
		if (c == ' ' || c =='\t') {
			if (last_ws == 0)
				last_ws = p;
		} else if (c == '\n' || c == '\r' || c == '|') {
			if (last_ws == 0)
				last_ws = p;
			break;
		} else {
			last_ws = 0;
		}
		++p;
	}
	return last_ws ? last_ws : p;
}

bool xr_file_system::parse_fs_spec(xr_reader& r)
{
	const char* p = r.pointer<const char>();
	const char* end = p + r.size();
	std::string alias, values[4];
	for (unsigned line = 1; p < end; p = next_line(p, end), ++line) {
		int c = *p;
		if (c == '$') {
			const char* last = read_alias(p, end);
			if (last == 0) {
				msg("can't parse line %u", line);
				return false;
			}
			alias.assign(p, last);

			p = skip_ws(last, end);
			if (p == end || *p++ != '=') {
				msg("can't parse line %u", line);
				return false;
			}

			int i;
			for (i = -2; i < 4;) {
				last = read_value(p, end);
				if (i < 0 && (last == end || *last != '|')) {
					msg("can't parse line %u", line);
					return false;
				}
				if (i >= 0)
					values[i].assign(p, last);
				p = last + 1;
				++i;
				if (p == end || *last != '|')
					break;
			}
			assert(i > 0);
			if (i < 2)
				values[1].clear();
			path_alias* pa = add_path_alias(alias, values[0], values[1]);
			if (pa == 0) {
				msg("can't parse line %u", line);
				return false;
			}
			if (i > 2)
				pa->filter = values[2];
			if (i > 3)
				pa->caption = values[3];
		} else if (c != ';' && !std::isspace(c)) {
			msg("can't parse line %u", line);
			return false;
		}
	}
	return true;
}

bool xr_file_system::initialize(const char* fs_spec, unsigned flags)
{
	if (fs_spec && fs_spec[0] != '\0') {
		xr_reader* r = r_open(fs_spec);
		if (r == 0)
			return false;
			
		std::string folder;
		split_path(fs_spec, &folder);
		add_path_alias(PA_FS_ROOT, folder, "");
		if (!parse_fs_spec(*r))
			this->m_aliases.clear();
		r_close(r);
	} else {
#if 0
		std::string folder;
		get_working_folder(folder);
#endif
		add_path_alias(PA_FS_ROOT, "", "");
	}
	m_flags = flags;
	return !m_aliases.empty();
}

const char* xr_file_system::resolve_path(const char* path) const
{
	const path_alias* pa = find_path_alias(path);
	return pa != 0 ? pa->root.c_str() : 0;
}

bool xr_file_system::resolve_path(const char* path, const char* name, std::string& full_path) const
{
	const path_alias* pa = find_path_alias(path);
	if (pa == 0)
		return false;
	full_path = pa->root;
	if (name)
		full_path.append(name);
	return true;
}

xr_reader* xr_file_system::r_open(const char* path, const char* name) const
{
	const path_alias* pa = find_path_alias(path);
	return pa ? r_open(pa->root + name) : 0;
}

xr_writer* xr_file_system::w_open(const char* path, const char* name) const
{
	const path_alias* pa = find_path_alias(path);
	return pa ? w_open(pa->root + name) : nullptr;
}

bool xr_file_system::folder_exist(const char* path, const char* name) const
{
	const path_alias* pa = find_path_alias(path);
	return pa ? folder_exist(pa->root + name) : false;
}

bool xr_file_system::file_exist(const char* path, const char* name) const
{
	const path_alias* pa = find_path_alias(path);
	return pa ? file_exist(pa->root + name) : false;
}

uint32_t xr_file_system::file_age(const char* path, const char* name) const
{
	const path_alias* pa = find_path_alias(path);
	return pa ? file_age(pa->root + name) : 0;
}

bool xr_file_system::copy_file(const char* src_path, const char* src_name,
		const char* tgt_path, const char* tgt_name) const
{
	const path_alias* src_pa = find_path_alias(src_path);
	if (src_pa == 0)
		return false;
	const path_alias* tgt_pa = find_path_alias(tgt_path);
	if (tgt_pa == 0)
		return false;
	if (tgt_name == 0)
		tgt_name = src_name;
	return copy_file(src_pa->root + src_name, tgt_pa->root + tgt_name);
}

bool xr_file_system::create_folder(const char* path, const char* name) const
{
	const path_alias* pa = find_path_alias(path);
	if (pa == 0)
		return false;
	if (read_only()) {
		dbg("fs_ro: creating folder %s%s", pa->root.c_str(), name);
		return true;
	}
	return create_folder(pa->root + name);
}
