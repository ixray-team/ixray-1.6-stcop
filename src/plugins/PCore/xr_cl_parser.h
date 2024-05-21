#ifndef __GNUC__
#pragma once
#endif
#ifndef __CL_PARSER_H__
#define __CL_PARSER_H__

#include <string>

// depends on the argv persistence.
class cl_parser {
public:
	enum option_type {
		OT_STRING,
		OT_BOOL,
		OT_FLOAT,
		OT_INTEGER,
		OT_PARAMETER,
	};
	struct option_desc {
		const char*	name;
		option_type	type;
	};

			cl_parser();
			~cl_parser();

	bool		parse(int argc, const char* const argv[],
					int optc, const option_desc* optv);
	bool		get_bool(const char* name) const;
	bool		get_string(const char* name, const char*& value) const;
	bool		get_string(const char* name, std::string& value) const;
	bool		get_float(const char* name, float& value) const;
	bool		get_integer(const char* name, int& value) const;
	bool		exist(const char* name) const;

	const char*	param(size_t index) const;
	size_t		num_options() const;
	size_t		num_params() const;

private:
	struct option {
		const char*	name;
		option_type	type;
		union {
			const char*	v_string;
			int		v_integer;
			float		v_float;
			bool		v_bool;
		};
	};

	const option*	find_option(const char* name) const;

private:
	option*		m_options;
	size_t		m_num_options;
	size_t		m_num_params;
	size_t		m_argc;
};

inline const char* cl_parser::param(size_t index) const { return m_options[m_argc - index - 1].name; }

inline size_t cl_parser::num_options() const { return m_num_options; }
inline size_t cl_parser::num_params() const { return m_num_params; }

inline bool cl_parser::exist(const char* name) const { return find_option(name) != 0; }

#endif
