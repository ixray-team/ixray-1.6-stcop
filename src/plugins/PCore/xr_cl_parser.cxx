#include <climits>
#include <cstring>
#include <cstdlib>
#include "xr_cl_parser.h"

cl_parser::cl_parser(): m_options(0),
	m_num_options(0), m_num_params(0) {}

cl_parser::~cl_parser()
{
	delete[] m_options;
}

bool cl_parser::parse(int argc, const char* const argv[],
		int optc, const option_desc* optv)
{
	m_options = new option[argc];
	m_argc = size_t(argc & INT_MAX);

	size_t opt_idx = 0, param_idx = size_t(argc & INT_MAX) - 1;
	for (++argv; argc > 1; ++argv, --argc) {
		if ((*argv)[0] != '-') {
			m_options[param_idx].name = *argv;
			--param_idx;
			++m_num_params;
			continue;
		}
		bool valid = false;
		const option_desc* desc = optv;
		for (int j = optc; j > 0; --j, ++desc) {
			if (std::strcmp(*argv, desc->name) != 0)
				continue;
			valid = true;
			option& opt = m_options[opt_idx++];
			opt.name = desc->name;
			opt.type = desc->type;
			if (opt.type == OT_BOOL) {
				opt.v_bool = true;
			} else if (--argc <= 1) {
				return false;
			} else {
				++argv;
				switch (opt.type) {
				case OT_STRING:
					opt.v_string = *argv;
					break;
				case OT_INTEGER:
					opt.v_integer = std::atoi(*argv);
					break;
				case OT_FLOAT:
					opt.v_float = float(std::atof(*argv));
					break;
				default:
					return false;
				}
			}
			break;
		}
		if (!valid)
			return false;
	}
	m_num_options = opt_idx;
	return true;
}

const cl_parser::option* cl_parser::find_option(const char* name) const
{
	for (const option *opt = m_options, *end = opt + m_num_options;
			opt != end; ++opt) {
		if (std::strcmp(opt->name, name) == 0)
			return opt;
	}
	return 0;
}

bool cl_parser::get_bool(const char* name) const
{
	const option* opt = find_option(name);
	return opt && opt->type == OT_BOOL;
}

bool cl_parser::get_string(const char* name, const char*& value) const
{
	const option* opt = find_option(name);
	if (opt && opt->type == OT_STRING) {
		value = opt->v_string;
		return true;
	}
	return false;
}

bool cl_parser::get_string(const char* name, std::string& value) const
{
	const option* opt = find_option(name);
	if (opt && opt->type == OT_STRING) {
		value = opt->v_string;
		return true;
	}
	return false;
}

bool cl_parser::get_integer(const char* name, int& value) const
{
	const option* opt = find_option(name);
	if (opt && opt->type == OT_INTEGER) {
		value = opt->v_integer;
		return true;
	}
	return false;
}
