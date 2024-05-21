#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_NAME_GEN_H__
#define __XR_NAME_GEN_H__

#include "xr_types.h"

namespace xray_re {

class xr_name_gen {
public:
			xr_name_gen();
			xr_name_gen(const char* pattern, bool mimic_gsc = true);
			~xr_name_gen();

	void		init(const char* pattern, bool mimic_gsc = true);

	const char*	get() const;
	void		next();

private:
	char*		m_name;
	int		m_next_idx;
	unsigned	m_offset;
};

inline xr_name_gen::xr_name_gen(): m_name(0) {}

inline xr_name_gen::xr_name_gen(const char* pattern, bool mimic_gsc)
{
	init(pattern, mimic_gsc);
}

inline xr_name_gen::~xr_name_gen() { delete m_name; }

inline const char* xr_name_gen::get() const { return m_name; }

} // end of namespace xray_re

#endif
