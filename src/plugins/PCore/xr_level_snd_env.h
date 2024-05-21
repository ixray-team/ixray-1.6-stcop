#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_SND_ENV_H__
#define __XR_LEVEL_SND_ENV_H__

#include <string>
#include <vector>
#include "xr_cform.h"

namespace xray_re {

enum {
	FSL_SND_ENV_IDS		= 0,
	FSL_SND_ENV_GEOMETRY	= 1,
};

class xr_level_snd_env: public xr_cform {
public:
			xr_level_snd_env(xr_reader& r);
	virtual		~xr_level_snd_env();

	void		load(xr_reader& r);
	void		save(xr_writer& w) const;

	const std::vector<std::string>&	env_ids() const;

private:
	std::vector<std::string>	m_env_ids;
};

inline xr_level_snd_env::xr_level_snd_env(xr_reader& r) { load(r); }
inline const std::vector<std::string>& xr_level_snd_env::env_ids() const { return m_env_ids; }

} // end of namespace xray_re

#endif
