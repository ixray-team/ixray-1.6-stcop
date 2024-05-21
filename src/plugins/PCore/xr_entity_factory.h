#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_ENTITY_FACTORY_H__
#define __XR_ENTITY_FACTORY_H__

#include <string>
#include "xr_entity.h"

namespace xray_re {

void load_system_ini(const char* game_config);

xr_clsid* get_entity_clsid(const char* name);

cse_abstract* create_entity(const char* name);

static inline cse_abstract* create_entity(const std::string& name)
{
	return create_entity(name.c_str());
}

static inline xr_clsid* get_entity_clsid(const std::string& name)
{
	return get_entity_clsid(name.c_str());
}


} // end of namespace xray_re

#endif
