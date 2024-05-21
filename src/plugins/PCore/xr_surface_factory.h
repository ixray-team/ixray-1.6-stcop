#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SURFACE_FACTORY_H__
#define __XR_SURFACE_FACTORY_H__

namespace xray_re {

class xr_surface;
class xr_raw_surface;

class xr_surface_factory {
public:
	virtual			~xr_surface_factory();
	virtual xr_surface*	create_surface(const xr_raw_surface& raw_surface) const = 0;
};

inline xr_surface_factory::~xr_surface_factory() {}

} // end of namespace xray_re

#endif
