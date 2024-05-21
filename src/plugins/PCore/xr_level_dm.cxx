#include "xr_level_dm.h"

using namespace xray_re;

xr_surface* xr_level_dm::create_surface(const xr_raw_surface& raw_surface) const
{
	xr_surface* surface = new xr_surface;
	surface->texture() = texture();
	surface->eshader() = shader();
	surface->set_two_sided();
	return surface;
}
