#include "xr_guid.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

void xr_guid::load(xr_reader& r)
{
	g[0] = r.r_u32();
	g[1] = r.r_u32();
	g[2] = r.r_u32();
	g[3] = r.r_u32();
}

void xr_guid::save(xr_writer& w) const
{
	w.w_u32(g[0]);
	w.w_u32(g[1]);
	w.w_u32(g[2]);
	w.w_u32(g[3]);
}
