#include "xr_details.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

void details_header::load(xr_reader& r)
{
	version = r.r_u32();
	object_count = r.r_u32();
	offs_x = r.r_s32();
	offs_z = r.r_s32();
	size_x = r.r_u32();
	size_z = r.r_u32();
}

void details_header::save(xr_writer& w) const
{
	w.w_u32(version);
	w.w_u32(object_count);
	w.w_s32(offs_x);
	w.w_s32(offs_z);
	w.w_u32(size_x);
	w.w_u32(size_z);
}
