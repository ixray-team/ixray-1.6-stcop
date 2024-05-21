#include "xr_d3d_light.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

void d3d_light::load(xr_reader& r)
{
	type = r.r_u32();
	r.r_fcolor(diffuse);
	r.r_fcolor(specular);
	r.r_fcolor(ambient);
	r.r_fvector3(position);
	r.r_fvector3(direction);
	range = r.r_float();
	falloff = r.r_float();
	attenuation0 = r.r_float();
	attenuation1 = r.r_float();
	attenuation1 = r.r_float();
	theta = r.r_float();
	phi = r.r_float();
}

void d3d_light::save(xr_writer& w) const
{
	w.w_u32(type);
	w.w_fcolor(diffuse);
	w.w_fcolor(specular);
	w.w_fcolor(ambient);
	w.w_float(range);
	w.w_float(falloff);
	w.w_float(attenuation0);
	w.w_float(attenuation1);
	w.w_float(attenuation2);
	w.w_float(theta);
	w.w_float(phi);
}
