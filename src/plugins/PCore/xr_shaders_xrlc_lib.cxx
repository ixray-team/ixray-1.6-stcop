#include "xr_shaders_xrlc_lib.h"
#include "xr_file_system.h"
#include "xr_utils.h"

using namespace xray_re;

xr_shaders_xrlc_lib::~xr_shaders_xrlc_lib()
{
	delete_elements(m_shaders);
}

struct read_shader_xrlc { void operator()(xr_shader_xrlc*& _s, xr_reader& r) {
	xr_shader_xrlc* s = new xr_shader_xrlc;
	_s = s;
	size_t pos = r.tell();
	r.r_sz(s->name);
	r.seek(pos + 128);
	s->dummy = r.r_u32();
	s->vert_translucency = r.r_float();
	s->vert_ambient = r.r_float();
	s->lm_density = r.r_float();
}};

void xr_shaders_xrlc_lib::load(xr_reader& r)
{
	xr_assert(r.size()%SHADER_XRLC_SIZE == 0);
	r.r_seq(r.size()/SHADER_XRLC_SIZE, m_shaders, read_shader_xrlc());
}

bool xr_shaders_xrlc_lib::load(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	load(*r);
	fs.r_close(r);
	return true;
}
