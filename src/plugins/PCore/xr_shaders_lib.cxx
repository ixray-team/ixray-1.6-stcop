#include "xr_shaders_lib.h"
#include "xr_file_system.h"
#include "xr_utils.h"

using namespace xray_re;

// guessed names
enum {
	SHADERS_CHUNK_CONSTANTS		= 0,
	SHADERS_CHUNK_MATRICES		= 1,
	SHADERS_CHUNK_BLENDERS		= 2,
	SHADERS_CHUNK_NAMES		= 3,
};

xr_shaders_lib::~xr_shaders_lib()
{
//	delete_elements(m_shaders);
}

// FIXME: implement method
void xr_shaders_lib::load(xr_reader& r)
{
	xr_reader* s = r.open_chunk(SHADERS_CHUNK_CONSTANTS);
	if (s) {
		r.close_chunk(s);
	}

	s = r.open_chunk(SHADERS_CHUNK_MATRICES);
	if (s) {
		r.close_chunk(s);
	}

	s = r.open_chunk(SHADERS_CHUNK_BLENDERS);
	if (s) {
		r.close_chunk(s);
	}

	s = r.open_chunk(SHADERS_CHUNK_NAMES);
	if (s) {
		s->r_seq(s->r_u32(), m_names, xr_reader::f_r_sz());
		r.close_chunk(s);
	}
}

bool xr_shaders_lib::load(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	load(*r);
	fs.r_close(r);
	return true;
}
