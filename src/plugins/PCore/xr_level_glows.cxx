#include "xr_level_version.h"
#include "xr_level_glows.h"
#include "xr_reader.h"
#include "xr_utils.h"

using namespace xray_re;

xr_level_glows::~xr_level_glows()
{
	delete_elements(m_glows);
}

struct read_glow_v5 { void operator()(glow_data*& _glow, xr_reader& r) const {
	glow_data* glow = new glow_data;
	_glow = glow;
	r.r_fvector3(glow->position);
	glow->radius = r.r_float();
	glow->texture_id = uint16_t(r.r_u32() & UINT16_MAX);
	glow->shader_id = uint16_t(r.r_u32() & UINT16_MAX);
}};

void xr_level_glows::load_v5(xr_reader& r)
{
	xr_assert(r.size() % sizeof(fsl_glow_v5) == 0);
	r.r_seq(r.size()/sizeof(fsl_glow_v5), m_glows, read_glow_v5());
}

struct read_glow_v13 { void operator()(glow_data*& _glow, xr_reader& r) const {
	glow_data* glow = new glow_data;
	_glow = glow;
	r.r_fvector3(glow->position);
	glow->radius = r.r_float();
	glow->texture_id = glow->shader_id = r.r_u16();
}};

void xr_level_glows::load_v13(xr_reader& r)
{
	xr_assert(r.size() % sizeof(fsl_glow_v13) == 0);
	r.r_seq(r.size()/sizeof(fsl_glow_v13), m_glows, read_glow_v13());
}

void xr_level_glows::load(uint32_t xrlc_version, xr_reader& r)
{
	xr_reader* s = 0;
	if (xrlc_version <= XRLC_VERSION_9) {
		if (xrlc_version == XRLC_VERSION_5)
			s = r.open_chunk(FSL5_GLOWS);
		else
			s = r.open_chunk(FSL8_GLOWS);
		xr_assert(s);
		load_v5(*s);
	} else if (xrlc_version == XRLC_VERSION_10 || xrlc_version == XRLC_VERSION_11) {
		s = r.open_chunk(FSL10_GLOWS);
		xr_assert(s);
		load_v5(*s);
	} else {
		s = r.open_chunk(FSL13_GLOWS);
		xr_assert(s);
		load_v13(*s);
	}
	r.close_chunk(s);
}
