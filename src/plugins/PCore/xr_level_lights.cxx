#include "xr_level_version.h"
#include "xr_level_lights.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_utils.h"

using namespace xray_re;

xr_level_lights::~xr_level_lights()
{
	delete_elements(m_lights);
}

struct read_light_v5 { void operator()(light_data*& _light, xr_reader& r) const {
	light_data* light = new light_data;
	_light = light;
	light->d3d_params.load(r);
	r.advance(sizeof(fsl_light_v5) - sizeof(d3d_light));
	if (light->d3d_params.type == D3D_LIGHT_POINT)
		light->controller_id = 2;
	else if (light->d3d_params.type == D3D_LIGHT_DIRECTIONAL)
		light->controller_id = 1;
	else
		xr_not_expected();
}};

void xr_level_lights::load_v5(xr_reader& r)
{
	xr_assert(r.size() % sizeof(fsl_light_v5) == 0);
	r.r_seq(r.size()/sizeof(fsl_light_v5), m_lights, read_light_v5());
}

struct read_light_v8 { void operator()(light_data*& _light, xr_reader& r) const {
	light_data* light = new light_data;
	_light = light;
	light->d3d_params.load(r);
	r.advance(sizeof(fsl_light_v8) - sizeof(d3d_light));
	if (light->d3d_params.type == D3D_LIGHT_POINT)
		light->controller_id = 2;
	else if (light->d3d_params.type == D3D_LIGHT_DIRECTIONAL)
		light->controller_id = 1;
	else
		xr_not_expected();
}};

void xr_level_lights::load_v8(xr_reader& r)
{
	xr_assert(r.size() % sizeof(fsl_light_v8) == 0);
	r.r_seq(r.size()/sizeof(fsl_light_v8), m_lights, read_light_v8());
}

struct read_light_v13 { void operator()(light_data*& _light, xr_reader& r) const {
	light_data* light = new light_data;
	_light = light;
	light->controller_id = r.r_u32();
	light->d3d_params.load(r);
}};

void xr_level_lights::load_v13(xr_reader& r)
{
	xr_assert(r.size() % sizeof(fsl_light_v13) == 0);
	r.r_seq(r.size()/sizeof(fsl_light_v13), m_lights, read_light_v13());
}

void xr_level_lights::load(uint32_t xrlc_version, xr_reader& r)
{
	xr_reader* s = 0;
	switch (xrlc_version) {
	case XRLC_VERSION_5:
		s = r.open_chunk(FSL5_LIGHT_DYNAMIC);
		xr_assert(s);
		load_v5(*s);
		break;
	case XRLC_VERSION_8:
		s = r.open_chunk(FSL8_LIGHT_DYNAMIC);
		xr_assert(s);
		load_v8(*s);
		break;
	case XRLC_VERSION_9:
		s = r.open_chunk(FSL9_LIGHT_DYNAMIC);
		xr_assert(s);
		load_v13(*s);
		break;
	default: //10+
		s = r.open_chunk(FSL13_LIGHT_DYNAMIC);
		xr_assert(s);
		load_v13(*s);
		break;
	}
	r.close_chunk(s);
}

void xr_level_lights::save(xr_writer& w) const
{
}
