#include "xr_surface.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_utils.h"

using namespace xray_re;

xr_surface::xr_surface(bool skeletal):
	m_eshader(skeletal ? "models\\model" : "default"),
	m_cshader("default"),
	m_gamemtl(skeletal ? "default_object" : "default"),
	m_vmap("Texture"),
	m_flags(0),
	m_fvf(ESURFACE_DEFAULT_FVF) {}

xr_surface::~xr_surface() {}

void xr_surface::load_2(xr_reader& r)
{
	r.r_sz(m_name);
	r.r_sz(m_eshader);
	r.r_sz(m_cshader);
	r.r_sz(m_gamemtl);
	r.r_sz(m_texture);
	r.r_sz(m_vmap);
	m_flags = r.r_u32();
	m_fvf = r.r_u32();
	unsigned tc = r.r_u32();
	xr_assert(tc <= 1);
}

void xr_surface::load_1(xr_reader& r)
{
	r.r_sz(m_name);
	r.r_sz(m_eshader);
	r.r_sz(m_cshader);
	m_gamemtl = "default";
	r.r_sz(m_texture);
	r.r_sz(m_vmap);
	m_flags = r.r_u32();
	m_fvf = r.r_u32();
	unsigned tc = r.r_u32();
	xr_assert(tc <= 1);
}

void xr_surface::load_0(xr_reader& r)
{
	r.r_sz(m_name);
	r.r_sz(m_eshader);
	m_cshader = "default";
	m_gamemtl = "default";
	if (r.r_bool())
		m_flags = ESF_TWO_SIDED;
	m_fvf = r.r_u32();
	unsigned tc = r.r_u32();
	xr_assert(tc <= 1);
	r.r_sz(m_texture);
	r.r_sz(m_vmap);
}

void xr_surface::save(xr_writer& w) const
{
	w.w_sz(m_name);
	w.w_sz(m_eshader);
	w.w_sz(m_cshader);
	w.w_sz(m_gamemtl);
	w.w_sz(m_texture);
	w.w_sz(m_vmap);
	w.w_u32(m_flags);
	w.w_u32(m_fvf);
	w.w_u32(1);
}

int xr_surface::compare(const xr_surface& right) const
{
	int result;
	if ((result = m_name.compare(right.m_name)))
		return result;
	if ((result = m_eshader.compare(right.m_eshader)))
		return result;
	if ((result = m_cshader.compare(right.m_cshader)))
		return result;
	if ((result = m_gamemtl.compare(right.m_gamemtl)))
		return result;
	if ((result = m_texture.compare(right.m_texture)))
		return result;
	if ((result = m_vmap.compare(right.m_vmap)))
		return result;
	if ((result = __compare(m_flags, right.m_flags)))
		return result;
	return __compare(m_flags, right.m_flags);
}
