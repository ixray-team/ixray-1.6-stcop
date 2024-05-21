#include "xr_dm.h"
#include "xr_reader.h"
#include "xr_file_system.h"
#include "xr_mesh_builder.h"
#include "xr_surface.h"

using namespace xray_re;

xr_dm::xr_dm(): m_flags(0), m_min_scale(0.5f), m_max_scale(2.f) {}

xr_dm::~xr_dm() {}

bool xr_dm::load_dm(const char* path)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path);
	if (r == 0)
		return false;
	load_dm(*r);
	fs.r_close(r);
	return true;
}

void xr_dm::load_dm(xr_reader& r)
{
	r.r_sz(m_shader);
	r.r_sz(m_texture);
	m_flags = r.r_u32();
	m_min_scale = r.r_float();
	m_max_scale = r.r_float();
	size_t num_vertices = r.r_u32();
	size_t num_indices = r.r_u32();
	m_vb.load_dm(r, num_vertices);
	m_ib.load(r, num_indices);
}

void xr_dm::save_dm(xr_writer& w) const
{
	w.w_sz(m_shader);
	w.w_sz(m_texture);
	w.w_u32(m_flags);
	w.w_float(m_min_scale);
	w.w_float(m_max_scale);
	w.w_size_u32(m_vb.size());
	w.w_size_u32(m_ib.size());
	m_vb.save_dm(w);
	m_ib.save(w);
}

xr_surface* xr_dm::create_surface(const xr_raw_surface& raw_surface) const
{
	xr_surface* surface = new xr_surface;
	surface->texture() = m_texture;
	surface->eshader() = m_shader;
	if (raw_surface.two_sided())
		surface->set_two_sided();
	return surface;
}

void xr_dm::to_object()
{
	xr_mesh_builder* mesh = new xr_mesh_builder;
	mesh->prepare(xr_vbuf::S_POINTS|xr_vbuf::S_TEXCOORDS, m_vb.size(), m_ib.size());
	mesh->push(m_vb, m_ib, 0, 0);
	mesh->compact_geometry();
	mesh->remove_duplicate_faces();
	mesh->remove_back_faces();
	mesh->commit(*this);
	mesh->name() = "detailShape";
	denominate_surfaces();
}

void xr_dm::calc_uv_bounds(frect& bounds) const
{
	bounds.invalidate();
	for (const fvector2 *it = m_vb.tc(), *end = it + m_vb.size(); it != end; ++it)
		bounds.extend(*it);
}
