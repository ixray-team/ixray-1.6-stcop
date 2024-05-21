#include "xr_level_geom.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_level_version.h"

using namespace xray_re;

xr_level_geom::~xr_level_geom() {}

void xr_level_geom::load_d3d7(xr_reader& r)
{
	m_vbufs.resize(r.r_u32());
	for (xr_vbuf_vec_it it = m_vbufs.begin(), end = m_vbufs.end(); it != end; ++it) {
		uint32_t fvf = r.r_u32();
		size_t n = r.r_u32();
		it->load_d3d7(r, n, fvf);
	}
}

void xr_level_geom::load_v9(xr_reader& r)
{
	xr_reader* s = r.open_chunk(FSL9_VB);
	if (s) {
		m_vbufs.resize(s->r_u32());
		for (xr_vbuf_vec_it it = m_vbufs.begin(), end = m_vbufs.end(); it != end; ++it) {
			const d3d_vertex_element *ve = s->skip<d3d_vertex_element>();
			size_t n_ve = 1;
			for (; s->skip<d3d_vertex_element>()->type != D3D_VE_TYPE_UNUSED; ++n_ve) {}
			it->load_d3d9(*s, s->r_u32(), ve, n_ve);
		}
		r.close_chunk(s);
	} else {
		s = r.open_chunk(FSL9_VB_OLD);
		xr_assert(s);
		m_vbufs.resize(s->r_u32());
		for (xr_vbuf_vec_it it = m_vbufs.begin(), end = m_vbufs.end(); it != end; ++it) {
			uint32_t fvf = s->r_u32();
			size_t n = s->r_u32();
			it->load_d3d7(*s, n, fvf);
		}
		r.close_chunk(s);
	}

	s = r.open_chunk(FSL9_IB);
	xr_assert(s);
	m_ibufs.resize(s->r_u32());
	for (xr_ibuf_vec_it it = m_ibufs.begin(), end = m_ibufs.end(); it != end; ++it)
		it->load(*s, s->r_u32());
	r.close_chunk(s);
}

void xr_level_geom::load_1865(xr_reader& r)
{
	xr_reader* s = r.open_chunk(FSL12_VB);
	xr_assert(s);
	m_vbufs.resize(s->r_u32());
	for (xr_vbuf_vec_it it = m_vbufs.begin(), end = m_vbufs.end(); it != end; ++it) {
		const d3d_vertex_element *ve = s->skip<d3d_vertex_element>();
		size_t n_ve = 1;
		for (; s->skip<d3d_vertex_element>()->type != D3D_VE_TYPE_UNUSED; ++n_ve) {}
		it->load_d3d9(*s, s->r_u32(), ve, n_ve);
	}
	r.close_chunk(s);

	s = r.open_chunk(FSL12_IB);
	xr_assert(s);
	m_ibufs.resize(s->r_u32());
	for (xr_ibuf_vec_it it = m_ibufs.begin(), end = m_ibufs.end(); it != end; ++it)
		it->load(*s, s->r_u32());
	r.close_chunk(s);

	s = r.open_chunk(FSL12_SWIS);
	if (s != 0)
	{
		m_swibufs.resize(s->r_u32());
		for (xr_swibuf_vec_it it = m_swibufs.begin(), end = m_swibufs.end(); it != end; ++it)
			it->load(*s);
		r.close_chunk(s);
	}
}

void xr_level_geom::load_d3d9(xr_reader& r)
{
	xr_reader* s = r.open_chunk(FSL13_VB);
	xr_assert(s);
	m_vbufs.resize(s->r_u32());
	for (xr_vbuf_vec_it it = m_vbufs.begin(), end = m_vbufs.end(); it != end; ++it) {
		const d3d_vertex_element *ve = s->skip<d3d_vertex_element>();
		size_t n_ve = 1;
		for (; s->skip<d3d_vertex_element>()->type != D3D_VE_TYPE_UNUSED; ++n_ve) {}
		it->load_d3d9(*s, s->r_u32(), ve, n_ve);
	}
	r.close_chunk(s);

	s = r.open_chunk(FSL13_IB);
	xr_assert(s);
	m_ibufs.resize(s->r_u32());
	for (xr_ibuf_vec_it it = m_ibufs.begin(), end = m_ibufs.end(); it != end; ++it)
		it->load(*s, s->r_u32());
	r.close_chunk(s);

	s = r.open_chunk(FSL13_SWIS);
	xr_assert(s);
	m_swibufs.resize(s->r_u32());
	for (xr_swibuf_vec_it it = m_swibufs.begin(), end = m_swibufs.end(); it != end; ++it)
		it->load(*s);
	r.close_chunk(s);
}

void xr_level_geom::load(uint32_t xrlc_version, xr_reader& r)
{
	if (xrlc_version == UINT32_MAX) {
		if (!r.find_chunk(FSL_HEADER))
			xr_not_expected();
		xrlc_version = r.r_u16();
		r.debug_find_chunk();
	}
	
	if (xrlc_version >= XRLC_VERSION_5 && xrlc_version <= XRLC_VERSION_8) {
		xr_reader* s = 0;
		if (xrlc_version == XRLC_VERSION_5)
			s = r.open_chunk(FSL5_VB);
		else if (xrlc_version == XRLC_VERSION_8)
			s = r.open_chunk(FSL8_VB);
		xr_assert(s);
		load_d3d7(*s);
		r.close_chunk(s);
	} else if (xrlc_version == XRLC_VERSION_9) {
		load_v9(r);
	} else if (xrlc_version >= XRLC_VERSION_10 && xrlc_version <= XRLC_VERSION_12) {
		load_1865(r);
	} else if (xrlc_version >= XRLC_VERSION_13) {
		load_d3d9(r);
	}
}
