#include "xr_geom_buf.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_vbuf::xr_vbuf(): m_signature(0),
	m_points(0), m_normals(0),
	m_texcoords(0), m_lightmaps(0),
	m_influences(0), m_colors(0) {}

xr_vbuf::~xr_vbuf()
{
	clear();
}

xr_vbuf::xr_vbuf(const xr_vbuf& that): xr_flexbuf(that)
{
	set_owner(true);
	set_size(that.size());
	m_points = duplicate(that.m_points);
	m_normals = duplicate(that.m_normals);
	m_texcoords = duplicate(that.m_texcoords);
	m_lightmaps = duplicate(that.m_lightmaps);
	m_colors = duplicate(that.m_colors);
	m_influences = duplicate(that.m_influences);
	make_signature();
}

xr_vbuf::xr_vbuf(size_t n, const fvector3* points, const fvector3* normals, const fvector2* texcoords):
	m_lightmaps(0), m_influences(0), m_colors(0)
{
	set_owner(true);
	set_size(n);
	m_points = duplicate(points);
	m_normals = duplicate(normals);
	m_texcoords = duplicate(texcoords);
	make_signature();
}

xr_vbuf& xr_vbuf::operator=(const xr_vbuf& right)
{
	clear();
	set_size(right.size());
	m_points = duplicate(right.m_points);
	m_normals = duplicate(right.m_normals);
	m_texcoords = duplicate(right.m_texcoords);
	m_lightmaps = duplicate(right.m_lightmaps);
	m_colors = duplicate(right.m_colors);
	m_influences = duplicate(right.m_influences);
	make_signature();
	return *this;
}

void xr_vbuf::proxy(const xr_vbuf& that, size_t base, size_t n)
{
	xr_assert(base + n <= that.size());
	clear();
	set_owner(false);
	set_size(n);
	m_points = that.m_points ? that.m_points + base : 0;
	m_normals = that.m_normals ? that.m_normals + base : 0;
	m_texcoords = that.m_texcoords ? that.m_texcoords + base : 0;
	m_lightmaps = that.m_lightmaps ? that.m_lightmaps + base : 0;
	m_colors = that.m_colors ? that.m_colors + base : 0;
	m_influences = that.m_influences ? that.m_influences + base : 0;
	make_signature();
}

void xr_vbuf::clear()
{
	m_signature = 0;
	if (owner()) {
		delete[] m_points;
		delete[] m_normals;
		delete[] m_texcoords;
		delete[] m_lightmaps;
		delete[] m_influences;
		delete[] m_colors;
	}
	m_points = 0;
	m_normals = 0;
	m_texcoords = 0;
	m_lightmaps = 0;
	m_influences = 0;
	m_colors = 0;
	xr_flexbuf::clear();
}

static inline void r_qnormal(xr_reader& r, fvector3& n)
{
	n.x = r.r_float_q8(-1.f, 1.f);
	n.y = r.r_float_q8(-1.f, 1.f);
	n.z = r.r_float_q8(-1.f, 1.f);
	r.advance(sizeof(uint8_t));
}

void xr_vbuf::load_d3d7(xr_reader& r, size_t n, uint32_t fvf)
{
	clear();

	xr_assert((fvf & D3D_FVF_POSITION_MASK) == D3D_FVF_XYZ);
	if ((fvf & D3D_FVF_POSITION_MASK) == D3D_FVF_XYZ)
		m_points = new fvector3[n];
	if (fvf & (D3D_FVF_DIFFUSE|D3D_FVF_NORMAL))
		m_normals = new fvector3[n];
	unsigned tc = (fvf & D3D_FVF_TEXCOUNT_MASK) >> D3D_FVF_TEXCOUNT_SHIFT;
	xr_assert(tc == 1 || tc == 2);
	if (tc == 1 || tc == 2)
		m_texcoords = new fvector2[n];
	if (tc == 2)
		m_lightmaps = new fvector2[n];
	xr_assert((fvf & ~(D3D_FVF_POSITION_MASK|D3D_FVF_NORMAL|D3D_FVF_DIFFUSE|D3D_FVF_TEX1|D3D_FVF_TEX2)) == 0);
	set_size(n);
	for (size_t i = 0; i != n; ++i) {
		r.r_fvector3(m_points[i]);
		if (fvf & D3D_FVF_NORMAL)
			r.r_fvector3(m_normals[i]);
		if (fvf & D3D_FVF_DIFFUSE)
		{
			fvector3 temp;
			r_qnormal(r, temp);
			m_normals[i] = temp;
		}
		if (fvf & (D3D_FVF_TEX1|D3D_FVF_TEX2))
			r.r_fvector2(m_texcoords[i]);
		if (fvf & D3D_FVF_TEX2)
			r.r_fvector2(m_lightmaps[i]);
	}
	make_signature();
}

static inline void r_qtexcoord(xr_reader& r, fvector2& uv)
{
	uv.x = r.r_s16()*(32.f/32768.f);
	uv.y = r.r_s16()*(32.f/32768.f);
}

static inline void r_qlightmap(xr_reader& r, fvector2& uv)
{
	uv.x = r.r_s16()*(1.f/32768.f);
	uv.y = r.r_s16()*(1.f/32768.f);
}

static inline void r_qcolor(xr_reader& r, fcolor& c)
{
	c.r = r.r_float_q8(-1.f, 1.f);
	c.g = r.r_float_q8(-1.f, 1.f);
	c.b = r.r_float_q8(-1.f, 1.f);
	c.a = r.r_float_q8(-1.f, 1.f);
}

void xr_vbuf::load_d3d9(xr_reader& r, size_t n, const d3d_vertex_element ve[], size_t n_ve)
{
	clear();

	// validate vertex elements and create buffers
	for (size_t i = 0; i != n_ve; ++i) {
		unsigned type = ve[i].type;
		switch (ve[i].usage) {
		case D3D_VE_USAGE_POSITION:
			xr_assert(type == D3D_VE_TYPE_FLOAT3);
			xr_assert(m_points == 0);
			m_points = new fvector3[n];
			break;

		case D3D_VE_USAGE_NORMAL:
			xr_assert(type == D3D_VE_TYPE_D3DCOLOR);
			xr_assert(m_normals == 0);
			m_normals = new fvector3[n];
			break;

		case D3D_VE_USAGE_TEXCOORD:
			xr_assert(type == D3D_VE_TYPE_SHORT2 || type == D3D_VE_TYPE_SHORT4 || type == D3D_VE_TYPE_FLOAT2);
			if (m_texcoords == 0)
				m_texcoords = new fvector2[n];
			else
				m_lightmaps = new fvector2[n];
			break;

		case D3D_VE_USAGE_TANGENT:
		case D3D_VE_USAGE_BINORMAL:
			xr_assert(type == D3D_VE_TYPE_D3DCOLOR);
			break;

		case D3D_VE_USAGE_COLOR:
			xr_assert(type == D3D_VE_TYPE_D3DCOLOR);
			xr_assert(m_colors == 0);
			m_colors = new fcolor[n];
			break;

		default:
			xr_not_expected();
			break;
		}
	}

	fvector2 uv_fix;
	set_size(n);
	for (size_t i = 0; i != n; ++i) {
		unsigned tc = 0;
		uv_fix.set(0, 0);
		for (size_t j = 0; j != n_ve; ++j) {
			switch (ve[j].usage) {
			case D3D_VE_USAGE_POSITION:
				r.r_fvector3(m_points[i]);
				break;
			case D3D_VE_USAGE_NORMAL:
				r_qnormal(r, m_normals[i]);
				break;
			case D3D_VE_USAGE_TEXCOORD:
				switch (ve[j].type) {
				case D3D_VE_TYPE_FLOAT2:
					r.r_fvector2(++tc == 1 ? m_texcoords[i] : m_lightmaps[i]);
					break;
				case D3D_VE_TYPE_SHORT2:
					if (++tc == 1)
						r_qtexcoord(r, m_texcoords[i]);
					else
						r_qlightmap(r, m_lightmaps[i]);
					break;
				default:
				case D3D_VE_TYPE_SHORT4:
					r_qtexcoord(r, m_texcoords[i]);
					r.advance(2*sizeof(int16_t));
					break;
				}
				break;
			case D3D_VE_USAGE_TANGENT:
				r.advance(3*sizeof(uint8_t));
				uv_fix.x = r.r_float_q8()*(32.f/32768.f);
				break;
			case D3D_VE_USAGE_BINORMAL:
				r.advance(3*sizeof(uint8_t));
				uv_fix.y = r.r_float_q8()*(32.f/32768.f);
				break;
			case D3D_VE_USAGE_COLOR:
				r_qcolor(r, m_colors[i]);
				break;
			}
		}
		if (m_texcoords) {
			m_texcoords[i].x += uv_fix.x;
			m_texcoords[i].y += uv_fix.y;
		}
	}
	make_signature();
}

void xr_vbuf::load_dm(xr_reader& r, size_t n)
{
	clear();
	m_points = new fvector3[n];
	m_texcoords = new fvector2[n];
	set_size(n);
	for (size_t i = 0; i != n; ++i) {
		r.r_fvector3(m_points[i]);
		r.r_fvector2(m_texcoords[i]);
	}
	make_signature();
}

void xr_vbuf::save_dm(xr_writer& w) const
{
	for (size_t i = 0, n = size(); i != n; ++i) {
		w.w_fvector3(m_points[i]);
		w.w_fvector2(m_texcoords[i]);
	}
}

void xr_vbuf::load_ogf3(xr_reader& r, size_t n, ogf_vertex_format vf)
{
	if (vf == OGF3_VERTEXFORMAT_FVF_1L) {
		clear();
		m_influences = new finfluence[n];
		m_points = new fvector3[n];
		m_normals = new fvector3[n];
		m_texcoords = new fvector2[n];
		set_size(n);
		for (size_t i = 0; i != n; ++i) {
			r.r_fvector3(m_points[i]);
			r.r_fvector3(m_normals[i]);
			r.r_fvector2(m_texcoords[i]);
			m_influences[i].set(r.r_u32());
		}
		make_signature();
	} else if (vf == OGF3_VERTEXFORMAT_FVF_2L) {
		clear();
		m_influences = new finfluence[n];
		m_points = new fvector3[n];
		m_normals = new fvector3[n];
		m_texcoords = new fvector2[n];
		set_size(n);
		for (size_t i = 0; i != n; ++i) {
			uint16_t bone0 = r.r_u16();
			uint16_t bone1 = r.r_u16();
			r.r_fvector3(m_points[i]);
			r.r_fvector3(m_normals[i]);
			r.advance(2*sizeof(fvector3));	// skip tangent and binormal
			m_influences[i].set_wo_reorder(bone0, bone1, r.r_float());//set_wo_reorder нужно для восстановления модели
			r.r_fvector2(m_texcoords[i]);
		}
		make_signature();
	} else {
		load_d3d7(r, n, vf);
	}
}

void xr_vbuf::load_ogf4(xr_reader& r, size_t n, ogf_vertex_format vf)
{
	clear();
	switch (vf) {
	case OGF4_VERTEXFORMAT_FVF_1L:
	case OGF4_VERTEXFORMAT_FVF_1L_CS:
	case OGF4_VERTEXFORMAT_FVF_2L:
	case OGF4_VERTEXFORMAT_FVF_2L_CS:
	case OGF4_VERTEXFORMAT_FVF_3L_CS:
	case OGF4_VERTEXFORMAT_FVF_4L_CS:
		m_influences = new finfluence[n];
		// fall through
	case OGF_VERTEXFORMAT_FVF:
		m_points = new fvector3[n];
		m_normals = new fvector3[n];
		m_texcoords = new fvector2[n];
		break;
	case OGF4_VERTEXFORMAT_FVF_NL:
	default:
		msg("unknown ogf v4 vertex format %#8.8x", vf);
		xr_not_expected();
		return;
	}
	set_size(n);
	switch (vf) {
	case OGF_VERTEXFORMAT_FVF:
		for (size_t i = 0; i != n; ++i) {
			r.r_fvector3(m_points[i]);
			r.r_fvector3(m_normals[i]);
			r.r_fvector2(m_texcoords[i]);
		}
		break;
	case OGF4_VERTEXFORMAT_FVF_1L:
	case OGF4_VERTEXFORMAT_FVF_1L_CS:
		for (size_t i = 0; i != n; ++i) {
			r.r_fvector3(m_points[i]);
			r.r_fvector3(m_normals[i]);
			r.advance(2*sizeof(fvector3));	// skip tangent and binormal
			r.r_fvector2(m_texcoords[i]);
			m_influences[i].set(r.r_u32());
		}
		break;
	case OGF4_VERTEXFORMAT_FVF_2L:
	case OGF4_VERTEXFORMAT_FVF_2L_CS:
		for (size_t i = 0; i != n; ++i) {
			uint16_t bone0 = r.r_u16();
			uint16_t bone1 = r.r_u16();
			r.r_fvector3(m_points[i]);
			r.r_fvector3(m_normals[i]);
			r.advance(2*sizeof(fvector3));	// skip tangent and binormal
			m_influences[i].set(bone0, bone1, r.r_float());
			r.r_fvector2(m_texcoords[i]);
		}
		break;
	case OGF4_VERTEXFORMAT_FVF_3L_CS:
		for (size_t i = 0; i != n; ++i) {
			const uint16_t* bones = r.skip<uint16_t>(3);
			r.r_fvector3(m_points[i]);
			r.r_fvector3(m_normals[i]);
			r.advance(2*sizeof(fvector3));	// skip tangent and binormal
			m_influences[i].set(3, bones, r.skip<float>(2));
			r.r_fvector2(m_texcoords[i]);
		}
		break;
	case OGF4_VERTEXFORMAT_FVF_4L_CS:
		for (size_t i = 0; i != n; ++i) {
			const uint16_t* bones = r.skip<uint16_t>(4);
			r.r_fvector3(m_points[i]);
			r.r_fvector3(m_normals[i]);
			r.advance(2*sizeof(fvector3));	// skip tangent and binormal
			m_influences[i].set(4, bones, r.skip<float>(3));
			r.r_fvector2(m_texcoords[i]);
		}
		break;
	default:
		xr_not_expected();
		break;
	}
	make_signature();
}

#if 0
bool xr_vbuf::operator==(const xr_vbuf& right) const
{
	// FIXME: only proxy vbufs are supported
	return size() == right.size() && !owner() &&
			owner() == right.owner() &&
			m_points == right.m_points &&
			m_normals == right.m_normals &&
			m_texcoords == right.m_texcoords &&
			m_lightmaps == right.m_lightmaps &&
			m_influences == right.m_influences &&
			m_colors == right.m_colors;
}
#endif

void xr_vbuf::make_signature()
{
	m_signature = 0;
	if (m_points)
		m_signature |= S_POINTS;
	if (m_normals)
		m_signature |= S_NORMALS;
	if (m_texcoords)
		m_signature |= S_TEXCOORDS;
	if (m_lightmaps)
		m_signature |= S_LIGHTMAPS;
	if (m_influences)
		m_signature |= S_INFLUENCES;
	if (m_colors)
		m_signature |= S_COLORS;
}

////////////////////////////////////////////////////////////////////////////////

xr_ibuf::xr_ibuf(): m_indices(0) {}

xr_ibuf::~xr_ibuf()
{
	clear();
}

xr_ibuf::xr_ibuf(const xr_ibuf& that): xr_flexbuf(that)
{
	set_owner(true);
	set_size(that.size());
	m_indices = duplicate(that.m_indices);
}

xr_ibuf& xr_ibuf::operator=(const xr_ibuf& right)
{
	clear();
	set_size(right.size());
	m_indices = duplicate(right.m_indices);
	return *this;
}

void xr_ibuf::proxy(const xr_ibuf& that, size_t base, size_t n)
{
	xr_assert(base + n <= that.size());
	clear();
	set_owner(false);
	set_size(n);
	m_indices = that.m_indices + base;
}

void xr_ibuf::clear()
{
	if (owner())
		delete[] m_indices;
	m_indices = 0;
	xr_flexbuf::clear();
}

void xr_ibuf::load(xr_reader& r, size_t n)
{
	xr_assert(n && (n%3 == 0));
	clear();
	m_indices = new uint16_t[n];
	r.r_cseq(n, m_indices);
	set_size(n);
}

void xr_ibuf::save(xr_writer& w) const
{
	w.w_cseq(size(), m_indices);
}

#if 0
bool xr_ibuf::operator==(const xr_ibuf& right) const
{
	return size() == right.size() && !owner() &&
			owner() == right.owner() &&
			m_indices == right.m_indices;
}
#endif

////////////////////////////////////////////////////////////////////////////////

xr_swibuf::xr_swibuf(): m_slide_windows(0) {}

xr_swibuf::~xr_swibuf()
{
	clear();
}

xr_swibuf::xr_swibuf(const xr_swibuf& that)
{
	set_owner(true);
	set_size(that.size());
	m_slide_windows = duplicate(that.m_slide_windows);
}

xr_swibuf& xr_swibuf::operator=(const xr_swibuf& right)
{
	set_size(right.size());
	m_slide_windows = duplicate(right.m_slide_windows);
	return *this;
}

void xr_swibuf::proxy(const xr_swibuf& that)
{
	clear();
	set_owner(false);
	set_size(that.size());
	m_slide_windows = that.m_slide_windows;
}

void xr_swibuf::clear()
{
	if (owner())
		delete[] m_slide_windows;
	m_slide_windows = 0;
	xr_flexbuf::clear();
}

struct read_sw { void operator()(ogf4_slide_window& sw, xr_reader& r) const {
	sw.offset = r.r_u32();
	sw.num_tris = r.r_u16();
	sw.num_verts = r.r_u16();
}};

void xr_swibuf::load(xr_reader& r)
{
	xr_flexbuf::clear();
	r.r_cseq<uint32_t>(4, m_reserved);
	size_t n = r.r_u32();
	xr_assert(n > 0);
	m_slide_windows = new ogf4_slide_window[n];
	r.r_cseq(n, m_slide_windows, read_sw());
}

struct write_sw { void operator()(const ogf4_slide_window& sw, xr_writer& w) const {
	w.w_u32(sw.offset);
	w.w_u16(sw.num_tris);
	w.w_u16(sw.num_verts);
}};

void xr_swibuf::save(xr_writer& w) const
{
	w.w_cseq<uint32_t>(4, m_reserved);
	w.w_size_u32(size());
	w.w_cseq(size(), m_slide_windows, write_sw());
}
