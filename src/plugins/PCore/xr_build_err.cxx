#include "xr_build_err.h"
#include "xr_file_system.h"

using namespace xray_re;

#if 0
err_invalid	- N троек вершин
err_tjunction	- N вершин
err_multiedge	- N пар вершин

ECF_ZERO_AREA
ECF_ZERO_UV_AREA
ECF_LONG_EDGE
ECF_T_JUNCTION
#endif

enum {
	ERR_CHUNK_TJUNCTION	= 0,
	ERR_CHUNK_MULTIEDGE	= 1,	// also long edges
	ERR_CHUNK_INVALID	= 2,
};

xr_build_err::xr_build_err()
{
	m_invalid = new xr_memory_writer;
}

xr_build_err::~xr_build_err()
{
	delete m_invalid;
}

bool xr_build_err::empty() const
{
	return m_invalid->tell() == 0;
}

void xr_build_err::zero_area_face(const fvector3& p0, const fvector3& p1, const fvector3& p2)
{
	m_invalid->w_fvector3(p0);
	m_invalid->w_fvector3(p1);
	m_invalid->w_fvector3(p2);
}

void xr_build_err::zero_uv_area_face(const fvector3& p0, const fvector3& p1, const fvector3& p2)
{
	m_invalid->w_fvector3(p0);
	m_invalid->w_fvector3(p1);
	m_invalid->w_fvector3(p2);
}

void xr_build_err::save(xr_writer& w) const
{
	w.open_chunk(ERR_CHUNK_TJUNCTION);
	w.w_u32(0);
	w.close_chunk();

	w.open_chunk(ERR_CHUNK_MULTIEDGE);
	w.w_u32(0);
	w.close_chunk();

	size_t size = m_invalid->tell();
	w.open_chunk(ERR_CHUNK_INVALID);
	w.w_size_u32(size/(3*sizeof(fvector3)));
	if (size)
		w.w_raw(m_invalid->data(), size);
	w.close_chunk();
}

bool xr_build_err::save(const char* path, const char* name) const
{
	xr_file_system& fs = xr_file_system::instance();
	xr_writer* w = fs.w_open(path, name);
	if (w == 0)
		return false;
	save(*w);
	fs.w_close(w);
	return true;
}
