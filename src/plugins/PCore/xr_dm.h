#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_DM_H__
#define __XR_DM_H__

#include <vector>
#include <string>
#include "xr_geom_buf.h"
#include "xr_object.h"
#include "xr_rect.h"

namespace xray_re {

enum {
	DOF_NO_WAVING	= 0x1,
};

// this is not used. just for the record.
struct dm_vertex {
	fvector3	point;
	float		u, v;
};

class xr_reader;
class xr_writer;

class xr_dm: public xr_object {
public:
				xr_dm();
	virtual			~xr_dm();
	virtual void		to_object();

	bool			load_dm(const char* path);
	void			load_dm(xr_reader& r);
	void			save_dm(xr_writer& w) const;

	void			calc_uv_bounds(frect& bounds) const;

	std::string&		shader();
	const std::string&	shader() const;
	std::string&		texture();
	const std::string&	texture() const;
	uint32_t&		flags();
	uint32_t		flags() const;
	float&			min_scale();
	float			min_scale() const;
	float&			max_scale();
	float			max_scale() const;
	const xr_vbuf&		vb() const;
	const xr_ibuf&		ib() const;

protected:
	virtual xr_surface*	create_surface(const xr_raw_surface& raw_surface) const;

private:
	std::string	m_shader;
	std::string	m_texture;
	uint32_t	m_flags;
	float		m_min_scale;
	float		m_max_scale;
	xr_vbuf		m_vb;
	xr_ibuf		m_ib;
};

TYPEDEF_STD_VECTOR_PTR(xr_dm)

inline std::string& xr_dm::shader() { return m_shader; }
inline const std::string& xr_dm::shader() const { return m_shader; }
inline std::string& xr_dm::texture() { return m_texture; }
inline const std::string& xr_dm::texture() const { return m_texture; }
inline uint32_t& xr_dm::flags() { return m_flags; }
inline uint32_t xr_dm::flags() const { return m_flags; }
inline float& xr_dm::min_scale() { return m_min_scale; }
inline float xr_dm::min_scale() const { return m_min_scale; }
inline float& xr_dm::max_scale() { return m_max_scale; }
inline float xr_dm::max_scale() const { return m_max_scale; }
inline const xr_vbuf& xr_dm::vb() const { return m_vb; }
inline const xr_ibuf& xr_dm::ib() const { return m_ib; }

} // end of namespace xray_re

#endif
