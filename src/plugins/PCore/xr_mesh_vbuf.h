#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_MESH_VBUF_H__
#define __XR_MESH_VBUF_H__

#include "xr_geom_buf.h"

namespace xray_re {

class xr_mesh_vbuf: public xr_vbuf {
public:
			xr_mesh_vbuf();
	void		clear();
	void		set_signature(uint32_t _signature);
	void		reserve(size_t size);
	void		push(const xr_vbuf& vb, const xr_ibuf* ib = 0, const fmatrix* xform = 0);
	void		set_tc_fix(bool tc_fix);

protected:
	void		transform_points(const fvector3* source, fvector3* target, size_t size,
					const fmatrix& xform) const;
	void		transform_normals(const fvector3* source, fvector3* target, size_t size,
					const fmatrix& xform) const;
	template<typename T> void
			extend(T*& source, size_t new_size);

private:
	size_t		m_reserved;
	bool		m_tc_fix;	// FIXME: hack
};

inline xr_mesh_vbuf::xr_mesh_vbuf(): m_reserved(0), m_tc_fix(false) {}

inline void xr_mesh_vbuf::set_tc_fix(bool tc_fix) { m_tc_fix = tc_fix; }

} // end of namespace xray_re

#endif
