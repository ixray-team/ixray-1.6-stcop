#include "xr_mesh_vbuf.h"

using namespace xray_re;

template<typename T> void xr_mesh_vbuf::extend(T*& source, size_t new_size)
{
	T* target = new T[new_size];
	if (source)
		copy(source, target, size());
	delete source;
	source = target;
}

void xr_mesh_vbuf::transform_normals(const fvector3* source, fvector3* target, size_t size,
		const fmatrix& xform) const
{
	dmatrix m;
	m.set(xform);
	m.i.normalize();
	m.j.normalize();
	m.k.normalize();
	for (; size != 0; --size, ++target)
		target->set(dvector3().set(*source++).rotate(m));
}

void xr_mesh_vbuf::transform_points(const fvector3* source, fvector3* target, size_t size,
		const fmatrix& xform) const
{
	dmatrix m;
	m.set(xform);
	for (; size != 0; --size, ++target)
		target->set(dvector3().set(*source++).transform(m));
}

void xr_mesh_vbuf::clear()
{
	xr_vbuf::clear();
	m_reserved = 0;
}

void xr_mesh_vbuf::set_signature(uint32_t _signature)
{
	xr_assert(size() == 0);
	m_signature = _signature;
}

void xr_mesh_vbuf::reserve(size_t reserved)
{
	if (reserved <= size())
		return;
	if (has_points())
		extend(m_points, reserved);
	if (has_normals())
		extend(m_normals, reserved);
	if (has_texcoords())
		extend(m_texcoords, reserved);
	if (has_influences())
		extend(m_influences, reserved);
	if (has_colors())
		extend(m_colors, reserved);
	m_reserved = reserved;
}

void xr_mesh_vbuf::push(const xr_vbuf& vb, const xr_ibuf* ib, const fmatrix* xform)
{
	if (signature() == 0) {
		xr_assert(size() == 0);
		set_signature(vb.signature());
	}
	size_t new_size = size() + vb.size();
	if (m_reserved < new_size)
		reserve(new_size);
	if (has_points()) {
		xr_assert(vb.has_points());
		if (xform)
			transform_points(vb.p(), m_points + size(), vb.size(), *xform);
		else
			copy(vb.p(), m_points + size(), vb.size());
	}
	if (has_normals()) {
		if (vb.has_normals()) {
			if (false && xform)
				transform_normals(vb.n(), m_normals + size(), vb.size(), *xform);
			else
				copy(vb.n(), m_normals + size(), vb.size());
		} else {
			// generate fake normals for 1xxx builds.
			// FIXME (maybe): this will not work correctly for the duplicate faces.
			xr_assert(ib);
			fvector3* normals = m_normals + size();
			for (size_t i = 0, num_indices = ib->size(); i < num_indices; i += 3) {
				uint32_t v0 = (*ib)[i + 0];
				uint32_t v1 = (*ib)[i + 1];
				uint32_t v2 = (*ib)[i + 2];
				fvector3 normal;
				normal.calc_normal(vb.p(v0), vb.p(v1), vb.p(v2));
				normals[v0].set(normal);
				normals[v1].set(normal);
				normals[v2].set(normal);
			}
		}
	}
	if (has_texcoords()) {
		xr_assert(vb.has_texcoords());
		copy(vb.tc(), m_texcoords + size(), vb.size());
		if (m_tc_fix) {
			fvector2* uv = m_texcoords + size();
			for (size_t n = vb.size(); n != 0; --n, ++uv)
				uv->mul(0.5f);
		}
	}
	// intentionally ignoring lightmaps here
	if (has_influences()) {
		xr_assert(vb.has_influences());
		copy(vb.w(), m_influences + size(), vb.size());
	}
	if (has_colors()) {
		xr_assert(vb.has_colors());
		copy(vb.c(), m_colors + size(), vb.size());
	}
	set_size(new_size);
}
