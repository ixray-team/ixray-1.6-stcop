#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_CFORM_H__
#define __XR_CFORM_H__

#include <vector>
#include "xr_shape.h"
#include "xr_aabb.h"

namespace xray_re {

const uint32_t CFORM_VERSION_2 = 2;
const uint32_t CFORM_VERSION_3 = 3;
const uint32_t CFORM_VERSION_4 = 4;
const uint32_t CFORM_VERSION = CFORM_VERSION_4;

struct cform_header {
	uint32_t	version;
	uint32_t	vertex_count;
	uint32_t	face_count;
	fbox		aabb;
};

// v2
struct cform_face_v2 {
	uint32_t	verts[6];
	uint16_t	unk;
	uint16_t	sector;
	uint32_t	zero;		// really zero?
};

// v4
struct cform_face_v4 {
	uint32_t	verts[3];
	union {
		uint32_t	dummy;
		struct {
			uint32_t	material:14;
			uint32_t	suppress_shadows:1;
			uint32_t	suppress_wms:1;
			uint32_t	sector:16;
		};
	};
};

struct cf_vertex {
	bool		operator<(const cf_vertex& right) const;

	uint32_t	face0;
	fvector3	p;
};
TYPEDEF_STD_VECTOR(cf_vertex)

inline bool cf_vertex::operator<(const cf_vertex& right) const { return p < right.p; }


struct cf_face {
	uint_fast32_t	next_face_idx(uint_fast32_t vert_idx) const;
	uint_fast32_t	local_vert_idx(uint_fast32_t vert_idx) const;
	bool		is_duplicate(const cf_face& face) const;

	union {
		struct {
			uint32_t	v[3];
			uint32_t	link[3];
		};
		struct {
			uint32_t	v0, v1, v2;
			uint32_t	link0, link1, link2;
		};
	};
	union {
		uint32_t		dummy;
		struct {
			uint32_t	material:14;
			uint32_t	suppress_shadows:1;
			uint32_t	suppress_wms:1;
			uint32_t	sector:16;
		};
	};
};
TYPEDEF_STD_VECTOR(cf_face)

inline uint_fast32_t cf_face::local_vert_idx(uint_fast32_t vert_idx) const
{
	for (uint_fast32_t i = 3; i != 0;) {
		if (v[--i] == vert_idx)
			return i;
	}
	xr_not_expected();
	return BAD_IDX;
}

inline uint_fast32_t cf_face::next_face_idx(uint_fast32_t vert_idx) const
{
	for (uint_fast32_t i = 3; i != 0;) {
		if (v[--i] == vert_idx)
			return link[i];
	}
	xr_not_expected();
	return BAD_IDX;
}


class xr_reader;
class xr_writer;

class xr_cform {
public:
				xr_cform();
	virtual			~xr_cform();

	void			load(xr_reader& r);
	void			save(xr_writer& w) const;

	void			optimize();
	void			generate_vertex_faces();

	const fbox&		bbox() const;
	const cf_vertex_vec&	vertices() const;
	const cf_face_vec&	faces() const;

private:
	fbox			m_bbox;
	cf_vertex_vec		m_vertices;
	cf_face_vec		m_faces;
};

inline const fbox& xr_cform::bbox() const { return m_bbox; }
inline const cf_vertex_vec& xr_cform::vertices() const { return m_vertices; }
inline const cf_face_vec& xr_cform::faces() const { return m_faces; }

} // end of namespace xray_re

#endif
