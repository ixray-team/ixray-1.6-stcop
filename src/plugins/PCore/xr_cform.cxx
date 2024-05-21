#include <algorithm>
#include "xr_cform.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_cform::xr_cform() { m_bbox.null(); }

xr_cform::~xr_cform() {}

struct read_vertex { void operator()(cf_vertex& vertex, xr_reader& r) const {
	r.r_fvector3(vertex.p);
	vertex.face0 = BAD_IDX;
}};

struct read_face_v2 { void operator()(cf_face& face, xr_reader& r) const {
	face.v0 = r.r_u32();
	face.v1 = r.r_u32();
	face.v2 = r.r_u32();
	r.advance(12+2);
	face.sector = r.r_u16();
	r.advance(4);
	face.link0 = BAD_IDX;
	face.link1 = BAD_IDX;
	face.link2 = BAD_IDX;
}};

struct read_face_v4 { void operator()(cf_face& face, xr_reader& r) const {
	face.v0 = r.r_u32();
	face.v1 = r.r_u32();
	face.v2 = r.r_u32();
	face.link0 = BAD_IDX;
	face.link1 = BAD_IDX;
	face.link2 = BAD_IDX;
	face.dummy = r.r_u32();
}};

void xr_cform::load(xr_reader& r)
{
	uint32_t version = r.r_u32();
	xr_assert(version >= CFORM_VERSION_2 && version <= CFORM_VERSION_4);
	size_t vertex_count = r.r_u32();
	size_t face_count = r.r_u32();
	r.r_fvector3(m_bbox.min);
	r.r_fvector3(m_bbox.max);
	r.r_seq(vertex_count, m_vertices, read_vertex());
	if (version == CFORM_VERSION_2 || version == CFORM_VERSION_3)
		r.r_seq(face_count, m_faces, read_face_v2());
	else if (version == CFORM_VERSION_4)
		r.r_seq(face_count, m_faces, read_face_v4());
}

struct write_vertex { void operator()(const cf_vertex& vertex, xr_writer& w) const {
	w.w_fvector3(vertex.p);
}};

struct write_face_v4 { void operator()(const cf_face& face, xr_writer& w) const {
	w.w_u32(face.v0);
	w.w_u32(face.v1);
	w.w_u32(face.v2);
	w.w_u32(face.dummy);
}};

void xr_cform::save(xr_writer& w) const
{
	w.w_u32(CFORM_VERSION_4);
	w.w_size_u32(m_vertices.size());
	w.w_size_u32(m_faces.size());
	w.w_fvector3(m_bbox.min);
	w.w_fvector3(m_bbox.max);
	w.w_seq(m_vertices, write_vertex());
	w.w_seq(m_faces, write_face_v4());
}

void xr_cform::generate_vertex_faces()
{
	for (cf_vertex_vec_it it = m_vertices.begin(), end = m_vertices.end();
			it != end; ++it) {
		it->face0 = BAD_IDX;
	}
	uint32_t face_idx = 0;
	for (cf_face_vec_it it = m_faces.begin(), end = m_faces.end();
			it != end; ++it, ++face_idx) {
		for (uint_fast32_t i = 3; i != 0;) {
			uint32_t& face0 = m_vertices[it->v[--i]].face0;
			it->link[i] = face0;
			face0 = face_idx;
		}
	}
}

bool cf_face::is_duplicate(const cf_face& face) const
{
	for (uint_fast32_t i = 3; i != 0;) {
		if (v[--i] != face.v0)
			continue;
		return v[(i+1)%3] == face.v1 && v[(i+2)%3] == face.v2 &&
				dummy == face.dummy;
	}
	return false;
}

struct face_degenerate_pred { bool operator()(const cf_face& face) const {
	return face.v0 == face.v1 || face.v1 == face.v2 || face.v2 == face.v0;
}};

struct face_duplicate_pred { bool operator()(const cf_face& face) const {
	return face.dummy == BAD_IDX;
}};

void xr_cform::optimize()
{
	m_faces.erase(std::remove_if(m_faces.begin(), m_faces.end(), face_degenerate_pred()), m_faces.end());

	generate_vertex_faces();
	for (cf_face_vec_it it = m_faces.begin(), end = m_faces.end(); it != end; ++it) {
		if (it->dummy == BAD_IDX)
			continue;
		for (uint_fast32_t next = m_vertices[it->v0].face0; next != BAD_IDX;) {
			cf_face& face = m_faces[next];
			if (&face > &*it && it->is_duplicate(face))
				face.dummy = BAD_IDX;
			next = face.next_face_idx(it->v0);
		}
	}
//	size_t n = count_if(m_faces.begin(), m_faces.end(), is_duplicate());
	m_faces.erase(std::remove_if(m_faces.begin(), m_faces.end(), face_duplicate_pred()), m_faces.end());
//	msg("CFORM optimize: was %"PRIuSIZE", left: %"PRIuSIZE"\n", n, m_faces.size());

	generate_vertex_faces();
}
