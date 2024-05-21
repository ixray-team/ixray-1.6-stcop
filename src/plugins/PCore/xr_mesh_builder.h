#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_MESH_BUILDER_H__
#define __XR_MESH_BUILDER_H__

#include <vector>
#include <string>
#include "xr_mesh.h"
#include "xr_mesh_vbuf.h"
#include "xr_surface.h"
#include "xr_build_err.h"

namespace xray_re {

enum {
	PF_DEFAULT		= 0,
	PF_REMOVE_ZERO_FACE	= 0x01,
	PF_REMOVE_ZERO_UV_FACE	= 0x02,
};

class xr_bone;

class xr_mesh_builder: public xr_mesh {
public:
			xr_mesh_builder();
	virtual		~xr_mesh_builder();

	void		set_tc_fix(bool tc_fix);
	void		prepare(uint32_t signature = 0, size_t num_vertices = 0, size_t num_indices = 0);
	void		push(const xr_vbuf& vb, const xr_ibuf& ib, uint16_t texture, uint16_t eshader);
	void		compact_geometry();
	void		remove_back_faces(float normal_tolerance = 1e-6f);
	void		remove_duplicate_faces();
	void		commit(xr_object& object);

	xr_build_err&	build_err();

	struct b_face {
				b_face();
				b_face(uint16_t _sector);
				b_face(uint16_t _sector, uint32_t _tag);
		void		clear();
		b_face&		set(uint32_t signature, uint32_t x0, uint32_t x1, uint32_t x2);
		bool		is_same_layer(uint_fast32_t v0, uint_fast32_t v1) const;
		bool		is_duplicate(const b_face& face) const;
		bool		is_back(const b_face& face, const std::vector<fvector3>& normals,
						float normal_tolerance) const;
		uint32_t	next_face_idx(uint_fast32_t vert_idx) const;
		uint32_t	next_face_idx(uint_fast32_t v0, uint_fast32_t v1) const;
		uint_fast32_t	local_vert_idx(uint_fast32_t vert_idx) const;
		bool		is_edge_smooth(const b_face& face, uint_fast32_t v0, uint_fast32_t v1) const;

		union {
			struct {
				uint32_t	v[3];
				uint32_t	n[3];
				uint32_t	tc[3];
				uint32_t	link[3];
			};
			struct {
				uint32_t	v0, v1, v2;
				uint32_t	n0, n1, n2;
				uint32_t	tc0, tc1, tc2;
				uint32_t	link0, link1, link2;
			};
			uint32_t		__refs[12];
		};
		union {
			xr_raw_surface		surface;
			struct {
				uint32_t	surf_idx;
				uint32_t	sgroup;
			};
		};
		uint32_t	tag;
		uint16_t	temp;
		uint16_t	sector;
	};
	TYPEDEF_STD_VECTOR(b_face)

	struct b_edge {
				b_edge(uint_fast32_t _v1, uint_fast32_t _face0, uint_fast32_t _link);

		uint32_t	v1;		// second vertex, first is implied
		uint32_t	face0;		// first face in the adjacency list
		uint32_t	link;		// edges link
		uint16_t	size;
		bool		smooth;
		bool		border;
	};
	TYPEDEF_STD_VECTOR(b_edge)

protected:
	struct b_proxy {
		explicit	b_proxy(size_t _index);

		template<typename T> struct less;
		template<typename T1, typename T2> struct less2;

		uint32_t	index;
	};
	TYPEDEF_STD_VECTOR(b_proxy)

	template<typename T> static void
			create_unique_array(b_proxy_vec& refs, std::vector<T>& data, const T* raw_data, uint32_t* mapping);
	template<typename T1, typename T2> static void
			create_unique_array2(b_proxy_vec& refs, std::vector<T1>& data1, std::vector<T2>& data2,
					const T1* raw_data1, const T2* raw_data2, uint32_t* mapping);

	void		generate_vertex_faces();

	b_edge*		find_edge(uint_fast32_t v0, uint_fast32_t v1);

	void		create_edges(bool calc_smoothness = false);
	void		create_smoothing_groups();

	void		__push(const xr_vbuf& vb, const xr_ibuf& ib, const fmatrix* xform,
					const b_face& face_template, unsigned pflags = PF_DEFAULT);
	void		create_mappings(lw_face_vec& faces, lw_vmref_vec& vmrefs, xr_vmap_vec& vmaps,
					const std::vector<xr_bone*>& bones) const;
	void		compact_raw_surfaces();
	void		remove_empty_surfmaps();

protected:
	xr_mesh_vbuf		m_vb;
	b_proxy_vec		m_refs;
	std::vector<fvector3>	m_normals;
	std::vector<fvector2>	m_texcoords;
	std::vector<finfluence>	m_influences;
	b_face_vec		m_faces;
	b_edge_vec		m_edges;
	std::vector<uint32_t>	m_vertex_faces;
	std::vector<uint32_t>	m_vertex_edges;
	xr_raw_surface_vec	m_raw_surfaces;

	xr_build_err		m_build_err;
};

inline xr_build_err& xr_mesh_builder::build_err() { return m_build_err; }

inline xr_mesh_builder::b_face::b_face(): tag(BAD_IDX), sector(0) { clear(); }
inline xr_mesh_builder::b_face::b_face(uint16_t _sector):
	tag(BAD_IDX), sector(_sector) { clear(); }
inline xr_mesh_builder::b_face::b_face(uint16_t _sector, uint32_t _tag):
	tag(_tag), sector(_sector) { clear(); }

inline uint_fast32_t xr_mesh_builder::b_face::local_vert_idx(uint_fast32_t vert_idx) const
{
	for (uint_fast32_t i = 3; i != 0;) {
		if (v[--i] == vert_idx)
			return i;
	}
	xr_not_expected();
	return BAD_IDX;
}

inline uint32_t xr_mesh_builder::b_face::next_face_idx(uint_fast32_t vert_idx) const
{
	for (uint_fast32_t i = 3; i != 0;) {
		if (v[--i] == vert_idx)
			return link[i];
	}
	xr_not_expected();
	return BAD_IDX;
}

inline uint32_t xr_mesh_builder::b_face::next_face_idx(uint_fast32_t v0, uint_fast32_t v1) const
{
	uint_fast32_t i0 = local_vert_idx(v0);
	uint_fast32_t i1 = local_vert_idx(v1);
	xr_assert(i0 != BAD_IDX && i1 != BAD_IDX);
	// 01: 0	1
	// 02: 2	2
	// 10: 0	-1
	// 12: 1	1
	// 20: 2	-2
	// 21: 1	-1
	int_fast32_t r = i1 - i0;
	if (r > 0)	// i0 < i1
		return r == 2 ? link[2] : link[i0];
	else		// i0 > i1
		return r == -2 ? link[2] : link[i1];
}

inline void xr_mesh_builder::b_face::clear()
{
	std::uninitialized_fill_n(__refs, xr_dim(__refs), BAD_IDX);
	surface.clear();
}

inline xr_mesh_builder::b_edge::b_edge(uint_fast32_t _v1, uint_fast32_t _face0, uint_fast32_t _link):
	v1(uint32_t(_v1)), face0(uint32_t(_face0)), link(uint32_t(_link)), size(1), smooth(false), border(false) {}

inline xr_mesh_builder::b_proxy::b_proxy(size_t _index): index(uint32_t(_index & UINT32_MAX)) {}

} // end of namespace xray_re

#endif
