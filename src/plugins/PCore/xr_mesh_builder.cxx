#include "xr_mesh_builder.h"
#include "xr_object.h"
#include "xr_utils.h"
#include "xr_mesh_utils.h"

using namespace xray_re;

xr_mesh_builder::b_face& xr_mesh_builder::b_face::set(uint32_t signature, uint32_t x0, uint32_t x1, uint32_t x2)
{
	xr_assert(signature);
	if (signature & xr_vbuf::S_POINTS) {
		v0 = x0;
		v1 = x1;
		v2 = x2;
	}
	if (signature & xr_vbuf::S_NORMALS) {
		n0 = x0;
		n1 = x1;
		n2 = x2;
	}
	if (signature & xr_vbuf::S_TEXCOORDS) {
		tc0 = x0;
		tc1 = x1;
		tc2 = x2;
	}
	return *this;
}

bool xr_mesh_builder::b_face::is_duplicate(const b_face& face) const
{
	for (uint_fast32_t i0 = 0; i0 != 3; ++i0) {
		if (v[i0] != face.v0)
			continue;
		uint_fast32_t i1 = (1 << i0) & 3;
		uint_fast32_t i2 = (1 << i1) & 3;
		return v[i1] == face.v1 && v[i2] == face.v2 &&
				sector == face.sector && surface == face.surface &&
				n[i0] == face.n0 && n[i1] == face.n1 && n[i2] == face.n2 &&
				tc[i0] == face.tc0 && tc[i1] == face.tc1 && tc[i2] == face.tc2;
	}
	return false;
}

bool xr_mesh_builder::b_face::is_back(const b_face& face,
		const std::vector<fvector3>& normals, float normal_tolerance) const
{
	for (uint_fast32_t i0 = 0; i0 != 3; ++i0) {
		if (v[i0] != face.v0)
			continue;
		uint_fast32_t i1 = (1 << i0) & 3;
		uint_fast32_t i2 = (1 << i1) & 3;
		return v[i1] == face.v2 && v[i2] == face.v1 &&
				sector == face.sector && surface == face.surface &&
				tc[i0] == face.tc0 && tc[i1] == face.tc2 && tc[i2] == face.tc1 &&
				normals[n[i0]].inverted(normals[face.n0], normal_tolerance) &&
				normals[n[i1]].inverted(normals[face.n2], normal_tolerance) &&
				normals[n[i2]].inverted(normals[face.n1], normal_tolerance);
	}
	return false;
}

bool xr_mesh_builder::b_face::is_same_layer(uint_fast32_t v0, uint_fast32_t v1) const
{
	return ((1u << local_vert_idx(v0)) & 3) != local_vert_idx(v1);
}

bool xr_mesh_builder::b_face::is_edge_smooth(const b_face& face, uint_fast32_t v0, uint_fast32_t v1) const
{
	uint_fast32_t i0 = local_vert_idx(v0);
	uint_fast32_t i1 = local_vert_idx(v1);
	uint_fast32_t j0 = face.local_vert_idx(v0);
	uint_fast32_t j1 = face.local_vert_idx(v1);
	return n[i0] == face.n[j0] && n[i1] == face.n[j1] &&
			v[3 - i0 - i1] != face.v[3 - j0 - j1];
}

////////////////////////////////////////////////////////////////////////////////

xr_mesh_builder::xr_mesh_builder() {}

xr_mesh_builder::~xr_mesh_builder() {}

void xr_mesh_builder::set_tc_fix(bool tc_fix) { m_vb.set_tc_fix(tc_fix); }

template<typename T> struct xr_mesh_builder::b_proxy::less {
	const T* data;
	explicit less(const T* _data): data(_data) {}
	inline bool operator()(const b_proxy& l, const b_proxy& r) const {
		return data[l.index] < data[r.index];
	}
};

template<typename T1, typename T2> struct xr_mesh_builder::b_proxy::less2 {
	const T1* data1;
	const T2* data2;
	explicit less2(const T1* _data1, const T2* _data2): data1(_data1), data2(_data2) {}
	inline bool operator()(const b_proxy& l, const b_proxy& r) const {
		return data1[l.index] < data1[r.index] ||
				(data1[l.index] == data1[r.index] && data2[l.index] < data2[r.index]);
	}
};

void xr_mesh_builder::prepare(uint32_t signature, size_t num_vertices, size_t num_indices)
{
	m_vb.set_signature(signature);
	m_vb.reserve(num_vertices);
	m_refs.reserve(num_vertices);
	m_faces.reserve(num_indices/3);
}

void xr_mesh_builder::push(const xr_vbuf& vb, const xr_ibuf& ib, uint16_t texture, uint16_t eshader)
{
	b_face face_template;
	face_template.surface.set(texture, eshader);
	if (vb.has_lightmaps())
		face_template.surface.flags |= RSF_LIGHTMAP;
	__push(vb, ib, 0, face_template, PF_DEFAULT);
}

void xr_mesh_builder::__push(const xr_vbuf& vb, const xr_ibuf& ib, const fmatrix* xform,
		const b_face& face_template, unsigned pflags)
{
	size_t num_new_faces = 0;
	uint32_t signature = m_vb.signature(), vb_offset = uint32_t(m_vb.size() & UINT32_MAX);
	b_face face(face_template);
	for (size_t i = 0, num_indices = ib.size(); i != num_indices; i += 3) {
		uint32_t v0 = ib[i + 0];
		uint32_t v1 = ib[i + 1];
		const fvector3& p0 = vb.p(v0);
		const fvector3& p1 = vb.p(v1);
		if (p0 == p1)
			continue;
		uint32_t v2 = ib[i + 2];
		const fvector3& p2 = vb.p(v2);
		if (p1 == p2 || p2 == p0)
			continue;

		float area = calc_area(p0, p1, p2);
		if (area <= 1e-5f) {
			m_build_err.zero_area_face(p0, p1, p2);
			if (pflags & PF_REMOVE_ZERO_FACE)
				continue;
		}
		float perim = calc_perimeter(vb.tc(v0), vb.tc(v1), vb.tc(v2));
		if (perim <= 1/8192.f) {
			m_build_err.zero_uv_area_face(p0, p1, p2);
			if (pflags & PF_REMOVE_ZERO_UV_FACE)
				continue;
		}
#if 0
		if (vb.has_lightmaps()) {
			const fvector2& lm0 = vb.lm(v0);
			const fvector2& lm1 = vb.lm(v1);
			const fvector2& lm2 = vb.lm(v2);
#if 1
			fvector3 uv0, uv1, uv2;
			uv0.set(lm0.x, 0, lm0.y);
			uv1.set(lm1.x, 0, lm1.y);
			uv2.set(lm2.x, 0, lm2.y);
			float k = calc_area_xz(uv0, uv1, uv2)*1024.f*1024.f/calc_area(p0, p1, p2);
			printf("lm_density: %f\n", k);
#else
			float k01 = lm0.distance(lm1)*1024.f/p0.distance(p1);
			float k12 = lm1.distance(lm2)*1024.f/p1.distance(p2);
			float k20 = lm2.distance(lm0)*1024.f/p2.distance(p0);
//			if (equivalent(k01, 0.f)) {
			if (equivalent(k01, 5.f)) {
//			if (k01 < 2.f) {
				printf("  face: [%f,%f,%f][%f,%f,%f][%f,%f,%f],\n  lm: [%f,%f][%f,%f][%f,%f]\n",
						p0.x, p0.y, p0.z, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z,
						lm0.x, lm0.y, lm1.x, lm1.y, lm2.x, lm2.y);
			}
			printf("lm_density: %.2f/%.2f/%.2f\n", k01, k12, k20);
#endif
		}
#endif
		m_faces.push_back(face.set(signature, vb_offset + v0, vb_offset + v1, vb_offset + v2));
		++num_new_faces;
	}
	if (num_new_faces) {
		for (size_t i = vb.size(); i != 0; --i, ++vb_offset)
			m_refs.push_back(b_proxy(vb_offset));
		m_vb.push(vb, &ib, xform);
	}
}

template<typename T> void
xr_mesh_builder::create_unique_array(b_proxy_vec& refs, std::vector<T>& data, const T* raw_data, uint32_t* mapping)
{
	std::sort(refs.begin(), refs.end(), b_proxy::less<T>(raw_data));
	uint32_t index = 0;
	for (b_proxy_vec_it it = refs.begin(), end = refs.end(); it != end; ++index) {
		const T& value = raw_data[it->index];
		data.push_back(value);
		mapping[it->index] = index;
		while (++it != end && raw_data[it->index] == value)
			mapping[it->index] = index;
	}
}

template<typename T1, typename T2> void
xr_mesh_builder::create_unique_array2(b_proxy_vec& refs, std::vector<T1>& data1, std::vector<T2>& data2,
		const T1* raw_data1, const T2* raw_data2, uint32_t* mapping)
{
	std::sort(refs.begin(), refs.end(), b_proxy::less2<T1, T2>(raw_data1, raw_data2));
	uint32_t index = 0;
	for (b_proxy_vec_it it = refs.begin(), end = refs.end(); it != end; ++index) {
		const T1& value1 = raw_data1[it->index];
		data1.push_back(value1);
		const T2& value2 = raw_data2[it->index];
		data2.push_back(value2);
		mapping[it->index] = index;
		while (++it != end && raw_data1[it->index] == value1 && raw_data2[it->index] == value2)
			mapping[it->index] = index;
	}
}

void xr_mesh_builder::compact_geometry()
{
	uint32_t* mapping = new uint32_t[m_refs.size()];
	if (m_vb.has_influences())
		create_unique_array2(m_refs, m_points, m_influences, m_vb.p(), m_vb.w(), mapping);
	else
		create_unique_array(m_refs, m_points, m_vb.p(), mapping);
	for (b_face_vec_it it = m_faces.begin(), end = m_faces.end(); it != end; ++it) {
		it->v0 = mapping[it->v0];
		it->v1 = mapping[it->v1];
		it->v2 = mapping[it->v2];
	}
	if (m_vb.has_normals()) {
		create_unique_array(m_refs, m_normals, m_vb.n(), mapping);
		for (b_face_vec_it it = m_faces.begin(), end = m_faces.end(); it != end; ++it) {
			it->n0 = mapping[it->n0];
			it->n1 = mapping[it->n1];
			it->n2 = mapping[it->n2];
		}
	}
	if (m_vb.has_texcoords()) {
		create_unique_array(m_refs, m_texcoords, m_vb.tc(), mapping);
		for (b_face_vec_it it = m_faces.begin(), end = m_faces.end(); it != end; ++it) {
			it->tc0 = mapping[it->tc0];
			it->tc1 = mapping[it->tc1];
			it->tc2 = mapping[it->tc2];
		}
	}
	delete[] mapping;

	m_vb.clear();
	b_proxy_vec().swap(m_refs);

#if 0
	for (b_face_vec_it it = m_faces.begin(), end = m_faces.end(); it != end; ++it) {
		fvector3 n1, n2, n3;
		uint16_t packed1 = n1.calc_normal(m_points[it->v0], m_points[it->v1], m_points[it->v2]).compress();
		uint16_t packed2 = n2.decompress(packed1).compress();
		uint16_t packed3 = n3.decompress(packed2).compress();
		msg("distance12=%f, distance23=%f", n1.distance(n2), n2.distance(n3));
		xr_assert(n1.distance(n2) < 0.3f);
	}
#endif
}

struct bad_face_pred { bool operator()(const xr_mesh_builder::b_face& face) const { return face.surf_idx == BAD_IDX; }};

void xr_mesh_builder::remove_duplicate_faces()
{
	generate_vertex_faces();
	for (b_face_vec_it it = m_faces.begin(), end = m_faces.end(); it != end; ++it) {
		for (uint_fast32_t next = m_vertex_faces[it->v0]; next != BAD_IDX;) {
			b_face* face = &m_faces[next];
			if (face > &*it && it->is_duplicate(*face)) {
				it->surf_idx = BAD_IDX;
				goto skip;
			}
			next = face->next_face_idx(it->v0);
		}
skip:;
	}
	m_faces.erase(std::remove_if(m_faces.begin(), m_faces.end(), bad_face_pred()), m_faces.end());
}

// FIXME: it should be possible to determine the "right" side in
// model (external) OGFs using simple face ordering heuristics.
// Not sure for the level (embedded) OGFs though.
void xr_mesh_builder::remove_back_faces(float normal_tolerance)
{
	generate_vertex_faces();

	size_t num_faces = m_faces.size();

	uint32_t* face_backs = new uint32_t[num_faces];
	xr_uninitialized_fill_n(face_backs, num_faces, BAD_IDX);
	uint32_t face_idx = 0;
	for (b_face_vec_cit it = m_faces.begin(), end = m_faces.end();
			it != end; ++it, ++face_idx) {
		for (uint_fast32_t next = m_vertex_faces[it->v0]; next != BAD_IDX;) {
			b_face& face = m_faces[next];
			if (it->is_back(face, m_normals, normal_tolerance)) {
				face_backs[face_idx] = uint32_t(next & UINT32_MAX);
				face_backs[next] = face_idx;
				break;
			}
			next = face.next_face_idx(it->v0);
		}
	}

	create_edges();

	std::vector<uint32_t> adjacents;
	adjacents.reserve(512);
	std::vector<int> sides;
	sides.reserve(256);

	uint32_t* lgroups = new uint32_t[num_faces];
	xr_uninitialized_fill_n(lgroups, num_faces, BAD_IDX);

	uint32_t lgroup = 0;
	for (size_t k = 0; k != num_faces; ++k) {
		if (face_backs[k] == BAD_IDX || lgroups[k] != BAD_IDX)
			continue;
		int side = 0;
		face_idx = uint32_t(k & UINT32_MAX);
		lgroups[face_idx] = lgroup;
		lgroups[face_backs[face_idx]] = lgroup + 1;
		for (;;) {
			const b_face& face = m_faces[face_idx];
			for (uint_fast32_t i = 3, v0, v1 = face.v0; i != 0; v1 = v0) {
				b_edge* edge = find_edge(v0 = face.v[--i], v1);
				xr_assert(edge && (v0 == edge->v1 || v1 == edge->v1));
				if (edge->size > 4)
					continue;
				for (uint_fast32_t next = edge->face0; next != BAD_IDX;) {
					const b_face& adjacent = m_faces[next];
					if (next != face_idx && next != face_backs[face_idx] &&
							lgroups[next] == BAD_IDX) {
						bool same_layer = adjacent.is_same_layer(v0, v1);
						uint_fast32_t next_back = face_backs[next];
						if (next_back == BAD_IDX) {
							if (edge->size > 3)
								break;
							if (same_layer)
								--side;
							else
								++side;
						} else if (same_layer) {
							adjacents.push_back(uint32_t(next & UINT32_MAX));
							lgroups[next] = lgroup;
							lgroups[next_back] = lgroup + 1;
							break;
						} else {
							adjacents.push_back(uint32_t(next_back & UINT32_MAX));
							lgroups[next_back] = lgroup;
							lgroups[next] = lgroup + 1;
							break;
						}
					}

					// check for endless cycle. It`s in build 2232 l06_rostok
					uint_fast32_t temp = adjacent.next_face_idx(v0, v1);
					if (next == temp)
						break;
					next = temp;
				}
			}
			if (adjacents.empty())
				break;
			face_idx = adjacents.back();
			adjacents.pop_back();

		}
		if (side == 0)
			side = -1;
		sides.push_back(side);
		lgroup += 2;
	}

	face_idx = 0;
	b_face_vec_it last = m_faces.begin();
	for (b_face_vec_it it = last, end = m_faces.end(); it != end; ++it, ++face_idx) {
		if (face_backs[face_idx] != BAD_IDX) {
			int side = sides[(lgroup = lgroups[face_idx])/2];
			if (lgroup & 1)
				side = -side;
			if (side < 0)
				it->surface.flags |= RSF_TWO_SIDED;
			else
				continue;
		}
		if (last != it)
			*last = *it;
		++last;
	}
	m_faces.erase(last, m_faces.end());

	delete[] lgroups;
	delete[] face_backs;
}

void xr_mesh_builder::generate_vertex_faces()
{
	m_vertex_faces.assign(m_points.size(), BAD_IDX);
	uint32_t face_idx = 0;
	for (b_face_vec_it it = m_faces.begin(), end = m_faces.end();
			it != end; ++it, ++face_idx) {
		it->link0 = m_vertex_faces[it->v0];
		it->link1 = m_vertex_faces[it->v1];
		it->link2 = m_vertex_faces[it->v2];
		m_vertex_faces[it->v0] = face_idx;
		m_vertex_faces[it->v1] = face_idx;
		m_vertex_faces[it->v2] = face_idx;
	}
}

xr_mesh_builder::b_edge* xr_mesh_builder::find_edge(uint_fast32_t v0, uint_fast32_t v1)
{
	for (uint_fast32_t next = m_vertex_edges[v0]; next != BAD_IDX;) {
		b_edge* edge = &m_edges[next];
		if (edge->v1 == v1)
			return edge;
		next = edge->link;
	}
	for (uint_fast32_t next = m_vertex_edges[v1]; next != BAD_IDX;) {
		b_edge* edge = &m_edges[next];
		if (edge->v1 == v0)
			return edge;
		next = edge->link;
	}
	return 0;
}

void xr_mesh_builder::create_edges(bool calc_smoothness)
{
	m_vertex_edges.assign(m_points.size(), BAD_IDX);
	m_edges.clear();
	m_edges.reserve(m_points.size());
	uint32_t face_idx = 0;
	for (b_face_vec_it it = m_faces.begin(), end = m_faces.end();
			it != end; ++it, ++face_idx) {
		for (uint_fast32_t i = 3, v0, v1 = it->v0; i != 0; v1 = v0) {
			b_edge* edge = find_edge(v0 = it->v[--i], v1);
			if (edge == 0) {
				it->link[i] = BAD_IDX;
				uint32_t next = m_vertex_edges[v0];
				m_vertex_edges[v0] = uint32_t(m_edges.size() & UINT32_MAX);
				m_edges.push_back(b_edge(v1, face_idx, next));
			} else {
				it->link[i] = edge->face0;
				if (!calc_smoothness) {
					++edge->size;
				} else {
					edge->smooth = (++edge->size == 2) ?
							it->is_edge_smooth(m_faces[edge->face0], v0, v1) : false;
				}
				edge->face0 = face_idx;
			}
		}
	}
}

////////////////////////////////////////////////////////////////////////////////

void xr_mesh_builder::create_smoothing_groups()
{
	if (m_normals.empty())
		return;

	create_edges(true);

	m_sgroups.assign(m_faces.size(), EMESH_NO_SG);
	std::vector<uint32_t> adjacents;
	adjacents.reserve(512);
	uint32_t sgroup = 0;
	for (uint_fast32_t idx = uint32_t(m_faces.size() & UINT32_MAX); idx != 0;) {
		if (m_sgroups[--idx] != EMESH_NO_SG)
			continue;
		uint32_t tag = m_faces[idx].tag;
		bool new_sgroup = false;
		for (uint_fast32_t face_idx = idx;;) {
			const b_face& face = m_faces[face_idx];
			for (uint_fast32_t i = 3, v0, v1 = face.v0; i != 0; v1 = v0) {
				b_edge* edge = find_edge(v0 = face.v[--i], v1);
				xr_assert(edge);
				if (!edge->smooth)
					continue;
				if (!new_sgroup) {
					new_sgroup = true;
					m_sgroups[face_idx] = sgroup;
				}
				uint32_t adjacent = (edge->face0 == face_idx) ?
						face.link[i] : edge->face0;
				if (m_sgroups[adjacent] == EMESH_NO_SG &&
						m_faces[adjacent].tag == tag) {
					adjacents.push_back(adjacent);
					m_sgroups[adjacent] = sgroup;
				}
			}
			if (adjacents.empty())
				break;
			face_idx = adjacents.back();
			adjacents.pop_back();
		}
		if (new_sgroup)
			++sgroup;
	}
}

struct surface_pred { bool operator()(const xr_mesh_builder::b_face* l, const xr_mesh_builder::b_face* r) const {
	return l->surface < r->surface;
}};

void xr_mesh_builder::compact_raw_surfaces()
{
	b_face **refs = new b_face*[m_faces.size()];
	b_face **ref = refs;
	b_face **refs_end = refs + m_faces.size();
	for (b_face_vec_it it = m_faces.begin(), end = m_faces.end(); it != end; ++it)
		*ref++ = &*it;
	std::sort(refs, refs_end, surface_pred());

	uint32_t surf_idx = 0;
	for (ref = refs; ref != refs_end; ++surf_idx) {
		b_face* face = *ref;
		xr_raw_surface surface(face->surface);
		m_raw_surfaces.push_back(surface);
		face->surf_idx = surf_idx;
		face->sgroup = EMESH_NO_SG;
		while (++ref != refs_end && (face = *ref)->surface == surface) {
			face->surf_idx = surf_idx;
			face->sgroup = EMESH_NO_SG;
		}
	}
	delete[] refs;
}

void xr_mesh_builder::create_mappings(lw_face_vec& faces, lw_vmref_vec& vmrefs, xr_vmap_vec& vmaps,
		const xr_bone_vec& bones) const
{
	xr_uv_vmap* uv_vmap = 0;
	xr_face_uv_vmap* face_uv_vmap = 0;
	uint32_t vert_idx = 0;
	for (std::vector<uint32_t>::const_iterator it = m_vertex_faces.begin(),
			end = m_vertex_faces.end(); it != end; ++it, ++vert_idx) {
		if (*it == BAD_IDX) {
			const fvector3& p = m_points[vert_idx];
			msg("unused vertex %g,%g,%g", p.x, p.y, p.z);
			continue;
		}

		lw_vmref vmref0;
		uint32_t tc0 = BAD_IDX, vmref0_idx = 0;
		for (uint32_t face_idx = *it; face_idx != BAD_IDX;) {
			const b_face& face = m_faces[face_idx];
			if (face_idx == BAD_IDX) {
				face_idx = face.next_face_idx(vert_idx);
				continue;
			}
			uint_fast32_t local_idx = face.local_vert_idx(vert_idx);
			uint32_t tc = face.tc[local_idx], vmref_idx;
			xr_assert(tc != BAD_IDX);
			if (tc0 == BAD_IDX) {
				if (uv_vmap == 0) {
					uv_vmap = new xr_uv_vmap("Texture");
					vmaps.push_back(uv_vmap);
				}
				vmref0.push_back(lw_vmref_entry(0, uv_vmap->add_uv(
						m_texcoords[tc0 = tc], vert_idx)));
				vmref0_idx = vmref_idx = uint32_t(vmrefs.size() & UINT32_MAX);
				vmrefs.push_back(vmref0);
			} else if (tc0 == tc) {
				vmref_idx = vmref0_idx;
			} else {
				if (face_uv_vmap == 0) {
					face_uv_vmap = new xr_face_uv_vmap("Texture");
					vmaps.push_back(face_uv_vmap);
				}
				lw_vmref vmref;
				vmref.push_back(lw_vmref_entry(1, face_uv_vmap->add_uv(
						m_texcoords[tc], vert_idx, face_idx)));
				vmref_idx = uint32_t(vmrefs.size() & UINT32_MAX);
				vmrefs.push_back(vmref);
			}
			faces[face_idx].ref[local_idx] = vmref_idx;
			face_idx = face.link[local_idx];
		}
	}
	// bones.empty() check is required when trying to convert external LOD parts in older builds.
	if (!m_influences.empty() && !bones.empty()) {
		uint32_t offset = uint32_t(vmaps.size() & UINT32_MAX);
		vmaps.resize(offset + bones.size());
		lw_vmref_vec weight_vmrefs(m_influences.size());
		vert_idx = 0;
		for (std::vector<finfluence>::const_iterator it = m_influences.begin(),
				end = m_influences.end(); it != end; ++it, ++vert_idx) {
			for (finfluence::const_iterator it1 = it->begin(), end1 = it->end();
					it1 != end1; ++it1) {
				xr_weight_vmap* weight_vmap = static_cast<xr_weight_vmap*>(vmaps.at(offset + it1->bone));
				if (weight_vmap == 0) {
					vmaps[offset + it1->bone] = weight_vmap =
							new xr_weight_vmap(bones.at(it1->bone)->name());
				}
				uint32_t weight_idx = weight_vmap->add_weight(it1->weight, vert_idx);
				xr_assert(!weight_vmrefs[vert_idx].full());
				weight_vmrefs[vert_idx].push_back(lw_vmref_entry(offset + it1->bone, weight_idx));
			}
		}
		for (lw_face_vec_it it = faces.begin(), end = faces.end(); it != end; ++it) {
			for (uint_fast32_t i = 3; i != 0;) {
				lw_vmref& vmref = vmrefs[it->ref[--i]];
				if (vmref.size() > 1)
					continue;
				vmref.append(weight_vmrefs[it->v[i]]);
			}
		}
	}
	uint32_t* vmap_remap = new uint32_t[vmaps.size()];
	unsigned used = 0;
	for (uint_fast32_t i = 0, n = vmaps.size(); i != n; ++i) {
		if (i != used)
			vmaps[used] = vmaps[i];
		vmap_remap[i] = vmaps[i] ? used++ : BAD_IDX;
	}
	vmaps.resize(used);
	for (lw_vmref_vec_it it = vmrefs.begin(), end = vmrefs.end(); it != end; ++it) {
		for (lw_vmref::iterator it1 = it->begin(), end1 = it->end(); it1 != end1; ++it1)
			it1->vmap = vmap_remap[it1->vmap];
	}
	delete[] vmap_remap;
}

void xr_mesh_builder::commit(xr_object& object)
{
	compact_raw_surfaces();
	m_surfmaps.reserve(m_raw_surfaces.size());
	for (xr_raw_surface_vec_it it = m_raw_surfaces.begin(), end = m_raw_surfaces.end();
			it != end; ++it) {
		xr_surfmap* smap = new xr_surfmap(object.attach(*it));
		m_surfmaps.push_back(smap);
	}
	xr_raw_surface_vec().swap(m_raw_surfaces);

	create_smoothing_groups();
	std::vector<fvector3>().swap(m_normals);
	b_edge_vec().swap(m_edges);
	std::vector<uint32_t>().swap(m_vertex_edges);

	xr_mesh::m_faces.reserve(m_faces.size());
	uint32_t face_idx = 0;
	for (b_face_vec_it it = m_faces.begin(), end = m_faces.end();
			it != end; ++it, ++face_idx) {
		xr_mesh::m_faces.push_back(lw_face(it->v0, it->v1, it->v2));
		xr_assert(it->surf_idx != BAD_IDX);
		m_surfmaps[it->surf_idx]->faces.push_back(face_idx);
	}

	generate_vertex_faces();
	create_mappings(xr_mesh::m_faces, m_vmrefs, m_vmaps, object.bones());
	std::vector<fvector2>().swap(m_texcoords);
	std::vector<finfluence>().swap(m_influences);
	std::vector<uint32_t>().swap(m_vertex_faces);
	b_face_vec().swap(m_faces);

	calculate_bbox();

	object.meshes().push_back(this);
}

void xr_mesh_builder::remove_empty_surfmaps()
{
	xr_surfmap_vec_it last = m_surfmaps.begin();
	for (xr_surfmap_vec_it it = last, end = m_surfmaps.end(); it != end; ++it) {
		xr_surfmap* smap = *it;
		if (smap->faces.empty()) {
			delete smap;
			continue;
		}
		if (it != last)
			*last = smap;
		++last;
	}
	m_surfmaps.erase(last, m_surfmaps.end());
}
