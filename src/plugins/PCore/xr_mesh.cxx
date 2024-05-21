#include "xr_mesh.h"
#include "xr_object_format.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_object.h"
#include "xr_utils.h"

using namespace xray_re;

xr_vmap::xr_vmap() {}

xr_vmap::xr_vmap(const char* name): m_name(name) {}

xr_vmap::xr_vmap(const std::string& name): m_name(name) {}

xr_vmap::~xr_vmap() {}

inline void xr_vmap::reserve(size_t _size) { m_vertices.reserve(_size); }

inline uint32_t xr_vmap::add_vertex(uint32_t vert_idx)
{
	uint32_t index = uint32_t(size() & UINT32_MAX);
	m_vertices.push_back(vert_idx);
	return index;
}

xr_vmap* xr_vmap::create(const char* name, unsigned type, unsigned dimension, bool discontiguous)
{
	if (type == VMT_UV && dimension == 2) {
		if (discontiguous)
			return new xr_face_uv_vmap(name);
		else
			return new xr_uv_vmap(name);
	} else if (type == VMT_WEIGHT && dimension == 1) {
		return new xr_weight_vmap(name);
	}
	xr_not_expected();
	return 0;
}

void xr_vmap::f_read_0::operator()(xr_vmap*& vmap, xr_reader& r)
{
	const char* name = r.skip_sz();
	vmap = xr_vmap::create(name, VMT_UV, 2, false);
	vmap->load_data(r);
};

void xr_vmap::f_read_1::operator()(xr_vmap*& vmap, xr_reader& r)
{
	const char* name = r.skip_sz();
	unsigned dimension = r.r_u8() & 0x3;
	unsigned type = r.r_u8() & 0x3;
	vmap = xr_vmap::create(name, type, dimension, false);
	vmap->load_data(r);
};

void xr_vmap::f_read_2::operator()(xr_vmap*& vmap, xr_reader& r)
{
	const char* name = r.skip_sz();
	unsigned dimension = r.r_u8() & 0x3;
	bool discontiguous = (r.r_u8() & 0x1) != 0;
	unsigned type = r.r_u8() & 0x3;
	vmap = xr_vmap::create(name, type, dimension, discontiguous);
	vmap->load_data(r);
};

void xr_vmap::f_write::operator()(const xr_vmap* vmap, xr_writer& w) const
{
	w.w_sz(vmap->name());
	vmap->save_data(w);
}

////////////////////////////////////////////////////////////////////////////////

xr_uv_vmap::xr_uv_vmap(const char* name): xr_vmap(name) {}

xr_uv_vmap::xr_uv_vmap(const std::string& name): xr_vmap(name) {}

void xr_uv_vmap::load_data(xr_reader& r)
{
	size_t n = r.r_u32();
	r.r_seq(n, m_uvs);
	r.r_seq(n, m_vertices);
}

void xr_uv_vmap::save_data(xr_writer& w) const
{
	w.w_u8(2);
	w.w_bool(false);
	w.w_u8(VMT_UV);
	w.w_size_u32(size());
	w.w_seq(m_uvs);
	w.w_seq(m_vertices);
}

void xr_uv_vmap::reserve(size_t _size)
{
	xr_vmap::reserve(_size);
	m_uvs.reserve(_size);
}

uint32_t xr_uv_vmap::add_uv(const fvector2& uv, uint32_t vert_idx)
{
	m_uvs.push_back(uv);
	return add_vertex(vert_idx);
}

unsigned xr_uv_vmap::type() const { return VMT_UV; }

////////////////////////////////////////////////////////////////////////////////

xr_face_uv_vmap::xr_face_uv_vmap(const char* name): xr_uv_vmap(name) {}

xr_face_uv_vmap::xr_face_uv_vmap(const std::string& name): xr_uv_vmap(name) {}

void xr_face_uv_vmap::load_data(xr_reader& r)
{
	size_t n = r.r_u32();
	r.r_seq(n, m_uvs);
	r.r_seq(n, m_vertices);
	r.r_seq(n, m_faces);
}

void xr_face_uv_vmap::save_data(xr_writer& w) const
{
	w.w_u8(2);
	w.w_bool(true);
	w.w_u8(VMT_UV);
	w.w_size_u32(size());
	w.w_seq(m_uvs);
	w.w_seq(m_vertices);
	w.w_seq(m_faces);
}

void xr_face_uv_vmap::reserve(size_t _size)
{
	xr_uv_vmap::reserve(_size);
	m_faces.reserve(_size);
}

uint32_t xr_face_uv_vmap::add_uv(const fvector2& uv, uint32_t vert_idx, uint32_t face_idx)
{
	m_faces.push_back(face_idx);
	return xr_uv_vmap::add_uv(uv, vert_idx);
}

////////////////////////////////////////////////////////////////////////////////

xr_weight_vmap::xr_weight_vmap(const char* name): xr_vmap(name) {}

xr_weight_vmap::xr_weight_vmap(const std::string& name): xr_vmap(name) {}

void xr_weight_vmap::load_data(xr_reader& r)
{
	size_t n = r.r_u32();
	r.r_seq(n, m_weights);
	r.r_seq(n, m_vertices);
}

void xr_weight_vmap::save_data(xr_writer& w) const
{
	w.w_u8(1);
	w.w_bool(false);
	w.w_u8(VMT_WEIGHT);
	w.w_size_u32(size());
	w.w_seq(m_weights);
	w.w_seq(m_vertices);
}

void xr_weight_vmap::reserve(size_t _size)
{
	xr_vmap::reserve(_size);
	m_weights.reserve(_size);
}

uint32_t xr_weight_vmap::add_weight(float weight, uint32_t vert_idx)
{
	m_weights.push_back(weight);
	return xr_vmap::add_vertex(vert_idx);
}

unsigned xr_weight_vmap::type() const { return VMT_WEIGHT; }

////////////////////////////////////////////////////////////////////////////////

xr_mesh::xr_mesh(): m_flags(EMF_VALID)
{
	m_bbox.null();
	m_options.unk1 = 0;
	m_options.unk2 = 0;
}

xr_mesh::~xr_mesh()
{
	delete_elements(m_vmaps);
	delete_elements(m_surfmaps);
}

struct read_face { void operator()(lw_face& face, xr_reader& r) const {
	face.v0 = r.r_u32();
	face.ref0 = r.r_u32();
	face.v1 = r.r_u32();
	face.ref1 = r.r_u32();
	face.v2 = r.r_u32();
	face.ref2 = r.r_u32();
}};

struct read_vmref { void operator()(lw_vmref& ref, xr_reader& r) const {
	r.r_seq(r.r_u8(), ref);
}};

struct read_surfmap {
	xr_object& object;
	read_surfmap(xr_object& _object): object(_object) {}
	void operator()(xr_surfmap*& _smap, xr_reader& r) const {
		xr_surfmap* smap = new xr_surfmap;
		_smap = smap;
		const char* name = r.skip_sz();
		smap->surface = object.find_surface(name);
		r.r_seq(r.r_u32(), smap->faces);
	}
};

void xr_mesh::load(xr_reader& r, xr_object& object)
{
	size_t size;

	uint16_t version;
	if (!r.r_chunk<uint16_t>(EMESH_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == EMESH_VERSION);

	if (!r.r_chunk(EMESH_CHUNK_MESHNAME, m_name))
		xr_not_expected();
	r.debug_find_chunk();

	if (!r.r_chunk<fbox>(EMESH_CHUNK_BBOX, m_bbox))
		xr_not_expected();

	if (!r.r_chunk<uint8_t>(EMESH_CHUNK_FLAGS, m_flags))
		xr_not_expected();

	r.r_chunk<lw_options>(EMESH_CHUNK_OPTIONS, m_options);

#if 0
	if (!(size = r.find_chunk(EMESH_CHUNK_VERTS)))
		xr_not_expected();
	r.r_seq(r.r_u32(), m_points);
//	if (m_flags & EMF_3DSMAX) {
//		xr_assert(m_points.size()*sizeof(fvector3) < size);
	if (m_points.size()*sizeof(fvector3) < size) {
		m_adj_counts.reserve(m_points.size());
		m_adj_refs.reserve(m_points.size());
		while (size) {
			uint8_t n = r.r_u8();
			m_adj_counts.push_back(n);
			xr_assert(size >= n*sizeof(uint32_t) + sizeof(uint8_t));
			size -= n*sizeof(uint32_t) + sizeof(uint8_t)
			while (n--)
				m_adj_refs.push_back(r.r_u32());
		}
	}
	r.debug_find_chunk();
#else
	if (!r.find_chunk(EMESH_CHUNK_VERTS))
		xr_not_expected();
	r.r_seq(r.r_u32(), m_points);
	// adjacency data is ignored by AE/xrECoreB.dll as well
#endif

	if (!r.find_chunk(EMESH_CHUNK_FACES))
		xr_not_expected();
	r.r_seq(r.r_u32(), m_faces, read_face());
	r.debug_find_chunk();

	if ((size = r.find_chunk(EMESH_CHUNK_SG))) {
		xr_assert(size == m_faces.size()*sizeof(uint32_t));
		r.r_seq(m_faces.size(), m_sgroups);
		r.debug_find_chunk();
	}

	if (!r.find_chunk(EMESH_CHUNK_VMREFS))
		xr_not_expected();
	r.r_seq(r.r_u32(), m_vmrefs, read_vmref());
	r.debug_find_chunk();

	if (!r.find_chunk(EMESH_CHUNK_SFACE))
		xr_not_expected();
	r.r_seq(r.r_u16(), m_surfmaps, read_surfmap(object));
	r.debug_find_chunk();

	if (r.find_chunk(EMESH_CHUNK_VMAPS_2)) {
		r.r_seq(r.r_u32(), m_vmaps, xr_vmap::f_read_2());
	} else if (r.find_chunk(EMESH_CHUNK_VMAPS_1)) {
		r.r_seq(r.r_u32(), m_vmaps, xr_vmap::f_read_1());
	} else if (r.find_chunk(EMESH_CHUNK_VMAPS_0)) {
		r.r_seq(r.r_u32(), m_vmaps, xr_vmap::f_read_0());
	} else {
		xr_not_expected();
	}
	r.debug_find_chunk();
#if 1
	for (lw_face_vec_it it = m_faces.begin(); it != m_faces.end(); ++it) {
		xr_assert(it->v0 < m_points.size());
		xr_assert(it->v1 < m_points.size());
		xr_assert(it->v2 < m_points.size());
		xr_assert(it->ref0 < m_vmrefs.size());
		xr_assert(it->ref1 < m_vmrefs.size());
		xr_assert(it->ref2 < m_vmrefs.size());
	}
#endif
}

struct write_face { void operator()(const lw_face& face, xr_writer& w) const {
	w.w_u32(face.v0);
	w.w_u32(face.ref0);
	w.w_u32(face.v1);
	w.w_u32(face.ref1);
	w.w_u32(face.v2);
	w.w_u32(face.ref2);
}};

struct write_vmref { void operator()(const lw_vmref& ref, xr_writer& w) const {
	w.w_size_u8(ref.size());
	w.w_seq(ref);
}};

struct write_surfmap { void operator()(const xr_surfmap* sm, xr_writer& w) const {
	w.w_sz(sm->surface->name());
	w.w_size_u32(sm->faces.size());
	w.w_seq(sm->faces);
}};

void xr_mesh::save(xr_writer& w) const
{
	w.w_chunk<uint16_t>(EMESH_CHUNK_VERSION, EMESH_VERSION);
	w.w_chunk(EMESH_CHUNK_MESHNAME, m_name);
	w.w_chunk<fbox>(EMESH_CHUNK_BBOX, m_bbox);
	w.w_chunk<uint8_t>(EMESH_CHUNK_FLAGS, m_flags);
	w.w_chunk<lw_options>(EMESH_CHUNK_OPTIONS, m_options);

	w.open_chunk(EMESH_CHUNK_VERTS);
	w.w_size_u32(m_points.size());
	w.w_seq(m_points);
	w.close_chunk();

	w.open_chunk(EMESH_CHUNK_FACES);
	w.w_size_u32(m_faces.size());
	w.w_seq(m_faces, write_face());
	w.close_chunk();

	if (!m_sgroups.empty()) {
		w.open_chunk(EMESH_CHUNK_SG);
		w.w_seq(m_sgroups);
		w.close_chunk();
	}

	if (!m_vertex_normals.empty()) 
	{
		w.open_chunk(EMESH_CHUNK_VNORMALS);
		w.w_seq(m_vertex_normals);
		w.close_chunk();
	}

	//if (!m_face_normals.empty())
	//{
	//	w.open_chunk(EMESH_CHUNK_FNORMALS);
	//	w.w_seq(m_face_normals);
	//	w.close_chunk();
	//}

	w.open_chunk(EMESH_CHUNK_VMREFS);
	w.w_size_u32(m_vmrefs.size());
	w.w_seq(m_vmrefs, write_vmref());
	w.close_chunk();

	w.open_chunk(EMESH_CHUNK_SFACE);
	w.w_size_u16(m_surfmaps.size());
	w.w_seq(m_surfmaps, write_surfmap());
	w.close_chunk();

	w.open_chunk(EMESH_CHUNK_VMAPS_2);
	w.w_size_u32(m_vmaps.size());
	w.w_seq(m_vmaps, xr_vmap::f_write());
	w.close_chunk();
}

void xr_mesh::calculate_bbox()
{
	if (m_points.empty()) {
		m_bbox.null();
	} else {
		m_bbox.invalidate();
		for (std::vector<fvector3>::iterator it = m_points.begin(),
				end = m_points.end(); it != end; ++it) {
			m_bbox.extend(*it);
		}
	}
}
