#include <algorithm>
#include "xr_math.h"
#include "xr_ai_version.h"
#include "xr_ai_map.h"
#include "xr_level_ai.h"
#include "xr_file_system.h"

using namespace xray_re;

const uint32_t AI_MAP_LINK_MASK = 0x7fffff;

// FIXME: won't work on non-IA32 machine
inline uint32_t as_u32(const uint8_t p[4]) { return *reinterpret_cast<const uint32_t*>(p); }

uint32_t ai_node::link(uint_fast32_t side) const
{
	switch (side) {
	case 0: return as_u32(data) & AI_MAP_LINK_MASK;
	case 1: return (as_u32(data + 2) >> 7) & AI_MAP_LINK_MASK;
	case 2: return (as_u32(data + 5) >> 6) & AI_MAP_LINK_MASK;
	case 3: return (as_u32(data + 8) >> 5) & AI_MAP_LINK_MASK;
	}
	return AI_MAP_BAD_LINK;
}

xr_level_ai::xr_level_ai(): m_version(AI_VERSION_8),
	m_num_nodes(0), m_size(0.7f), m_size_y(1.f), m_nodes(0)
{
	m_aabb.null();
	m_guid.reset();
}

xr_level_ai::~xr_level_ai()
{
	delete[] m_nodes;
}

struct read_node_v8 { void operator()(ai_node& node, xr_reader& r) const {
	r.r_cseq(12, node.data);
	node.low_cover = node.cover = r.r_u16();
	node.plane = r.r_u16();
	node.packed_xz = r.r_u24();
	node.packed_y = r.r_u16();
}};

struct write_node_v8 { void operator()(const ai_node& node, xr_writer& w) const {
	w.w_cseq(12, node.data);
	w.w_u16(node.cover);
	w.w_u16(node.plane);
	w.w_u24(node.packed_xz);
	w.w_u16(node.packed_y);
}};

// FIXME: see note about high/low cover order in xr_ai_map.h
struct read_node_v10 { void operator()(ai_node& node, xr_reader& r) const {
	r.r_cseq(12, node.data);
	node.cover = r.r_u16();
	node.low_cover = r.r_u16();
	node.plane = r.r_u16();
	node.packed_xz = r.r_u24();
	node.packed_y = r.r_u16();
}};

struct write_node_v10 { void operator()(const ai_node& node, xr_writer& w) const {
	w.w_cseq(12, node.data);
	w.w_u16(node.cover);
	w.w_u16(node.low_cover);
	w.w_u16(node.plane);
	w.w_u24(node.packed_xz);
	w.w_u16(node.packed_y);
}};

void xr_level_ai::load(xr_reader& r)
{
	m_version = r.r_u32();
	xr_assert(m_version >= AI_VERSION_4 && m_version <= AI_VERSION_10 || m_version == AI_VERSION_3);
	m_num_nodes = r.r_u32();
	m_size = r.r_float();
	m_size_y = r.r_float();
	r.r_fvector3(m_aabb.min);
	r.r_fvector3(m_aabb.max);
	if (m_version >= AI_VERSION_8)
		m_guid.load(r);

	if (m_version >= AI_VERSION_3 && m_version <= AI_VERSION_6) {
		m_num_nodes = 0; // because unknown format
		m_nodes = 0;
	} else {
		m_nodes = new ai_node[m_num_nodes];
		if (m_version >= AI_VERSION_10)
			r.r_cseq(m_num_nodes, m_nodes, read_node_v10());
		else
			r.r_cseq(m_num_nodes, m_nodes, read_node_v8());
	}

	m_column_length = unsigned(std::floor((m_aabb.max.x - m_aabb.min.x)/m_size + 1.501f));
	m_row_length = unsigned(std::floor((m_aabb.max.z - m_aabb.min.z)/m_size + 1.501f));
};

void xr_level_ai::save(xr_writer& w) const
{
	w.w_u32(m_version);
	w.w_u32(m_num_nodes);
	w.w_float(m_size);
	w.w_float(m_size_y);
	w.w(m_aabb);
	m_guid.save(w);
	if (m_version >= AI_VERSION_10)
		w.w_cseq(m_num_nodes, m_nodes, write_node_v10());
	else
		w.w_cseq(m_num_nodes, m_nodes, write_node_v8());
}

bool xr_level_ai::load(const char* key, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(key, name);
	if (r == 0)
		return false;
	load(*r);
	assert(r->eof());
	fs.r_close(r);
	return true;
}

bool xr_level_ai::save(const char* key, const char* name) const
{
	xr_file_system& fs = xr_file_system::instance();
	xr_writer* w = fs.w_open(key, name);
	if (w == 0)
		return false;
	save(*w);
	fs.w_close(w);
	return true;
}

void xr_level_ai::clear()
{
	delete[] m_nodes;
	m_nodes = 0;
}

void xr_level_ai::vertex_position(uint32_t& packed_xz, uint16_t& packed_y, const fvector3& position) const
{
	float x = (position.x - m_aabb.min.x)/m_size + 0.5f;
	float z = (position.z - m_aabb.min.z)/m_size + 0.5f;
	packed_xz = uint32_t(std::floor(x))*m_row_length + uint32_t(std::floor(z));
	packed_y = uint16_t(std::floor((position.y - m_aabb.min.y)/m_size_y*65535.f + 1e-7f));
}

float xr_level_ai::vertex_plane_y(const ai_node& node, float x, float z) const
{
	float x0, z0, y0;
	unpack_xz(node.packed_xz, x0, z0);
	unpack_y(node.packed_y, y0);

	fvector3 plane;
	plane.decompress(node.plane);
	float m = std::sqrt(1.f/plane.square_magnitude()), ky = plane.y*m;
	if (equivalent(ky, 0.f, 1e-7f)) {
		return y0;
	} else {
		float kx = plane.x*m;
		float kz = plane.z*m;
		float v0 = kx*x0 + ky*y0 + kz*z0;
		return y0 - (kx*x + ky*y0 + kz*z - v0)/ky;
	}
}

uint32_t xr_level_ai::vertex_id(const fvector3& position) const
{
	ai_node sample;
	vertex_position(sample.packed_xz, sample.packed_y, position);
	const ai_node* end = m_nodes + m_num_nodes;
	const ai_node* it = std::lower_bound(static_cast<const ai_node*>(m_nodes), end, sample);
	if (it == end || it->packed_xz != sample.packed_xz)
		return AI_MAP_BAD_NODE;

	size_t node_id = it - m_nodes;
	float y0 = vertex_plane_y(*it, position.x, position.z);
	while (++it != end && it->packed_xz == sample.packed_xz) {
		float y = vertex_plane_y(*it, position.x, position.z);
		if (position.y < y0) {
			if (position.y < y && (y0 - position.y) <= (y - position.y))
				continue;
		} else if (position.y < y || (position.y - y0) <= (position.y - y)) {
			continue;
		}
		node_id = it - m_nodes;
		y0 = y;
	}
	return uint32_t(node_id & UINT32_MAX);
}
