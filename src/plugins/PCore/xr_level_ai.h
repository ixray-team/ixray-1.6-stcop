#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_AI_H__
#define __XR_LEVEL_AI_H__

#include <string>
#include <vector>
#include "xr_aabb.h"
#include "xr_guid.h"

namespace xray_re {

struct ai_node {
	uint8_t		data[12];
	uint16_t	cover;
	uint16_t	low_cover;
	uint16_t	plane;
	uint32_t	packed_xz;
	uint16_t	packed_y;

	uint32_t	link(uint_fast32_t side) const;
	bool		operator<(const ai_node& right) const;
};

inline bool ai_node::operator<(const ai_node& right) const { return packed_xz < right.packed_xz; }

class xr_reader;
class xr_writer;

class xr_level_ai {
public:
			xr_level_ai();
			xr_level_ai(xr_reader& r);
	virtual		~xr_level_ai();

	void		clear();

	void		load(xr_reader& r);
	void		save(xr_writer& w) const;
	bool		load(const char* path, const char* name);
	bool		save(const char* path, const char* name) const;

	uint32_t	vertex_id(const fvector3& position) const;
	void		vertex_position(uint32_t& packed_xz, uint16_t& packed_y, const fvector3& position) const;
	void		unpack_xz(uint32_t packed_xz, float& x, float& z) const;
	void		unpack_y(uint16_t packed_y, float& y) const;
	float		vertex_plane_y(uint32_t node_id, float x, float z) const;
	float		vertex_plane_y(const ai_node& node, float x, float z) const;

	uint32_t&	version();
	uint32_t	version() const;
	uint32_t	num_nodes() const;
	float		size() const;
	float		size_y() const;
	const fbox&	aabb() const;
	const ai_node*	nodes() const;

private:
	uint32_t	m_version;
	uint32_t	m_num_nodes;
	float		m_size;
	float		m_size_y;
	fbox		m_aabb;
	xr_guid		m_guid;
	ai_node*	m_nodes;
	unsigned	m_row_length;
	unsigned	m_column_length;
};

inline xr_level_ai::xr_level_ai(xr_reader& r) { load(r); }

inline float xr_level_ai::vertex_plane_y(uint32_t node_id, float x, float z) const
{
	return vertex_plane_y(m_nodes[node_id], x, z);
}
inline void xr_level_ai::unpack_xz(uint32_t packed_xz, float& x, float& z) const
{
	x = (packed_xz/m_row_length)*m_size + m_aabb.min.x;
	z = (packed_xz%m_row_length)*m_size + m_aabb.min.z;
}

inline void xr_level_ai::unpack_y(uint16_t packed_y, float& y) const
{
	y = packed_y*(1.f/65535.f)*m_size_y + m_aabb.min.y;
}

inline uint32_t& xr_level_ai::version() { return m_version; }
inline uint32_t xr_level_ai::version() const { return m_version; }
inline uint32_t xr_level_ai::num_nodes() const { return m_num_nodes; }
inline float xr_level_ai::size() const { return m_size; }
inline float xr_level_ai::size_y() const { return m_size_y; }
inline const fbox& xr_level_ai::aabb() const { return m_aabb; }
inline const ai_node* xr_level_ai::nodes() const { return m_nodes; }

} // end of namespace xray_re

#endif
