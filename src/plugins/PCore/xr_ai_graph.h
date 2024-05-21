#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_AI_GRAPH_H__
#define __XR_AI_GRAPH_H__

#include <string>
#include "xr_guid.h"
#include "xr_vector3.h"

namespace xray_re {

// on-disk structures (not used)
#pragma pack(push, 1)
struct gg_header_2215 {
	uint32_t	version;
	uint32_t	level_count;
	uint32_t	vertex_count;
	uint32_t	edge_count;
	uint32_t	death_point_count;
	xr_guid		guid;
};

struct gg_header_2947 {
	uint8_t		version;
	uint16_t	vertex_count;
	uint32_t	edge_count;
	uint32_t	death_point_count;
	xr_guid		guid;
	uint8_t		level_count;
};

struct gg_vertex_2215 {
	fvector3	local_point;
	fvector3	global_point;
	uint32_t	level_id:8;
	uint32_t	node_id:24;
	uint8_t		vertex_types[4];
	uint32_t	neighbour_count:8;
	uint32_t	edge_offset:24;
	uint32_t	death_point_count:8;
	uint32_t	point_offset:24;
};

struct gg_vertex_2947 {
	fvector3	local_point;
	fvector3	global_point;
	uint32_t	level_id:8;
	uint32_t	node_id:24;
	uint8_t		vertex_types[4];
	uint32_t	edge_offset;
	uint32_t	point_offset;
	uint8_t		neighbour_count;
	uint8_t		death_point_count;
};

struct gg_edge_2215 {
	uint32_t	vertex_id;
	float		distance;
};

struct gg_edge_2947 {
	uint16_t	vertex_id;
	float		distance;
};
#pragma pack(pop)

struct gg_level {
	std::string	name;
	std::string	section;
	fvector3	offset;
	unsigned	level_id;
	xr_guid		guid;
};

struct gg_vertex {
	fvector3	local_point;
	fvector3	global_point;
	uint32_t	node_id;
	uint8_t		vertex_types[4];
	uint32_t	edge_index;
	uint32_t	point_index;
	uint8_t		neighbour_count;
	uint8_t		death_point_count;
	uint8_t		level_id;
	uint8_t		__pad;
};

struct gg_edge {
	unsigned	vertex_id;
	float		distance;
};

struct gg_level_point {
	fvector3	point;
	uint32_t	node_id;
	float		distance;
};

class xr_reader;
class xr_writer;

struct gg_level2215_io {
	void	operator()(gg_level& level, xr_reader& r) const;
	void	operator()(const gg_level& level, xr_writer& w) const;
};

struct gg_level_io {
	void	operator()(gg_level& level, xr_reader& r) const;
	void	operator()(const gg_level& level, xr_writer& w) const;
};

struct gg_vertex2215_io {
			gg_vertex2215_io(unsigned _edge_base, unsigned _death_point_base);
	void		operator()(gg_vertex& vert, xr_reader& r) const;
	void		operator()(const gg_vertex& vert, xr_writer& r) const;
	unsigned	edge_base;
	unsigned	death_point_base;
};

struct gg_vertex_io: public gg_vertex2215_io {
		gg_vertex_io(unsigned _edge_base, unsigned _death_point_base);
	void	operator()(gg_vertex& vert, xr_reader& r) const;
	void	operator()(const gg_vertex& vert, xr_writer& r) const;
};

struct gg_edge2215_io {
	void	operator()(gg_edge& edge, xr_reader& r) const;
	void	operator()(const gg_edge& edge, xr_writer& w) const;
};

struct gg_edge_io {
	void	operator()(gg_edge& edge, xr_reader& r) const;
	void	operator()(const gg_edge& edge, xr_writer& w) const;
};

struct gg_level_point_io {
	void	operator()(gg_level_point& point, xr_reader& r) const;
	void	operator()(const gg_level_point& point, xr_writer& w) const;
};

inline gg_vertex2215_io::gg_vertex2215_io(unsigned _edge_base, unsigned _death_point_base):
	edge_base(_edge_base), death_point_base(_death_point_base) {}

inline gg_vertex_io::gg_vertex_io(unsigned _edge_base, unsigned _death_point_base):
	gg_vertex2215_io(_edge_base, _death_point_base) {}

} // end of namespace xray_re

#endif
