#include "xr_ai_graph.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

void gg_level2215_io::operator()(gg_level& level, xr_reader& r) const
{
	r.r_sz(level.name);
	r.r_fvector3(level.offset);
	level.level_id = r.r_u32();
	r.r_sz(level.section);
	level.guid.load(r);
}

void gg_level2215_io::operator()(const gg_level& level, xr_writer& w) const
{
	w.w_sz(level.name);
	w.w_fvector3(level.offset);
	w.w_u32(level.level_id);
	w.w_sz(level.section);
	level.guid.save(w);
}

void gg_vertex2215_io::operator()(gg_vertex& vert, xr_reader& r) const
{
	r.r_fvector3(vert.local_point);
	r.r_fvector3(vert.global_point);
	vert.level_id = r.r_u8();
	vert.node_id = r.r_u24();
	r.r_cseq(4, vert.vertex_types);
	vert.neighbour_count = r.r_u8();
	unsigned edge_offset = r.r_u24();
	vert.edge_index = (edge_offset - edge_base)/sizeof(gg_edge_2215);
	vert.death_point_count = r.r_u8();
	unsigned point_offset = r.r_u24();
	if (vert.death_point_count != 0)
		vert.point_index = (point_offset - death_point_base)/sizeof(gg_level_point);
}

void gg_vertex2215_io::operator()(const gg_vertex& vert, xr_writer& w) const {
	w.w_fvector3(vert.local_point);
	w.w_fvector3(vert.global_point);
	w.w_u8(vert.level_id);
	w.w_u24(vert.node_id);
	w.w_cseq(4, vert.vertex_types);
	w.w_u8(vert.neighbour_count);
	w.w_u24(edge_base + vert.edge_index*sizeof(gg_edge_2215));
	w.w_u8(vert.death_point_count);
	if (vert.death_point_count == 0)
		w.w_u24(0);
	else
		w.w_u24(death_point_base + vert.point_index*sizeof(gg_level_point));
}

void gg_edge2215_io::operator()(gg_edge& edge, xr_reader& r) const
{
	edge.vertex_id = r.r_u32();
	edge.distance = r.r_float();
}

void gg_edge2215_io::operator()(const gg_edge& edge, xr_writer& w) const
{
	w.w_u32(edge.vertex_id);
	w.w_float(edge.distance);
}

void gg_level_io::operator()(gg_level& level, xr_reader& r) const
{
	r.r_sz(level.name);
	r.r_fvector3(level.offset);
	level.level_id = r.r_u8();
	r.r_sz(level.section);
	level.guid.load(r);
}

void gg_level_io::operator()(const gg_level& level, xr_writer& w) const
{
	w.w_sz(level.name);
	w.w_fvector3(level.offset);
	w.w_u8(uint8_t(level.level_id));
	w.w_sz(level.section);
	level.guid.save(w);
}

void gg_vertex_io::operator()(gg_vertex& vert, xr_reader& r) const
{
	r.r_fvector3(vert.local_point);
	r.r_fvector3(vert.global_point);
	vert.level_id = r.r_u8();
	vert.node_id = r.r_u24();
	r.r_cseq(4, vert.vertex_types);
	unsigned edge_offset = r.r_u32();
	unsigned point_offset = r.r_u32();
	vert.neighbour_count = r.r_u8();
	vert.death_point_count = r.r_u8();
	xr_assert((edge_offset - edge_base)%sizeof(gg_edge_2947) == 0);
	vert.edge_index = (edge_offset - edge_base)/sizeof(gg_edge_2947);
	if (vert.death_point_count != 0) {
		xr_assert((point_offset - death_point_base)%sizeof(gg_level_point) == 0);
		vert.point_index = (point_offset - death_point_base)/sizeof(gg_level_point);
	}
}

void gg_vertex_io::operator()(const gg_vertex& vert, xr_writer& w) const
{
	w.w_fvector3(vert.local_point);
	w.w_fvector3(vert.global_point);
	w.w_u8(vert.level_id);
	w.w_u24(vert.node_id);
	w.w_cseq(4, vert.vertex_types);
	w.w_u32(edge_base + vert.edge_index*sizeof(gg_edge_2947));
	if (vert.death_point_count == 0)
		w.w_u32(0);
	else
		w.w_u32(death_point_base + vert.point_index*sizeof(gg_level_point));
	w.w_u8(vert.neighbour_count);
	w.w_u8(vert.death_point_count);
}

void gg_edge_io::operator()(gg_edge& edge, xr_reader& r) const
{
	edge.vertex_id = r.r_u16();
	edge.distance = r.r_float();
}

void gg_edge_io::operator()(const gg_edge& edge, xr_writer& w) const
{
	w.w_u16(uint16_t(edge.vertex_id));
	w.w_float(edge.distance);
}

void gg_level_point_io::operator()(gg_level_point& point, xr_reader& r) const
{
	r.r_fvector3(point.point);
	point.node_id = r.r_u32();
	point.distance = r.r_float();
}

void gg_level_point_io::operator()(const gg_level_point& point, xr_writer& w) const
{
	w.w_fvector3(point.point);
	w.w_u32(point.node_id);
	w.w_float(point.distance);
}
