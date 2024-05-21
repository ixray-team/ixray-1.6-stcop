#include "xr_ai_version.h"
#include "xr_level_graph.h"
#include "xr_file_system.h"

using namespace xray_re;

xr_level_graph::xr_level_graph(): m_version(AI_VERSION_8),
	m_levels(0), m_vertices(0), m_edges(0), m_death_points(0)
{
	m_guid.reset();
}

xr_level_graph::~xr_level_graph()
{
	delete[] m_levels;
	delete[] m_vertices;
	delete[] m_edges;
	delete[] m_death_points;
}

void xr_level_graph::load(xr_reader& r)
{
	m_version = r.r_u8();
	m_num_vertices = r.r_u16();
	if (m_num_vertices == 0) {
		// looks like an older 2215-style v8 format, so
		// make sure the highest byte is zero.
		unsigned high = r.r_u8();
		xr_assert(m_version == AI_VERSION_8 && high == 0);
		m_version = AI_VERSION_2215;

		m_num_levels = r.r_u32();
		m_num_vertices = r.r_u32();
		m_num_edges = r.r_u32();
		m_num_death_points = r.r_u32();
		m_guid.load(r);

		m_levels = new gg_level[m_num_levels];
		r.r_cseq(m_num_levels, m_levels, gg_level2215_io());
		unsigned edge_base = m_num_vertices * sizeof(gg_vertex_2215);
		unsigned death_point_base = edge_base + m_num_edges * sizeof(gg_edge_2215);
		m_vertices = new gg_vertex[m_num_vertices];
		r.r_cseq(m_num_vertices, m_vertices, gg_vertex2215_io(edge_base, death_point_base));
		m_edges = new gg_edge[m_num_edges];
		r.r_cseq(m_num_edges, m_edges, gg_edge2215_io());
	} else {
		// either modern 2947-style v8 or CS.
		xr_assert(m_version >= AI_VERSION_8 && m_version <= AI_VERSION_10);
		m_num_edges = r.r_u32();
		m_num_death_points = r.r_u32();
		m_guid.load(r);
		m_num_levels = r.r_u8();

		m_levels = new gg_level[m_num_levels];
		r.r_cseq(m_num_levels, m_levels, gg_level_io());
		unsigned edge_base = m_num_vertices*sizeof(gg_vertex_2947);
		unsigned death_point_base = edge_base + m_num_edges*sizeof(gg_edge_2947);
		m_vertices = new gg_vertex[m_num_vertices];
		r.r_cseq(m_num_vertices, m_vertices, gg_vertex_io(edge_base, death_point_base));
		m_edges = new gg_edge[m_num_edges];
		r.r_cseq(m_num_edges, m_edges, gg_edge_io());
	}
	if (m_num_death_points) {
		m_death_points = new gg_level_point[m_num_death_points];
		r.r_cseq(m_num_death_points, m_death_points, gg_level_point_io());
	}
}

void xr_level_graph::save(xr_writer& w) const
{
	if (m_version == AI_VERSION_2215) {
		w.w_u32(AI_VERSION_8);
		w.w_u32(m_num_levels);
		w.w_u32(m_num_vertices);
		w.w_u32(m_num_edges);
		w.w_u32(m_num_death_points);
		m_guid.save(w);

		w.w_cseq(m_num_levels, m_levels, gg_level2215_io());
		unsigned edge_base = m_num_vertices*sizeof(gg_vertex_2215);
		unsigned death_point_base = m_num_edges*sizeof(gg_edge_2215) + edge_base;
		w.w_cseq(m_num_vertices, m_vertices, gg_vertex2215_io(edge_base, death_point_base));
		w.w_cseq(m_num_edges, m_edges, gg_edge2215_io());
	} else {
		w.w_u8(m_version);
		w.w_size_u16(m_num_vertices);
		w.w_u32(m_num_edges);
		w.w_u32(m_num_death_points);
		m_guid.save(w);
		w.w_size_u8(m_num_levels);

		w.w_cseq(m_num_levels, m_levels, gg_level_io());
		unsigned edge_base = m_num_vertices*sizeof(gg_vertex_2947);
		unsigned death_point_base = m_num_edges*sizeof(gg_edge_2947) + edge_base;
		w.w_cseq(m_num_vertices, m_vertices, gg_vertex_io(edge_base, death_point_base));
		w.w_cseq(m_num_edges, m_edges, gg_edge_io());
	}
	if (m_num_death_points)
		w.w_cseq(m_num_death_points, m_death_points, gg_level_point_io());
}

bool xr_level_graph::load(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	load(*r);
	fs.r_close(r);
	return true;
}

bool xr_level_graph::save(const char* path, const char* name) const
{
	xr_file_system& fs = xr_file_system::instance();
	xr_writer* w = fs.w_open(path, name);
	if (w == 0)
		return false;
	save(*w);
	fs.w_close(w);
	return true;
}
