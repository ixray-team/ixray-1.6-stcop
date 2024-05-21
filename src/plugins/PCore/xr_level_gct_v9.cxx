// CS-style GCTs (v9, v10)
#include "xr_ai_version.h"
#include "xr_level_gct.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

void xr_level_gct::load_v9(xr_reader& r)
{
	size_t size = r.r_u32();
	xr_assert(size >= sizeof(uint32_t) + sizeof(gct_header_v8));
	m_version = r.r_u32();
	xr_assert(m_version == AI_VERSION_9 || m_version == AI_VERSION_10);
	m_num_nodes = r.r_u32();
	m_num_graph_points = r.r_u32();
	m_level_guid.load(r);
	m_game_guid.load(r);
	r.r_cseq(m_num_nodes, m_cells = new gct_cell[m_num_nodes], gct_cell_io());
}

void xr_level_gct::save_v9(xr_writer& w) const
{
	w.w_size_u32(sizeof(uint32_t) + sizeof(gct_header_v8) + m_num_nodes*sizeof(gct_cell_v8));
	w.w_u32(m_version);
	w.w_u32(m_num_nodes);
	w.w_u32(m_num_graph_points);
	m_level_guid.save(w);
	m_game_guid.save(w);
	w.w_cseq(m_num_nodes, m_cells, gct_cell_io());
}
