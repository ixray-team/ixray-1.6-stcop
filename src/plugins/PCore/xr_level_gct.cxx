#include "xr_ai_version.h"
#include "xr_ai_cross_table.h"
#include "xr_level_gct.h"
#include "xr_file_system.h"
#include <memory>

using namespace xray_re;

xr_level_gct::xr_level_gct(): m_version(AI_VERSION_8),
	m_num_nodes(0), m_num_graph_points(0), m_cells(0)
{
	m_level_guid.reset();
	m_game_guid.reset();
}

xr_level_gct::xr_level_gct(const xr_level_gct& that)
{
	m_version = that.m_version;
	m_num_graph_points = that.m_num_graph_points;
	m_level_guid = that.m_level_guid;
	m_game_guid = that.m_game_guid;
	if (that.m_cells) {
		m_cells = new gct_cell[m_num_nodes = that.m_num_nodes];
#if defined(_MSC_VER) && _MSC_VER >= 1400 && _MSC_VER < 1600
		stdext::unchecked_uninitialized_copy(that.m_cells, that.m_cells + that.m_num_nodes, m_cells);
#else
		std::uninitialized_copy(that.m_cells, that.m_cells + that.m_num_nodes, m_cells);
#endif
	} else {
		m_num_nodes = 0;
		m_cells = 0;
	}
}

xr_level_gct::~xr_level_gct()
{
	delete[] m_cells;
}

void xr_level_gct::load(xr_reader& r)
{
	if (!r.find_chunk(GCT_CHUNK_HEADER))
		xr_not_expected();
	m_version = r.r_u32();
	// v9 is always embedded into game graph and handled there
	xr_assert(m_version == AI_VERSION_8);
	m_num_nodes = r.r_u32();
	m_num_graph_points = r.r_u32();
	m_level_guid.load(r);
	m_game_guid.load(r);
	r.debug_find_chunk();

	if (!r.find_chunk(GCT_CHUNK_CELLS))
		xr_not_expected();
	r.r_cseq(m_num_nodes, m_cells = new gct_cell[m_num_nodes], gct_cell_io());
	r.debug_find_chunk();
}

void xr_level_gct::save(xr_writer& w) const
{
	w.open_chunk(GCT_CHUNK_HEADER);
	w.w_u32(m_version);
	w.w_u32(m_num_nodes);
	w.w_u32(m_num_graph_points);
	m_level_guid.save(w);
	m_game_guid.save(w);
	w.close_chunk();

	w.open_chunk(GCT_CHUNK_CELLS);
	w.w_cseq(m_num_nodes, m_cells, gct_cell_io());
	w.close_chunk();
}

bool xr_level_gct::load(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path, name);
	if (r == 0)
		return false;
	load(*r);
	fs.r_close(r);
	return true;
}

bool xr_level_gct::save(const char* path, const char* name) const
{
	xr_file_system& fs = xr_file_system::instance();
	xr_writer* w = fs.w_open(path, name);
	if (w == 0)
		return false;
	save(*w);
	fs.w_close(w);
	return true;
}
