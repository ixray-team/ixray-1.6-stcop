#include "xr_ai_cross_table.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

void gct_cell_io::operator()(gct_cell& cell, xr_reader& r) const
{
	cell.graph_id = r.r_u16();
	cell.distance = r.r_float();
}

void gct_cell_io::operator()(const gct_cell& cell, xr_writer& w) const
{
	w.w_u16(cell.graph_id);
	w.w_float(cell.distance);
}
