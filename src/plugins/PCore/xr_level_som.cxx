#include "xr_level_som.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_level_som::~xr_level_som()
{
	delete[] m_polys;
}

void xr_level_som::load(xr_reader& r)
{
	uint32_t version;
	if (!r.r_chunk(SOM_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == SOM_VERSION);

	size_t size = r.find_chunk(SOM_CHUNK_POLYGONS);
	xr_assert(size && (size % sizeof(som_poly) == 0));
	m_polys = new som_poly[size /= sizeof(som_poly)];
	r.r_cseq(size, m_polys);
	m_num_polys = uint32_t(size & UINT32_MAX);
}

void xr_level_som::save(xr_writer& w) const
{
	w.w_chunk<uint32_t>(SOM_CHUNK_VERSION, SOM_VERSION);
	w.open_chunk(SOM_CHUNK_POLYGONS);
	w.w_cseq(m_num_polys, m_polys);
	w.close_chunk();
}
