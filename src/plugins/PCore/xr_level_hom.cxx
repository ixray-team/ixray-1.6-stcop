#include "xr_level_hom.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_level_hom::~xr_level_hom()
{
	delete[] m_polys;
}

void xr_level_hom::load(xr_reader& r)
{
	uint32_t version;
	if (!r.r_chunk(HOM_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == HOM_VERSION);
	size_t size = r.find_chunk(HOM_CHUNK_POLYGONS);
	xr_assert(size && (size % sizeof(hom_poly) == 0));
	m_polys = new hom_poly[size /= sizeof(hom_poly)];
	r.r_cseq(size, m_polys);
	m_num_polys = uint32_t(size & UINT32_MAX);
}

void xr_level_hom::save(xr_writer& w) const
{
	w.w_chunk<uint32_t>(HOM_CHUNK_VERSION, HOM_VERSION);
	w.open_chunk(HOM_CHUNK_POLYGONS);
	w.w_cseq(m_num_polys, m_polys);
	w.close_chunk();
}
