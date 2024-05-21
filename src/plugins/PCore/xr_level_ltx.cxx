#include "xr_ini_file.h"
#include "xr_level_ltx.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_level_ltx::~xr_level_ltx()
{
	delete m_ini;
}

void xr_level_ltx::load(xr_reader& r)
{
	const char* p = r.pointer<const char>();
	m_data.assign(p, p + r.size());

	m_ini = new xr_ini_file;
	m_ini->load(r);
}

void xr_level_ltx::save(xr_writer& w) const
{
}
