#include "xr_image.h"
#include "xr_file_system.h"

using namespace xray_re;

void xr_image::save_tga(xr_writer& w) const
{
	// tga header
	w.w_u8(0);		// ID Length
	w.w_u8(0);		// Color Map Type (none)
	w.w_u8(2);		// Image Type (RGBA)
	w.w_u16(0);
	w.w_u16(0);
	w.w_u8(0);
	w.w_u16(0);		// x
	w.w_u16(0);		// y
	w.w_size_u16(m_width);
	w.w_size_u16(m_height);
	w.w_u8(32);
	w.w_u8(0x2f);
	
	w.w_raw(m_data, m_width*m_height * sizeof(rgba32));

	char footer[] = "\0\0\0\0\0\0\0\0TRUEVISION-XFILE.";
	w.w_raw(footer, sizeof(footer));
}

bool xr_image::save_tga(const char* path, const char* name) const
{
	xr_memory_writer* w = new xr_memory_writer();
	save_tga(*w);
	bool status = w->save_to(path, name);
	delete w;
	return status;
}

bool xr_image::save_tga(const std::string& path) const
{
	xr_memory_writer* w = new xr_memory_writer();
	save_tga(*w);
	bool status = w->save_to(path);
	delete w;
	return status;
}
