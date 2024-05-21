#include "xr_image.h"
#include "xr_file_system.h"

using namespace xray_re;

void xr_image::save_bmp(xr_writer& w) const
{
	unsigned row_length = (m_width*3 + 3) & ~3;
	unsigned pad = row_length - m_width*3;

	w.w_u16(0x4d42);
	w.w_u32(m_height*row_length);
	w.w_u16(0);
	w.w_u16(0);
	w.w_u32(0x36);

	// bmp v5
	w.w_u32(0x28);
	w.w_u32(m_width);
	w.w_u32(m_height);
	w.w_u16(1);
	w.w_u16(24);
	w.w_u32(0);
	w.w_u32(0);
	w.w_u32(0xb12);
	w.w_u32(0xb12);
	w.w_u32(0);
	w.w_u32(0);

	unsigned column = m_width;
	for (const rgba32* p = m_data + m_width*(m_height - 1); p >= m_data; ++p) {
		rgba32 rgba = *p;
		w.w_u8(uint8_t(rgba & UINT8_MAX));
		w.w_u8(uint8_t((rgba >> 8) & UINT8_MAX));
		w.w_u8(uint8_t((rgba >> 16) & UINT8_MAX));
		if (--column == 0) {
			column = m_width;
			p -= 2*m_width;
			for (size_t k = pad; k != 0; --k)
				w.w_u8(0);
		}
	}
}

bool xr_image::save_bmp(const std::string& path) const
{
	xr_memory_writer* w = new xr_memory_writer();
	save_bmp(*w);
	bool status = w->save_to(path);
	delete w;
	return status;
}

bool xr_image::save_bmp(const char* path, const char* name) const
{
	xr_memory_writer* w = new xr_memory_writer();
	save_bmp(*w);
	bool status = w->save_to(path, name);
	delete w;
	return status;
}
