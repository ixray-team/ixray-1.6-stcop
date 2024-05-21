#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_IMAGE_H__
#define __XR_IMAGE_H__

#include <string>
#include "xr_types.h"
#include "xr_color.h"
#include "xr_rect.h"

namespace xray_re {

class xr_reader;
class xr_writer;

class xr_image {
public:
			xr_image();
			xr_image(unsigned width, unsigned height);
	virtual		~xr_image();

	bool		load_dds(const char* path, const char* name);
	bool		load_dds(const std::string& path);
	bool		save_dds(xr_writer& w, const irect* rect) const;
	bool		save_dds(const char* path, const std::string& name, const irect* rect = 0) const;

	void		save_tga(xr_writer& w) const;
	bool		save_tga(const char* path, const char* name) const;
	bool		save_tga(const std::string& path) const;

	void		save_bmp(xr_writer& w) const;
	bool		save_bmp(const char* path, const char* name) const;
	bool		save_bmp(const std::string& path) const;

	rgba32&		pixel(unsigned x, unsigned y);
	const rgba32&	pixel(unsigned x, unsigned y) const;

	unsigned	width() const;
	unsigned	height() const;

private:
	unsigned	m_width;
	unsigned	m_height;
	rgba32*		m_data;
};

inline xr_image::xr_image(): m_width(0), m_height(0), m_data(0) {}
inline unsigned xr_image::width() const { return m_width; }
inline unsigned xr_image::height() const { return m_height; }

inline rgba32& xr_image::pixel(unsigned x, unsigned y) { return m_data[m_width*y + x]; }
inline const rgba32& xr_image::pixel(unsigned x, unsigned y) const { return m_data[m_width*y + x]; }

} // end of namespace xray_re

#endif
