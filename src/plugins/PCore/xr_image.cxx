#include "xr_image.h"

using namespace xray_re;

xr_image::xr_image(unsigned width, unsigned height):
	m_width(width), m_height(height)
{
	m_data = new rgba32[width*height];
}

xr_image::~xr_image() { delete[] m_data; }
