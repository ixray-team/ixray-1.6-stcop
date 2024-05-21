#if 0
#define NOMINMAX
#include <climits>
#pragma warning(push)
#pragma warning(disable: 4595)
#include <nvimage/Image.h>
#include <nvimage/DirectDrawSurface.h>
#include <nvmath/Color.h>
#include <nvtt/nvtt.h>
#pragma warning(pop)
#include "xr_image.h"
#include "xr_file_system.h"

using namespace xray_re;

bool xr_image::load_dds(const std::string& path)
{
	nv::DirectDrawSurface dds(path.c_str());
	if (!dds.isValid() || !dds.isTexture2D())
		return false;
	nv::Image image;
	dds.mipmap(&image, 0, 0);
	m_width = image.width();
	m_height = image.height();
	m_data = new rgba32[m_width*m_height];
	for (unsigned i = m_height*m_width; i > 0;) {
		const nv::Color32& pix = image.pixel(--i);
		m_data[i] = pix.u;
	}
	return true;
}

bool xr_image::load_dds(const char* path, const char* name)
{
	xr_file_system& fs = xr_file_system::instance();
	std::string full_path;
	if (!fs.resolve_path(path, name, full_path))
		return false;
	return load_dds(full_path);
}

bool xr_image::save_dds(const char* path, const std::string& name, const irect* rect) const
{
	xr_memory_writer* w = new xr_memory_writer();
	bool status = save_dds(*w, rect) && w->save_to(path, name);
	delete w;
	return status;
}

struct dds_writer: public nvtt::OutputHandler {
			dds_writer(xr_writer& _w);

	virtual void	beginImage(int size, int width, int height, int depth, int face, int miplevel);
	virtual bool	writeData(const void* data, int size);
	xr_writer&	w;
};

inline dds_writer::dds_writer(xr_writer& _w): w(_w) {}

void dds_writer::beginImage(int size, int width, int height, int depth, int face, int miplevel)
{
}

bool dds_writer::writeData(const void* data, int size)
{
	w.w_raw(data, size_t(size & INT_MAX));
	return true;
}

bool xr_image::save_dds(xr_writer& w, const irect* rect) const
{
#if 1
	int width, height;
	rgba32* data;
	if (rect) {
		width = int((rect->x2 - rect->x1 + 1) & INT_MAX);
		height = int((rect->y2 - rect->y1 + 1) & INT_MAX);
		data = new rgba32[width * height];
		const rgba32* src_data = &m_data[rect->y1*m_width + rect->x1];
		for (int i = 0; i != height; ++i) {
			memcpy(&data[i*width], src_data, width*sizeof(rgba32));
			src_data += m_width;
		}
	} else {
		width = m_width;
		height = m_height;
		data = m_data;
	}
#else
	int width = int(m_width & INT_MAX);
	int height = int(m_height & INT_MAX);
	const rgba32* data = m_data;
#endif

	nvtt::InputOptions in_opts;
	in_opts.setTextureLayout(nvtt::TextureType_2D, width, height);
	in_opts.setWrapMode(nvtt::WrapMode_Clamp);
	in_opts.setMipmapData(data, width, height);
	if (data != m_data)
		delete[] data;
	in_opts.setNormalMap(false);
	in_opts.setConvertToNormalMap(false);
	in_opts.setGamma(2.2f, 2.2f);
	in_opts.setNormalizeMipmaps(false);

	nvtt::CompressionOptions comp_opts;
	comp_opts.setFormat(nvtt::Format_BC3);
	comp_opts.setQuality(nvtt::Quality_Highest);

	nvtt::OutputOptions out_opts;
	dds_writer dds(w);
	out_opts.setOutputHandler(&dds);
	out_opts.setErrorHandler(0);

	return nvtt::Compressor().process(in_opts, comp_opts, out_opts);
}
#endif