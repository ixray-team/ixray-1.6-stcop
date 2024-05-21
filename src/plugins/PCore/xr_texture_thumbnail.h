#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_TEXTURE_THUMBNAIL_H__
#define __XR_TEXTURE_THUMBNAIL_H__

#include <string>
#include "xr_thumbnail.h"
#include "xr_color.h"

namespace xray_re {

enum {
	THM_CHUNK_TEXTUREPARAM		= 0x0812,
	THM_CHUNK_TEXTUREPARAM_TYPE	= 0x0814,
	THM_CHUNK_TEXTUREPARAM_DETAIL	= 0x0815,
	THM_CHUNK_TEXTUREPARAM_MATERIAL	= 0x0816,
	THM_CHUNK_TEXTUREPARAM_BUMP	= 0x0817,
	THM_CHUNK_TEXTUREPARAM_NMAP	= 0x0818,
	THM_CHUNK_TEXTUREPARAM_FADE	= 0x0819,
};

const uint16_t THM_VERSION_TEXTUREPARAM = 0x12;

class xr_reader;
class xr_writer;

class xr_texture_thumbnail {
public:
			xr_texture_thumbnail();
	virtual		~xr_texture_thumbnail();

	void		load(xr_reader& r);
	bool		load(const char* path, const char* name);

	bool		has_alpha_channel() const;
	bool		is_implicitly_lighted() const;

	enum et_format {
		tf_dxt1,
		tf_adxt1,
		tf_dxt3,
		tf_dxt5,
		tf_4444,
		tf_1555,
		tf_565,
		tf_rgb,
		tf_rgba,
		tf_nvhs,
		tf_nvhu,
		tf_a8,
		tf_l8,
		tf_a8l8,
		tf_force_u32	= UINT32_MAX,
	};
	enum et_flag {
		fl_generate_mip_maps	= 0x00000001,
		fl_binary_alpha		= 0x00000002,

		fl_implicitly_lighted	= 0x01000000,
		fl_force_u32		= UINT32_MAX,
	};
	enum et_type {
		tt_image	= 0,
		tt_cube_map	= 1,
		tt_bump_map	= 2,
		tt_normal_map = 3,
		tt_terrain = 4,
		tt_force_u32	= UINT32_MAX,
	};
	enum et_material {
		tm_orennayar_blin,
		tm_blin_phong,
		tm_phong_metal,
		tm_metal_orennayar,
		tm_force_u32	= UINT32_MAX,
	};
	enum et_bump_mode {
		tbm_reserved,
		tbm_none,
		tbm_use,
		tbm_force_u32	= UINT32_MAX,
	};
	enum et_mip_filter {
		tmf_box,
		tmf_cubic,
		tmf_point,
		tmf_quadratic,
		tmf_advanced,
		tmf_catrom,
		tmf_mitchell,
		tmf_gaussian,
		tmf_sinc,
		tmf_bessel,
		tmf_hanning,
		tmf_hamming,
		tmf_blackman,
		tmf_kaiser,
	};

	uint8_t*	data;

	et_format	fmt;
	uint32_t	flags;
	rgba32		border_color;
	rgba32		fade_color;
	uint32_t	fade_amount;
	uint8_t		fade_delay;
	uint32_t	mip_filter;
	int32_t		width;
	int32_t		height;

	std::string	detail_name;
	float		detail_scale;

	et_type		type;
	et_material	material;
	float		material_weight;

	float		bump_virtual_height;
	et_bump_mode	bump_mode;
	std::string	bump_name;

	std::string	ext_normal_map_name;
};

inline bool xr_texture_thumbnail::has_alpha_channel() const
{
	return (fmt != tf_dxt1 && fmt <= tf_1555) || fmt == tf_rgba;
}

inline bool xr_texture_thumbnail::is_implicitly_lighted() const
{
	return (flags & xr_texture_thumbnail::fl_implicitly_lighted) != 0;
}

} // end of namespace xray_re

#endif
