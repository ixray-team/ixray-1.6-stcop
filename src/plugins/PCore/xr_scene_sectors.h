#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_SECTORS_H__
#define __XR_SCENE_SECTORS_H__

#include "xr_scene_objects.h"
#include "xr_color.h"

namespace xray_re {

// CSector
const uint16_t SECTOR_VERSION = 0x11;
const uint16_t SECTOR_VERSION_V12 = 0x12;

enum {
	SECTOR_CHUNK_VERSION	= 0xf010,
	SECTOR_CHUNK_COLOR	= 0xf020,
	SECTOR_CHUNK_PRIVATE	= 0xf025,
	SECTOR_CHUNK_ITEMS	= 0xf030,
	SECTOR_CHUNK_ONE_ITEM	= 0xf031,
};

struct sector_item {
	std::string	object;
	std::string	mesh;
};
TYPEDEF_STD_VECTOR(sector_item)

class xr_sector_object: public xr_custom_object {
public:
				xr_sector_object(xr_scene& scene);
	virtual			~xr_sector_object();
	virtual void		load(xr_reader& r);
	virtual void		save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	fcolor&			color();
	uint8_t&		priv();
	sector_item_vec&	items();

private:
	fcolor			m_color;
	uint8_t			m_private;
	sector_item_vec		m_items;
};

inline fcolor& xr_sector_object::color() { return m_color; }
inline uint8_t& xr_sector_object::priv() { return m_private; }
inline sector_item_vec& xr_sector_object::items() { return m_items; }

// ESceneSectorTools
enum {
	SECTORS_FLAG_DRAW_SOLID	= 0x80000000,
};

enum {
	SECTORS_CHUNK_COMMON_FLAGS	= 0x1002,
};

class xr_scene_sectors: public xr_scene_objects {
public:
			xr_scene_sectors(xr_scene& scene);
	virtual		~xr_scene_sectors();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&	flags();

private:
	uint32_t	m_flags;	// SECTORS_CHUNK_COMMON_FLAGS
};

inline uint32_t& xr_scene_sectors::flags() { return m_flags; }

} // end of namespace xray_re

#endif
