#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_PORTALS_H__
#define __XR_SCENE_PORTALS_H__

#include "xr_scene_objects.h"

namespace xray_re {

// CPortal
const uint16_t PORTAL_VERSION = 0x10;

enum {
	PORTAL_CHUNK_VERSION		= 0xfa10,
	PORTAL_CHUNK_SECTOR_FRONT	= 0xfa30,
	PORTAL_CHUNK_SECTOR_BACK	= 0xfa40,
	PORTAL_CHUNK_VERTICES		= 0xfa50,
};

class xr_portal_object: public xr_custom_object {
public:
			xr_portal_object(xr_scene& scene);
	virtual		~xr_portal_object();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	std::string&		sector_front();
	std::string&		sector_back();
	std::vector<fvector3>&	vertices();

private:
	std::string		m_sector_front;
	std::string		m_sector_back;
	std::vector<fvector3>	m_vertices;
};

inline std::string& xr_portal_object::sector_front() { return m_sector_front; }
inline std::string& xr_portal_object::sector_back() { return m_sector_back; }
inline std::vector<fvector3>& xr_portal_object::vertices() { return m_vertices; }


// EScenePortalTools
enum {
	PORTALS_CHUNK_COMMON_FLAGS	= 0x1002,
};

// common flags
enum {
	PORTALS_FLAG_DRAW_SIMPLE_MODEL	= 0x80000000,
};

class xr_scene_portals: public xr_scene_objects {
public:
			xr_scene_portals(xr_scene& scene);
	virtual		~xr_scene_portals();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&	flags();

private:
	uint32_t	m_flags;	// PORTALS_CHUNK_COMMON_FLAGS
};

inline uint32_t& xr_scene_portals::flags() { return m_flags; }

} // end of namespace xray_re

#endif
