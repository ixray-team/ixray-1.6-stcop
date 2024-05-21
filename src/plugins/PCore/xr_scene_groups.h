#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_GROUPS_H__
#define __XR_SCENE_GROUPS_H__

#include "xr_scene_objects.h"

namespace xray_re {

// CGroupObject
const uint16_t GROUPOBJ_VERSION = 0x11;
const uint16_t GROUPOBJ_VERSION_V12 = 0x12;

enum {
	GROUPOBJ_CHUNK_VERSION		= 0x0000,
	GROUPOBJ_CHUNK_OBJECTS		= 0x0001,
	GROUPOBJ_CHUNK_FLAGS		= 0x0003,
	GROUPOBJ_CHUNK_REFERENCE	= 0x0004,
	GROUPOBJ_CHUNK_OPEN_OBJECT_LIST	= 0x0005,
};

// from GROUPOBJ_CHUNK_FLAGS
enum {
	GOF_OPEN	= 0x1,
};

class xr_group_object: public xr_custom_object {
public:
			xr_group_object(xr_scene& scene);
	virtual		~xr_group_object();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&			flags();
	std::string&			reference();
	xr_custom_object_vec&		objects();

private:
	uint32_t			m_flags;	// GROUPOBJ_CHUNK_FLAGS
	std::string			m_reference;	// GROUPOBJ_CHUNK_REFERENCE
	xr_custom_object_vec		m_objects;	// GROUPOBJ_CHUNK_OBJECTS
	std::vector<std::string>	m_open_objects;	// GROUPOBJ_CHUNK_OPEN_OBJECT_LIST
};

inline uint32_t& xr_group_object::flags() { return m_flags; }
inline std::string& xr_group_object::reference() { return m_reference; }
inline xr_custom_object_vec& xr_group_object::objects() { return m_objects; }

// ESceneGroupTools
class xr_scene_groups: public xr_scene_objects {
public:
				xr_scene_groups(xr_scene& scene);
	virtual			~xr_scene_groups();
	virtual void		load(xr_reader& r);
	virtual void		save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;
};

} // end of namespace xray_re

#endif
