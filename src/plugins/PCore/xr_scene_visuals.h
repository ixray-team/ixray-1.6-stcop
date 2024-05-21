#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_VISUALS_H__
#define __XR_SCENE_VISUALS_H__

#include "xr_scene_objects.h"

namespace xray_re {

// CSceneObject
const uint16_t SCENEOBJ_VERSION_16 = 0x10;
const uint16_t SCENEOBJ_VERSION_17 = 0x11;
const uint16_t SCENEOBJ_VERSION_18 = 0x12;
const uint16_t SCENEOBJ_VERSION = SCENEOBJ_VERSION_17;

enum {
	SCENEOBJ_CHUNK_VERSION		= 0x0900,
	SCENEOBJ_CHUNK_REFERENCE	= 0x0902,
	SCENEOBJ_CHUNK_PLACEMENT	= 0x0904,
	SCENEOBJ_CHUNK_FLAGS		= 0x0905,
};

class xr_visual_object: public xr_custom_object {
public:
			xr_visual_object(xr_scene& scene);
	virtual		~xr_visual_object();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&	file_version();
	std::string&	reference();
	uint32_t&	flags();

private:
	uint32_t	m_file_version;		// SCENEOBJ_CHUNK_REFERENCE
	std::string	m_reference;
	uint32_t	m_flags;		// SCENEOBJ_CHUNK_FLAGS
};

inline uint32_t& xr_visual_object::file_version() { return m_file_version; }
inline std::string& xr_visual_object::reference() { return m_reference; }
inline uint32_t& xr_visual_object::flags() { return m_flags; }

// ESceneObjectTools
enum {
	SCENEOBJS_FLAG_PROPORTIONAL	= 0x10000000,
	SCENEOBJS_FLAG_SCALE		= 0x40000000,
	SCENEOBJS_FLAG_ROTATE		= 0x80000000,
};

enum {
	SCENEOBJS_CHUNK_COMMON_PARAMS	= 0x1002,
	SCENEOBJS_CHUNK_COMMON_FLAGS	= 0x1003,
};

class xr_scene_visuals: public xr_scene_objects {
public:
				xr_scene_visuals(xr_scene& scene);
	virtual			~xr_scene_visuals();
	virtual void		load(xr_reader& r);
	virtual void		save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&		flags();
	uint32_t		flags() const;
	fvector3&		min_scale();
	const fvector3&		min_scale() const;
	fvector3&		max_scale();
	const fvector3&		max_scale() const;
	fvector3&		min_rotate();
	const fvector3&		min_rotate() const;
	fvector3&		max_rotate();
	const fvector3&		max_rotate() const;

private:
	uint32_t		m_flags;	// SCENEOBJS_CHUNK_COMMON_FLAGS

	fvector3		m_min_scale;	// SCENEOBJS_CHUNK_COMMON_PARAMS
	fvector3		m_max_scale;
	fvector3		m_min_rotate;
	fvector3		m_max_rotate;
	std::vector<std::string>m_snap_objects;
};

inline uint32_t& xr_scene_visuals::flags() { return m_flags; }
inline uint32_t xr_scene_visuals::flags() const { return m_flags; }
inline fvector3& xr_scene_visuals::min_scale() { return m_min_scale; }
inline const fvector3& xr_scene_visuals::min_scale() const { return m_min_scale; }
inline fvector3& xr_scene_visuals::max_scale() { return m_max_scale; }
inline const fvector3& xr_scene_visuals::max_scale() const { return m_max_scale; }
inline fvector3& xr_scene_visuals::min_rotate() { return m_min_rotate; }
inline const fvector3& xr_scene_visuals::min_rotate() const { return m_min_rotate; }
inline fvector3& xr_scene_visuals::max_rotate() { return m_max_rotate; }
inline const fvector3& xr_scene_visuals::max_rotate() const { return m_max_rotate; }

} // end of namespace xray_re

#endif
