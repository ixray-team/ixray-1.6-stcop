#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_OBJECTS_H__
#define __XR_SCENE_OBJECTS_H__

#include <string>
#include "xr_scene_part.h"
#include "xr_vector3.h"

namespace xray_re {

// CCustomObject

// common chunk ids
enum {
	CUSTOMOBJECT_CHUNK_PARAMS	= 0xf900,
	CUSTOMOBJECT_CHUNK_f902		= 0xf902,
	CUSTOMOBJECT_CHUNK_XFORM	= 0xf903,
	CUSTOMOBJECT_CHUNK_MOTION	= 0xf905,
	CUSTOMOBJECT_CHUNK_FLAGS	= 0xf906,
	CUSTOMOBJECT_CHUNK_NAME		= 0xf907,
	CUSTOMOBJECT_CHUNK_TIME		= 0xf908,
};

// flag values for CUSTOMOBJECT_CHUNK_FLAGS
enum {
	COF_SELECTED	= 0x1,
	COF_VISIBLE	= 0x2,
	COF_LOCKED	= 0x4,
	COF_MOTIONABLE	= 0x8,		// has motions
};

class xr_obj_motion;

class xr_custom_object {
public:
				xr_custom_object(xr_scene& scene);
	virtual			~xr_custom_object();
	virtual void		load(xr_reader& r);
	virtual void		save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	tools_class_id		class_id() const;
	uint32_t&		co_flags();
	uint32_t		co_flags() const;
	std::string&		co_name();
	const std::string&	co_name() const;
	fvector3&		co_position();
	const fvector3&		co_position() const;
	fvector3&		co_rotation();
	const fvector3&		co_rotation() const;
	fvector3&		co_scale();
	const fvector3&		co_scale() const;

protected:
				xr_custom_object(xr_scene& scene, tools_class_id class_id = TOOLS_CLASS_DEFAULT);
	xr_scene&		scene() const;

private:
	xr_scene&		m_scene;
	tools_class_id		m_class_id;

	uint32_t		m_flags;	// CUSTOMOBJECT_CHUNK_FLAGS
	std::string		m_name;		// CUSTOMOBJECT_CHUNK_NAME
	fvector3		m_position;	// CUSTOMOBJECT_CHUNK_XFORM
	fvector3		m_rotation;
	fvector3		m_scale;
	xr_obj_motion*		m_motion;	// CUSTOMOBJECT_CHUNK_MOTION
	float			m_time;		// CUSTOMOBJECT_CHUNK_TIME
};

TYPEDEF_STD_VECTOR_PTR(xr_custom_object)

inline xr_scene& xr_custom_object::scene() const { return m_scene; }
inline tools_class_id xr_custom_object::class_id() const { return m_class_id; }
inline uint32_t& xr_custom_object::co_flags() { return m_flags; }
inline uint32_t xr_custom_object::co_flags() const { return m_flags; }
inline std::string& xr_custom_object::co_name() { return m_name; }
inline const std::string& xr_custom_object::co_name() const { return m_name; }
inline fvector3& xr_custom_object::co_position() { return m_position; }
inline const fvector3& xr_custom_object::co_position() const { return m_position; }
inline fvector3& xr_custom_object::co_rotation() { return m_rotation; }
inline const fvector3& xr_custom_object::co_rotation() const { return m_rotation; }
inline fvector3& xr_custom_object::co_scale() { return m_scale; }
inline const fvector3& xr_custom_object::co_scale() const { return m_scale; }

class xr_scene_objects: public xr_scene_part {
public:
	virtual			~xr_scene_objects();
	virtual void		load(xr_reader& r);
	virtual void		save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	const xr_custom_object_vec&	objects() const;
	xr_custom_object_vec&		objects();

protected:
				xr_scene_objects(xr_scene& scene, const char* file_name, scene_chunk_id chunk_id);

private:
	xr_custom_object_vec	m_objects;
};

inline const xr_custom_object_vec& xr_scene_objects::objects() const { return m_objects; }
inline xr_custom_object_vec& xr_scene_objects::objects() { return m_objects; }

} // end of namespace xray_re

#endif
