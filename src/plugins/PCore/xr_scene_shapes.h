#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_SHAPES_H__
#define __XR_SCENE_SHAPES_H__

#include "xr_scene_objects.h"
#include "xr_shape.h"

namespace xray_re {

// shape_object (CEditShape)
const uint16_t SHAPE_VERSION = 1;
const uint16_t SHAPE_VERSION_V12 = 2;

enum {
	SHAPE_CHUNK_VERSION	= 0x0000,
	SHAPE_CHUNK_SHAPES	= 0x0001,
};

class xr_shape_object: public xr_custom_object {
public:
			xr_shape_object(xr_scene& scene);
	virtual		~xr_shape_object();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	shape_def_vec&	shapes();

protected:
			xr_shape_object(xr_scene& scene, tools_class_id class_id);

private:
	shape_def_vec	m_shapes;
};

inline shape_def_vec& xr_shape_object::shapes() { return m_shapes; }


// ESceneShapeTools
enum {
	SHAPES_CHUNK_COMMON_FLAGS	= 0x1002,
};

class xr_scene_shapes: public xr_scene_objects {
public:
			xr_scene_shapes(xr_scene& scene);
	virtual		~xr_scene_shapes();
	virtual void	load(xr_reader& r);
	virtual void	save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	uint32_t&	flags();

private:
	uint32_t	m_flags;	// SHAPES_CHUNK_COMMON_FLAGS
};

inline uint32_t& xr_scene_shapes::flags() { return m_flags; }

} // end of namespace xray_re

#endif
