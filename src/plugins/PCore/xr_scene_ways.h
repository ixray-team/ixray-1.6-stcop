#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_SCENE_WAYS_H__
#define __XR_SCENE_WAYS_H__

#include "xr_scene_objects.h"
#include "xr_ai_way.h"

namespace xray_re {

// CWayPoint
struct way_point_le: public way_point {
	uint16_t	id;
};

TYPEDEF_STD_VECTOR(way_point_le)

// CWayObject
class xr_way_object: public xr_custom_object {
public:
				xr_way_object(xr_scene& scene);
	virtual			~xr_way_object();
	virtual void		load(xr_reader& r);
	virtual void		save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;

	way_point_le_vec&	points();
	way_link_vec&		links();
	uint32_t&		type();

private:
	way_point_le_vec	m_points;	// WAYOBJECT_CHUNK_POINTS
	way_link_vec		m_links;	// WAYOBJECT_CHUNK_LINKS
	uint32_t		m_type;		// WAYOBJECT_CHUNK_TYPE
};

inline way_point_le_vec& xr_way_object::points() { return m_points; }
inline way_link_vec& xr_way_object::links() { return m_links; }
inline uint32_t& xr_way_object::type() { return m_type; }


// ESceneWayTools
class xr_scene_ways: public xr_scene_objects {
public:
				xr_scene_ways(xr_scene& scene);
	virtual			~xr_scene_ways();
	virtual void		load(xr_reader& r);
	virtual void		save(xr_writer& w) const;

	virtual void		save_v12(xr_ini_writer* w) const;
};

} // end of namespace xray_re

#endif
