#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_AI_WAY_H__
#define __XR_AI_WAY_H__

#include <vector>
#include <string>
#include "xr_types.h"
#include "xr_vector3.h"

namespace xray_re {

// common LE/game definitions

// way types
enum {
	WAY_TYPE_PATROL_PATH	= 0,
};

// CWayObject
const uint16_t WAYOBJECT_VERSION = 0x13;
const uint16_t WAYOBJECT_VERSION_12 = 0x12;

enum {
	WAYOBJECT_CHUNK_VERSION	= 0x0001,
	WAYOBJECT_CHUNK_POINTS	= 0x0002,
	WAYOBJECT_CHUNK_LINKS	= 0x0003,
	WAYOBJECT_CHUNK_TYPE	= 0x0004,
	WAYOBJECT_CHUNK_NAME	= 0x0005,
};

// in-game/base structures

struct way_link {
	uint16_t	from;
	uint16_t	to;
	float		weight;
};

TYPEDEF_STD_VECTOR(way_link)

struct way_point {
	std::string	name;
	fvector3	position;
	uint32_t	flags;
};

TYPEDEF_STD_VECTOR(way_point)

struct way_path {
	std::string	name;
	way_point_vec	points;
	way_link_vec	links;
};

TYPEDEF_STD_VECTOR_PTR(way_path)

// MP respawn point
struct mp_rpoint {
	fvector3	p;
	fvector3	a;
	uint8_t		team;
	uint8_t		respawn;
	uint8_t		game;
	uint8_t		__unused;
};

TYPEDEF_STD_VECTOR_PTR(mp_rpoint)

class xr_reader;
class xr_writer;

struct way_path_io {
	void	operator()(way_path*& point, xr_reader& r) const;
	void	operator()(const way_path* point, xr_writer& w) const;
};

struct way_point_io {
	void	operator()(way_point& point, xr_reader& r) const;
	void	operator()(const way_point& point, xr_writer& w) const;
};

struct way_point_io_12 {
	void	operator()(way_point& point, xr_reader& r, int index) const;
};

struct way_link_io {
	void	operator()(way_link& link, xr_reader& r) const;
	void	operator()(const way_link& link, xr_writer& w) const;
};

struct way_link_io_12 {
	void	operator()(way_link& link, xr_reader& r) const;
};

struct mp_rpoint_io {
	void	operator()(mp_rpoint*& rpoint, xr_reader& r) const;
	void	operator()(const mp_rpoint* rpoint, xr_writer& w) const;
};

} // end of namespace xray_re

#endif
