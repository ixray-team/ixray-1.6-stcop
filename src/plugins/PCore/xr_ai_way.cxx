#include "xr_ai_way.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_string_utils.h"

using namespace xray_re;

void way_point_io::operator()(way_point& point, xr_reader& r) const
{
	r.r_fvector3(point.position);
	point.flags = r.r_u32();
	r.r_sz(point.name);
}

void way_point_io_12::operator()(way_point& point, xr_reader& r, int index) const
{
	r.r_fvector3(point.position);
	point.flags = r.r_u32();
	char buf[8];
	xr_snprintf(buf, sizeof(buf), "wp%02d", index);
	point.name = buf;
}

void way_point_io::operator()(const way_point& point, xr_writer& w) const
{
	w.w_fvector3(point.position);
	w.w_u32(point.flags);
	w.w_sz(point.name);
}

void way_link_io::operator()(way_link& link, xr_reader& r) const
{
	link.from = r.r_u16();
	link.to = r.r_u16();
	link.weight = r.r_float();
}

void way_link_io_12::operator()(way_link& link, xr_reader& r) const
{
	link.from = r.r_u16();
	link.to = r.r_u16();
	link.weight = 1.0f;
}

void way_link_io::operator()(const way_link& link, xr_writer& w) const
{
	w.w_u16(link.from);
	w.w_u16(link.to);
	w.w_float(link.weight);
}

void way_path_io::operator()(way_path*& _path, xr_reader& r) const
{
	way_path* path = new way_path;
	_path = path;
	uint16_t version;
	if (!r.r_chunk<uint16_t>(WAYOBJECT_CHUNK_VERSION, version))
		xr_not_expected();
	xr_assert(version == WAYOBJECT_VERSION || version == WAYOBJECT_VERSION_12);

	if (!r.find_chunk(WAYOBJECT_CHUNK_POINTS))
		xr_not_expected();

	switch(version) {
	case WAYOBJECT_VERSION:
		r.r_seq(r.r_u16(), path->points, way_point_io());
		break;
	case WAYOBJECT_VERSION_12: {
		int n = r.r_u16();
		path->points.reserve(n);
		for (int i = 0; i != n; i++) {
			path->points.push_back(way_point());
			way_point_io_12()(path->points.back(), r, i);
		}
		}
		break;
	default:
		xr_assert(false);
	}

	r.debug_find_chunk();

	if (!r.find_chunk(WAYOBJECT_CHUNK_LINKS))
		xr_not_expected();

	switch(version) {
	case WAYOBJECT_VERSION:
		r.r_seq(r.r_u16(), path->links, way_link_io());
		break;
	case WAYOBJECT_VERSION_12:
		r.r_seq(r.r_u16(), path->links, way_link_io_12());
		break;
	default:
		xr_assert(false);
	}

	r.debug_find_chunk();

	if (!r.find_chunk(WAYOBJECT_CHUNK_NAME))
		xr_not_expected();
	r.r_sz(path->name);
	r.debug_find_chunk();
}

void way_path_io::operator()(const way_path* path, xr_writer& w) const
{
	w.w_chunk<uint16_t>(WAYOBJECT_CHUNK_VERSION, WAYOBJECT_VERSION);

	w.open_chunk(WAYOBJECT_CHUNK_POINTS);
	w.w_size_u16(path->points.size());
	w.w_seq(path->points, way_point_io());
	w.close_chunk();

	w.open_chunk(WAYOBJECT_CHUNK_LINKS);
	w.w_size_u16(path->links.size());
	w.w_seq(path->links, way_link_io());
	w.close_chunk();

	w.open_chunk(WAYOBJECT_CHUNK_NAME);
	w.w_sz(path->name);
	w.close_chunk();
}

void mp_rpoint_io::operator()(mp_rpoint*& _rpoint, xr_reader& r) const
{
	mp_rpoint* rpoint = new mp_rpoint;
	_rpoint = rpoint;
	r.r_fvector3(rpoint->p);
	r.r_fvector3(rpoint->a);
	rpoint->team = r.r_u8();
	rpoint->respawn = r.r_u8();
	rpoint->game = r.r_u8();
	r.advance(sizeof(uint8_t));
}

void mp_rpoint_io::operator()(const mp_rpoint* rpoint, xr_writer& w) const {
	w.w_fvector3(rpoint->p);
	w.w_fvector3(rpoint->a);
	w.w_u8(rpoint->team);
	w.w_u8(rpoint->respawn);
	w.w_u8(rpoint->game);
	w.w_u8(0);
}
