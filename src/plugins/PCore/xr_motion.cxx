#include "xr_motion.h"
#include "xr_reader.h"
#include "xr_writer.h"

using namespace xray_re;

xr_motion::xr_motion(motion_type type):
	m_frame_start(0), m_frame_end(0), m_fps(30.f), m_type(type) {}

xr_motion::~xr_motion() {}

void xr_motion::load(xr_reader& r)
{
	r.r_sz(m_name);
	m_frame_start = r.r_s32();
	m_frame_end = r.r_s32();
	m_fps = r.r_float();
}

void xr_motion::save(xr_writer& w) const
{
	w.w_sz(m_name);
	w.w_s32(m_frame_start);
	w.w_s32(m_frame_end);
	w.w_float(m_fps);
}
