#include "xr_skl_motion.h"
#include "xr_envelope.h"
#include "xr_utils.h"
#include "xr_string_utils.h"
#include "xr_file_system.h"

using namespace xray_re;

xr_bone_motion::xr_bone_motion(): m_flags(0)
{
	std::uninitialized_fill_n(m_envelopes, xr_dim(m_envelopes), static_cast<xr_envelope*>(0));
}

xr_bone_motion::xr_bone_motion(const char* name): m_name(name), m_flags(0)
{
	std::uninitialized_fill_n(m_envelopes, xr_dim(m_envelopes), static_cast<xr_envelope*>(0));
}

xr_bone_motion::~xr_bone_motion()
{
	delete_envelopes();
}

void xr_bone_motion::evaluate(float time, fvector3& t, fvector3& r) const
{
	t.x = m_envelopes[0]->evaluate(time);
	t.y = m_envelopes[1]->evaluate(time);
	t.z = m_envelopes[2]->evaluate(time);
	r.x = m_envelopes[4]->evaluate(time);
	r.y = m_envelopes[3]->evaluate(time);
	r.z = m_envelopes[5]->evaluate(time);
}

void xr_bone_motion::create_envelopes()
{
	for (uint_fast32_t i = 6; i != 0;)
		m_envelopes[--i] = new xr_envelope;
}

void xr_bone_motion::delete_envelopes()
{
	for (uint_fast32_t i = 6; i != 0;)
		delete m_envelopes[--i];
}

void xr_bone_motion::load_0(xr_reader& r)
{
	m_flags = uint8_t(r.r_u32() & UINT8_MAX);
	create_envelopes();
	for (uint_fast32_t i = 0; i != 6; ++i)
		m_envelopes[i]->load_1(r);
}

void xr_bone_motion::load_1(xr_reader& r)
{
	r.r_sz(m_name);
	m_flags = uint8_t(r.r_u32() & UINT8_MAX);
	create_envelopes();
	for (uint_fast32_t i = 0; i != 6; ++i)
		m_envelopes[i]->load_1(r);
}

void xr_bone_motion::load_2(xr_reader& r)
{
	r.r_sz(m_name);
	m_flags = r.r_u8();
	create_envelopes();
	for (uint_fast32_t i = 0; i != 6; ++i)
		m_envelopes[i]->load_2(r);
}

void xr_bone_motion::save(xr_writer& w) const
{
	w.w_sz(m_name);
	w.w_u8(m_flags);
	for (uint_fast32_t i = 0; i != 6; ++i)
		m_envelopes[i]->save(w);
}

////////////////////////////////////////////////////////////////////////////////

void xr_motion_marks::load(xr_reader& r)
{
	r.r_s(m_name);
	r.r_seq(r.r_u32(), *this);
}

void xr_motion_marks::save(xr_writer& w) const
{
	w.w_s(m_name);
	w.w_size_u32(size());
	w.w_seq(*this);
}

////////////////////////////////////////////////////////////////////////////////

xr_skl_motion::xr_skl_motion(): xr_motion(MT_SKELETON),
	m_bone_or_part(ALL_PARTITIONS),
	m_speed(1.f), m_accrue(2.f), m_falloff(2.f), m_power(1.f),
	m_flags(0) {}

xr_skl_motion::~xr_skl_motion()
{
	delete_elements(m_bone_motions);
	delete_elements(m_marks);
}

struct read_bm_0 {
	int index;
	read_bm_0(): index(0) {}
	void operator()(xr_bone_motion*& bm, xr_reader& r) {
		char name[8];
		xr_snprintf(name, sizeof(name), "%d", index++);
		bm = new xr_bone_motion(name);
		bm->load_0(r);
	}
};

void xr_skl_motion::load(xr_reader& r)
{
	xr_motion::load(r);
	uint16_t version = r.r_u16();
	if (version == SMOTION_VERSION_4) {
		m_bone_or_part = r.r_u16();
		if (r.r_bool())
			m_flags |= SMF_FX;
		else
			m_flags &= ~SMF_FX;
		if (r.r_bool())
			m_flags |= SMF_STOP_AT_END;
		else
			m_flags &= ~SMF_STOP_AT_END;
		m_speed = r.r_float();
		m_accrue = r.r_float();
		m_falloff = r.r_float();
		m_power = r.r_float();
		r.r_seq(r.r_u32(), m_bone_motions, read_bm_0());
	} else if (version == SMOTION_VERSION_5) {
		m_flags = r.r_u32();
		m_bone_or_part = uint16_t(r.r_u32() & UINT16_MAX);
		m_speed = r.r_float();
		m_accrue = r.r_float();
		m_falloff = r.r_float();
		m_power = r.r_float();
		r.r_seq(r.r_u32(), m_bone_motions, xr_reader::f_r_new<xr_bone_motion>(&xr_bone_motion::load_1));
	} else if (version == SMOTION_VERSION_6) {
		m_flags = r.r_u8();
		m_bone_or_part = r.r_u16();
		m_speed = r.r_float();
		m_accrue = r.r_float();
		m_falloff = r.r_float();
		m_power = r.r_float();
		r.r_seq(r.r_u16(), m_bone_motions, xr_reader::f_r_new<xr_bone_motion>(&xr_bone_motion::load_2));
	} else if (version == SMOTION_VERSION_7) {
		m_flags = r.r_u8();
		m_bone_or_part = r.r_u16();
		m_speed = r.r_float();
		m_accrue = r.r_float();
		m_falloff = r.r_float();
		m_power = r.r_float();
		r.r_seq(r.r_u16(), m_bone_motions, xr_reader::f_r_new<xr_bone_motion>(&xr_bone_motion::load_2));
		r.r_seq(r.r_u32(), m_marks, xr_reader::f_r_new<xr_motion_marks>(&xr_motion_marks::load));
	} else {
		xr_not_expected();
	}
}

void xr_skl_motion::save(xr_writer& w) const
{
	xr_motion::save(w);
	w.w_u16(m_marks.empty()? SMOTION_VERSION_6 : SMOTION_VERSION_7);
	w.w_u8(uint8_t(m_flags & 0xff));
	w.w_u16(m_bone_or_part);
	w.w_float(m_speed);
	w.w_float(m_accrue);
	w.w_float(m_falloff);
	w.w_float(m_power);
	w.w_size_u16(m_bone_motions.size());
	w.w_seq(m_bone_motions, xr_writer::f_w_const<xr_bone_motion>(&xr_bone_motion::save));
	
	// save marks
	if (!m_marks.empty()) {
		w.w_size_u32(m_marks.size());
		w.w_seq(m_marks, xr_writer::f_w_const<xr_motion_marks>(&xr_motion_marks::save));
	}
}

bool xr_skl_motion::load_skl(const char* path)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path);
	if (r == 0)
		return false;
	xr_reader* s = r->open_chunk(EOBJ_CHUNK_SMOTION);
	xr_assert(s);
	load(*s);
	r->close_chunk(s);
	fs.r_close(r);
	return true;
}

bool xr_skl_motion::save_skl(const char* path) const
{
	xr_memory_writer* w = new xr_memory_writer();
	w->open_chunk(EOBJ_CHUNK_SMOTION);
	save(*w);
	w->close_chunk();
	bool status = w->save_to(path);
	delete w;
	return status;
}

void xr_skl_motion::evaluate(uint16_t bone_id, float time, fvector3& t, fvector3& r) const
{
	m_bone_motions.at(bone_id)->evaluate(time, t, r);
}
