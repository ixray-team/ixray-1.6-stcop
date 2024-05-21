#include "xr_obj_motion.h"
#include "xr_envelope.h"
#include "xr_file_system.h"

using namespace xray_re;

xr_obj_motion::xr_obj_motion(): xr_motion(MT_OBJECT)
{
	std::uninitialized_fill_n(m_envelopes, xr_dim(m_envelopes), static_cast<xr_envelope*>(0));
}

xr_obj_motion::~xr_obj_motion()
{
	delete_envelopes();
}

void xr_obj_motion::create_envelopes()
{
	for (uint_fast32_t i = 6; i != 0;)
		m_envelopes[--i] = new xr_envelope;
}

void xr_obj_motion::delete_envelopes()
{
	for (uint_fast32_t i = 6; i != 0;)
		delete m_envelopes[--i];
}

void xr_obj_motion::load(xr_reader& r)
{
	xr_motion::load(r);
	uint16_t version = r.r_u16();
	create_envelopes();
	if (version == OMOTION_VERSION_3) {
		for (uint_fast32_t i = 0; i != 6; ++i)
			m_envelopes[i]->load_1(r);
	} else if (version == OMOTION_VERSION_4) {
		for (uint_fast32_t i = 0; i != 3; ++i)
			m_envelopes[i]->load_2(r);
		m_envelopes[4]->load_2(r);
		m_envelopes[3]->load_2(r);
		m_envelopes[5]->load_2(r);
	} else if (version == OMOTION_VERSION_5) {
		for (uint_fast32_t i = 0; i != 6; ++i)
			m_envelopes[i]->load_2(r);
	} else {
		xr_not_expected();
	}
}

void xr_obj_motion::save(xr_writer& w) const
{
	xr_motion::save(w);
	w.w_u16(OMOTION_VERSION_5);
	for (uint_fast32_t i = 0; i != 6; ++i)
		m_envelopes[i]->save(w);
}

bool xr_obj_motion::load_anm(const char* path)
{
	xr_file_system& fs = xr_file_system::instance();
	xr_reader* r = fs.r_open(path);
	if (r == 0)
		return false;
	xr_reader* s = r->open_chunk(EOBJ_CHUNK_OMOTION);
	xr_assert(s);
	load(*s);
	r->close_chunk(s);
	fs.r_close(r);
	return true;
}

bool xr_obj_motion::save_anm(const char* path) const
{
	xr_memory_writer* w = new xr_memory_writer();
	w->open_chunk(EOBJ_CHUNK_OMOTION);
	save(*w);
	w->close_chunk();
	bool status = w->save_to(path);
	delete w;
	return status;
}

void xr_obj_motion::evaluate(float time, fvector3& t, fvector3& r) const
{
	t.x = m_envelopes[0]->evaluate(time);
	t.y = m_envelopes[1]->evaluate(time);
	t.z = m_envelopes[2]->evaluate(time);
	r.x = m_envelopes[4]->evaluate(time);
	r.y = m_envelopes[3]->evaluate(time);
	r.z = m_envelopes[5]->evaluate(time);
}
