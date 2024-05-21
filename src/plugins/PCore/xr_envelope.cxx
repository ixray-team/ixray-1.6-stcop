#include "xr_envelope.h"
#include "xr_reader.h"
#include "xr_writer.h"
#include "xr_utils.h"

using namespace xray_re;

void xr_key::load_1(xr_reader& r)
{
	value = r.r_float();
	time = r.r_float();
	shape = uint8_t(r.r_u32());
	tension = r.r_float();
	continuity = r.r_float();
	bias = r.r_float();
	r.r_cseq(4, param);
}

void xr_key::load_2(xr_reader& r)
{
	value = r.r_float();
	time = r.r_float();
	if ((shape = r.r_u8()) != SHAPE_STEP) {
		tension = r.r_float_q16(-32.f, 32.f);
		continuity = r.r_float_q16(-32.f, 32.f);
		bias = r.r_float_q16(-32.f, 32.f);
		for (uint_fast32_t i = 0; i != 4; ++i)
			param[i] = r.r_float_q16(-32.f, 32.f);
	}
}

void xr_key::save(xr_writer& w) const
{
	w.w_float(value);
	w.w_float(time);
	w.w_u8(shape);
	if (shape != SHAPE_STEP) {
		w.w_float_q16(tension, -32.f, 32.f);
		w.w_float_q16(continuity, -32.f, 32.f);
		w.w_float_q16(bias, -32.f, 32.f);
		for (uint_fast32_t i = 0; i != 4; ++i)
			w.w_float_q16(param[i], -32.f, 32.f);
	}
}

xr_envelope::~xr_envelope()
{
	delete_elements(m_keys);
}

void xr_envelope::insert_key(xr_key* key)
{
	if (m_keys.empty()) {
		m_keys.push_back(key);
	} else if (m_keys.size() == 1) {
		m_keys.insert((key->time < m_keys.front()->time) ? m_keys.begin() : m_keys.end(), key);
	} else {
		xr_key* skey = m_keys.front();
		xr_key* ekey = m_keys.back();

		xr_key_vec_it it;
		float time = key->time;
		if (time < skey->time) {
			it = m_keys.begin();
		} else if (ekey->time < time) {
			it = m_keys.end();
		} else {
			for (it = m_keys.begin() + 1; (*it)->time < time; ++it) {}
			--it;
		}
		m_keys.insert(it, key);
	}
}

void xr_envelope::insert_key(float time, float value)
{
	xr_key_vec_it it;
#if 0
	it = m_keys.end();
#else
	if (m_keys.empty()) {
		it = m_keys.begin();
	} else if (m_keys.size() == 1) {
		xr_key* key = m_keys.front();
		switch (key->shape) {
		case xr_key::SHAPE_STEP:
			if (value == key->value)
				return;
			break;

		default:
			break;
		}
		if (time < key->time)
			it = m_keys.begin();
		else
			it = m_keys.end();
	} else {
		xr_key* skey = m_keys.front();
		xr_key* ekey = m_keys.back();

		if (time < skey->time) {
			switch (skey->shape) {
			case xr_key::SHAPE_STEP:
				if (value == skey->value)
					return;
				break;

			default:
				break;
			}
			it = m_keys.begin();
		} else if (ekey->time < time) {
			switch (ekey->shape) {
			case xr_key::SHAPE_STEP:
				if (value == ekey->value)
					return;
				break;

			default:
				break;
			}
			it = m_keys.end();
		} else {
			for (it = m_keys.begin() + 1; (*it)->time < time; ++it) {}
			xr_key* key1 = *it;
			xr_key* key0 = *--it;
			switch (key1->shape) {
			case xr_key::SHAPE_STEP:
				if (value == key0->value)
					return;
				break;

			default:
				break;
			};
		}
	}
#endif
	xr_key* key = new xr_key;
	key->time = time;
	key->value = value;
	key->shape = xr_key::SHAPE_STEP;
	m_keys.insert(it, key);
}

void xr_envelope::load_1(xr_reader& r)
{
	m_behaviour0 = uint8_t(r.r_u32() & UINT8_MAX);
	m_behaviour1 = uint8_t(r.r_u32() & UINT8_MAX);
	r.r_seq(r.r_u32(), m_keys, xr_reader::f_r_new<xr_key>(&xr_key::load_1));
}

void xr_envelope::load_2(xr_reader& r)
{
	m_behaviour0 = r.r_u8();
	m_behaviour1 = r.r_u8();
	r.r_seq(r.r_u16(), m_keys, xr_reader::f_r_new<xr_key>(&xr_key::load_2));
}

void xr_envelope::save(xr_writer& w) const
{
	w.w_u8(m_behaviour0);
	w.w_u8(m_behaviour1);
	w.w_size_u16(m_keys.size());
	w.w_seq(m_keys, xr_writer::f_w_const<xr_key>(&xr_key::save));
}
