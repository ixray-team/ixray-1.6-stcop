#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_ENVELOPE_H__
#define __XR_ENVELOPE_H__

#include <string>
#include <vector>
#include "xr_types.h"
#include <memory>

namespace xray_re {

class xr_reader;
class xr_writer;

// These data structures came from LightWave 3D.
struct xr_key {
	enum shape_type {
		SHAPE_TCB	= 0,
		SHAPE_HERM	= 1,
		SHAPE_BEZI	= 2,
		SHAPE_LINE	= 3,
		SHAPE_STEP	= 4,
		SHAPE_BEZ2	= 5,
	};

			xr_key();
			xr_key(shape_type _shape, float _time, float _value);
	void		load_1(xr_reader& r);
	void		load_2(xr_reader& r);
	void		save(xr_writer& w) const;

	float		value;
	float		time;
	uint8_t		shape;
	float		tension;
	float		continuity;
	float		bias;
	float		param[4];
};

TYPEDEF_STD_VECTOR_PTR(xr_key)

inline xr_key::xr_key() {}
inline xr_key::xr_key(shape_type _shape, float _time, float _value):
	value(_value), time(_time), shape(_shape),
	tension(0), continuity(0), bias(0)
{
	std::uninitialized_fill_n(param, xr_dim(param), 0.f);
}

class xr_envelope {
public:
			xr_envelope();
	virtual		~xr_envelope();
	void		load_a(xr_reader& r);
	void		load_1(xr_reader& r);
	void		load_2(xr_reader& r);
	void		save_a(xr_writer& w) const;
	void		save(xr_writer& w) const;
	float		evaluate(float time) const;

	void		insert_key(float time, float value);
	void		insert_key(xr_key* key);

	const xr_key_vec&	keys() const;
	uint8_t&		pre_behaviour();
	uint8_t			pre_behaviour() const;
	uint8_t&		post_behaviour();
	uint8_t			post_behaviour() const;

	enum behaviour {
		BEH_RESET	= 0,
		BEH_CONSTANT	= 1,
		BEH_REPEAT	= 2,
		BEH_OSCILLATE	= 3,
		BEH_OFFSET	= 4,
		BEH_LINEAR	= 5,
	};

protected:
	xr_key_vec	m_keys;
	uint8_t		m_behaviour0;
	uint8_t		m_behaviour1;
};

inline xr_envelope::xr_envelope(): m_behaviour0(BEH_CONSTANT), m_behaviour1(BEH_CONSTANT) {}
inline const xr_key_vec& xr_envelope::keys() const { return m_keys; }
inline uint8_t& xr_envelope::pre_behaviour() { return m_behaviour0; }
inline uint8_t xr_envelope::pre_behaviour() const { return m_behaviour0; }
inline uint8_t& xr_envelope::post_behaviour() { return m_behaviour1; }
inline uint8_t xr_envelope::post_behaviour() const { return m_behaviour1; }

} // end of namespace xray_re

#endif
