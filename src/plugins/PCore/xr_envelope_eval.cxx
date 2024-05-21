/*
======================================================================
interp.c

Interpolation (and extrapolation) of LightWave envelopes.

Ernie Wright  16 Nov 00

The LightWave plug-in SDK provides functions for evaluating envelopes
and channels at arbitrary times, which is what plug-ins should use.
This code shows how to evaluate envelopes in standalone programs.
====================================================================== */

#include "xr_envelope.h"
#include "xr_math.h"

using namespace xray_re;

static float range(float v, float lo, float hi, int* i = 0)
{
	float r = hi - lo;
	if (r == 0) {
		if (i)
			*i = 0;
		return lo;
	}
	float v2 = lo + v - r*std::floor((v - lo)/r);
	if (i)
		*i = -(int)((v2 - v)/r + (v2 > v ? 0.5 : -0.5));
	return v2;
}

static void hermite(float t, float* h1, float* h2, float* h3, float* h4)
{
	float t2 = t*t, t3 = t2*t;
	*h2 = 3.f*t2 - t3 - t3;
	*h1 = 1.f - *h2;
	*h4 = t3 - t2;
	*h3 = *h4 - t2 + t;
}

static float bezier(float x0, float x1, float x2, float x3, float t)
{
	float t2 = t*t, t3 = t2*t;

	float c = 3.f*(x1 - x0);
	float b = 3.f*(x2 - x1) - c;
	float a = x3 - x0 - c - b;

	return a*t3 + b*t2 + c*t + x0;
}

static float bez2_time(float x0, float x1, float x2, float x3, float time,
		float* t0, float* t1)
{
	float t = *t0 + (*t1 - *t0)*0.5f;
	float v = bezier(x0, x1, x2, x3, t);
	if (std::abs(time - v) > 1e-4f) {
		if (v > time)
			*t1 = t;
		else
			*t0 = t;
		return bez2_time(x0, x1, x2, x3, time, t0, t1);
	} else {
		return t;
	}
}

static float bez2(const xr_key* key0, const xr_key* key1, float time)
{
	float x = (key0->shape == xr_key::SHAPE_BEZ2) ?
			key0->time + key0->param[2] :
			key0->time + (key1->time - key0->time)/3.f;
	float t0 = 0.f, t1 = 1.f;
	float t = bez2_time(key0->time, x, key1->time + key1->param[0], key1->time, time, &t0, &t1);

	float y = (key0->shape == xr_key::SHAPE_BEZ2) ?
			key0->value + key0->param[3] :
			key0->value + key0->param[1]/3.f;
	return bezier(key0->value, y, key1->param[1] + key1->value, key1->value, t);
}

static float outgoing(const xr_key* key0_prev, const xr_key* key0, const xr_key* key1)
{
	float a, b, d, t, out;
	switch (key0->shape) {
	case xr_key::SHAPE_TCB:
		a = (1.f - key0->tension)*(1.f + key0->continuity)*(1.f + key0->bias);
		b = (1.f - key0->tension)*(1.f - key0->continuity)*(1.f - key0->bias);
		d = key1->value - key0->value;
		if (key0_prev) {
			t = (key1->time - key0->time)/(key1->time - key0_prev->time);
			out = t*(a*(key0->value - key0_prev->value) + b*d);
		} else {
			out = b*d;
		}
		break;

	case xr_key::SHAPE_LINE:
		d = key1->value - key0->value;
		if (key0_prev) {
			t = (key1->time - key0->time)/(key1->time - key0_prev->time);
			out = t*(key0->value - key0_prev->value + d);
		} else {
			out = d;
		}
		break;

	case xr_key::SHAPE_BEZI:
	case xr_key::SHAPE_HERM:
		out = key0->param[1];
		if (key0_prev)
			out *= (key1->time - key0->time)/(key1->time - key0_prev->time);
		break;

	case xr_key::SHAPE_BEZ2:
		out = key0->param[3]*(key1->time - key0->time);
		if (std::abs(key0->param[2]) > 1e-5f)
			out /= key0->param[2];
		else
			out *= 1e+5f;
		break;

	case xr_key::SHAPE_STEP:
	default:
		out = 0.f;
		break;
	}
	return out;
}

static float incoming(const xr_key* key0, const xr_key* key1, const xr_key* key1_next)
{
	float a, b, d, t, in;
	switch (key1->shape) {
	case xr_key::SHAPE_LINE:
		d = key1->value - key0->value;
		if (key1_next) {
			t = (key1->time - key0->time)/(key1_next->time - key0->time);
			in = t*(key1_next->value - key1->value + d);
		} else {
			in = d;
		}
		break;

	case xr_key::SHAPE_TCB:
		a = (1.f - key1->tension)*(1.f - key1->continuity)*(1.f + key1->bias);
		b = (1.f - key1->tension)*(1.f + key1->continuity)*(1.f - key1->bias);
		d = key1->value - key0->value;
		if (key1_next) {
			t = (key1->time - key0->time)/(key1_next->time - key0->time);
			in = t*(b*(key1_next->value - key1->value) + a*d);
		} else {
			in = a*d;
		}
		break;

	case xr_key::SHAPE_BEZI:
	case xr_key::SHAPE_HERM:
		in = key1->param[0];
		if (key1_next)
			in *= (key1->time - key0->time)/(key1_next->time - key0->time);
		break;

	case xr_key::SHAPE_BEZ2:
		in = key1->param[1]*(key1->time - key0->time);
		if (std::abs(key1->param[0]) > 1e-5f)
			in /= key1->param[0];
		else
			in *= 1e+5f;
		break;

	case xr_key::SHAPE_STEP:
	default:
		in = 0.f;
		break;
	}
	return in;
}

float xr_envelope::evaluate(float time) const
{
	if (m_keys.empty())
		return 0.f;

	if (m_keys.size() == 1)
		return m_keys.front()->value;

	int noff;
	float offset = 0;

	xr_key* skey = m_keys.front();
	xr_key* ekey = m_keys.back();
	if (time < skey->time) {
		switch (m_behaviour0) {
		case BEH_RESET:
			return 0.f;

		case BEH_CONSTANT:
			return skey->value;

		case BEH_REPEAT:
			time = range(time, skey->time, ekey->time);
			break;

		case BEH_OSCILLATE:
			time = range(time, skey->time, ekey->time, &noff);
			if (noff % 2)
				time = ekey->time - skey->time - time;
			break;

		case BEH_OFFSET:
			time = range(time, skey->time, ekey->time, &noff);
			offset = noff*(ekey->value - skey->value);
			break;

		case BEH_LINEAR: {
			xr_key* next = m_keys[1];
			return outgoing(0, skey, next)/(next->time - skey->time)*(time - skey->time) + skey->value;
			}
		}
	} else if (ekey->time < time) {
		switch (m_behaviour1) {
		case BEH_RESET:
			return 0.f;

		case BEH_CONSTANT:
			return ekey->value;

		case BEH_REPEAT:
			time = range(time, skey->time, ekey->time);
			break;

		case BEH_OSCILLATE:
			time = range(time, skey->time, ekey->time, &noff);
			if (noff % 2)
				time = ekey->time - skey->time - time;
			break;

		case BEH_OFFSET:
			time = range(time, skey->time, ekey->time, &noff);
			offset = noff*(ekey->value - skey->value);
			break;

		case BEH_LINEAR: {
			xr_key* prev = *(m_keys.end() - 2);
			return incoming(prev, ekey, 0)/(ekey->time - prev->time)*(time - ekey->time) + ekey->value;
			}
		}
	}

	xr_key_vec_cit it = m_keys.begin() + 1;
	// FIXME: use bisection
	while ((*it)->time < time)
		++it;
	xr_key* key0 = *(it - 1);
	xr_key* key1 = *it;

	if (time == key0->time)
		return key0->value + offset;
	else if (time == key1->time)
		return key1->value + offset;

	float t = (time - key0->time)/(key1->time - key0->time);

	switch (key1->shape) {
	case xr_key::SHAPE_TCB:
	case xr_key::SHAPE_BEZI:
	case xr_key::SHAPE_HERM: {
		float out = outgoing(key0 == skey ? 0 : *(it - 2), key0, key1);
		float in = incoming(key0, key1, key1 == ekey ? 0 : *(it + 1));
		float h1, h2, h3, h4;
		hermite(t, &h1, &h2, &h3, &h4);
		return h1*key0->value + h2*key1->value + h3*out + h4*in + offset;
	}
	case xr_key::SHAPE_BEZ2:
		return bez2(key0, key1, time) + offset;

	case xr_key::SHAPE_LINE:
		return key0->value + t*(key1->value - key0->value) + offset;

	case xr_key::SHAPE_STEP:
		return key0->value + offset;

	default:
		return offset;
	}
}
