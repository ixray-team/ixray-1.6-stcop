#include "xr_vector3.h"
#include "xr_math.h"

namespace xray_re {

static float uv_adjustment[0x2000];
static bool initialized = false;

void initialize()
{
//	for (int i = 0; i != xr_dim(uv_adjustment); ++i) {
	for (int i = xr_dim(uv_adjustment); --i >= 0;) {
		int u = i >> 7;
		int v = i & 0x7f;
		if (u + v >= 127) {
			u = 127 - u;
			v = 127 - v;
		}
		uv_adjustment[i] = 1.f/std::sqrt(u*u + v*v + (126.f-u-v)*(126.f-u-v));
	}
	initialized = true;
}

template<> _vector3<float>& _vector3<float>::decompress(uint16_t s)
{
	if (!initialized)
		initialize();

	int u = (s >> 7) & 0x3f;
	int v = s & 0x7f;
	if (u + v >= 127) {
		u = 127 - u;
		v = 127 - v;
	}
	float adj = uv_adjustment[s & 0x1fff];
	x = adj * u;
	y = adj * v;
	z = adj * (126 - u - v);
	if (s & 0x8000)
		x = -x;
	if (s & 0x4000)
		y = -y;
	if (s & 0x2000)
		z = -z;
	return *this;
}

template<> uint16_t _vector3<float>::compress() const
{
	if (!initialized)
		initialize();

	uint16_t s = 0;
	float _x, _y, _z;
	if ((_x = x) < 0) {
		_x = -_x;
		s |= 0x8000;
	}
	if ((_y = y) < 0) {
		_y = -_y;
		s |= 0x4000;
	}
	if ((_z = z) < 0) {
		_z = -_z;
		s |= 0x2000;
	}
	float adj = 126.f/(_x + _y + _z);
	int u = int(std::floor(_x * adj));
	int v = int(std::floor(_y * adj));
	if (u > 63) {
		u = 127 - u;
		v = 127 - v;
	}
	return (u << 7) | v | s;
}

}
