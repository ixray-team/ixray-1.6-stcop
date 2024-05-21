#include "xr_vector3.h"
#include "xr_quaternion.h"

namespace xray_re {
	template<> _quaternion<float>& _quaternion<float>::set(const _matrix<float>& m);
	template<> _quaternion<float>& _quaternion<float>::slerp(const _quaternion<float>& q1,
			const _quaternion<float>& q2, float t);

	template<> _quaternion<double>& _quaternion<double>::set(const _matrix<double>& m);
	template<> _quaternion<double>& _quaternion<double>::slerp(const _quaternion<double>& q1,
			const _quaternion<double>& q2, double t);
}
