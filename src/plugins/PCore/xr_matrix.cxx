#include "xr_matrix.h"

namespace xray_re {
	template<> bool _matrix<float>::can_invert_43(void) const;
	template<> _matrix<float>& _matrix<float>::invert_43(const _matrix<float>& a);
	template<> _matrix<float>& _matrix<float>::mul(const _matrix<float>& a, const _matrix<float>& b);
	template<> _matrix<float>& _matrix<float>::mul_43(const _matrix<float>& a, const _matrix<float>& b);
	template<> void _matrix<float>::get_hpb(float& h, float& p, float& b) const;
	
	template<> bool _matrix<double>::can_invert_43(void) const;
	template<> _matrix<double>& _matrix<double>::invert_43(const _matrix<double>& a);
	template<> _matrix<double>& _matrix<double>::mul(const _matrix<double>& a, const _matrix<double>& b);
	template<> _matrix<double>& _matrix<double>::mul_43(const _matrix<double>& a, const _matrix<double>& b);
	template<> void _matrix<double>::get_hpb(double& h, double& p, double& b) const;
}
