#include "xr_influence.h"

namespace xray_re {
	template<> void _influence<uint32_t, float>::set(uint16_t bone0, uint16_t bone1, float weight0);
	template<> void _influence<uint32_t, float>::set(size_t num_bones, const uint16_t* bones, const float* weights);
	template<> void _influence<uint32_t, float>::reorder();
}
