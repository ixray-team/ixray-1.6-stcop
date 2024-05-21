#include <climits>
#include "xr_name_gen.h"
#include "xr_string_utils.h"

using namespace xray_re;

static const size_t k_reserved = 16;

// when mimic_gsc is true: pattern, pattern_0000, pattern_0001, ...
// otherwise: pattern0000, pattern0001, pattern0002, ...
void xr_name_gen::init(const char* pattern, bool mimic_gsc)
{
	size_t size = std::strlen(pattern);
	m_name = new char[size + k_reserved];
	std::memcpy(m_name, pattern, size + 1);
	m_offset = unsigned(size & UINT_MAX);
	if (mimic_gsc) {
		m_next_idx = -1;
	} else {
		m_next_idx = 0;
		std::memcpy(&m_name[m_offset], "0000", 5);
	}
}

void xr_name_gen::next()
{
	if (m_next_idx++ < 0)
		m_name[m_offset++] = '_';
	xr_snprintf(&m_name[m_offset], k_reserved - 1, "%4.4d", m_next_idx);
}
