#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_REMAPPER_H__
#define __XR_REMAPPER_H__

#include <memory>
#include "xr_types.h"

namespace xray_re {

class xr_remapper {
public:
				xr_remapper(uint32_t min, uint32_t max);
				xr_remapper(size_t size);
				~xr_remapper();

	uint32_t		operator[](uint32_t value) const;
	uint32_t&		operator[](uint32_t value);
	uint32_t		min() const;
	uint32_t		max() const;

private:
	uint32_t		m_min, m_max;
	uint32_t*		m_mapping;
};

inline xr_remapper::xr_remapper(size_t size):
	m_min(0), m_max(uint32_t(size & UINT32_MAX) - 1)
{
	m_mapping = new uint32_t[size];
#if defined(_MSC_VER) && _MSC_VER >= 1400 && _MSC_VER < 1600
	stdext::unchecked_uninitialized_fill_n(m_mapping, size, BAD_IDX);
#else
	std::uninitialized_fill_n(m_mapping, size, BAD_IDX);
#endif
}

inline xr_remapper::xr_remapper(uint32_t min, uint32_t max): m_min(min), m_max(max)
{
	m_mapping = new uint32_t[max - min + 1];
#if defined(_MSC_VER) && _MSC_VER >= 1400 && _MSC_VER < 1600
	stdext::unchecked_uninitialized_fill_n(m_mapping, max - min + 1, BAD_IDX);
#else
	std::uninitialized_fill_n(m_mapping, max - min + 1, BAD_IDX);
#endif
}

inline xr_remapper::~xr_remapper()
{
	delete[] m_mapping;
}

inline uint32_t xr_remapper::operator[](uint32_t value) const
{
	if (value < m_min || value > m_max)
		return BAD_IDX;
	return uint32_t(m_mapping[value - m_min]);
}

inline uint32_t& xr_remapper::operator[](uint32_t value)
{
	xr_assert(value >= m_min);
	xr_assert(value <= m_max);
	return m_mapping[value - m_min];
}

inline uint32_t xr_remapper::min() const { return m_min; }
inline uint32_t xr_remapper::max() const { return m_max; }

} // end of namespace xray_re

#endif
