#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LEVEL_DETAILS_H__
#define __XR_LEVEL_DETAILS_H__

#include <vector>
#include "xr_details.h"

namespace xray_re {

class xr_reader;
class xr_writer;
class xr_image;
class xr_dm;

class xr_level_details {
public:
				xr_level_details();
				xr_level_details(xr_reader& r);
	virtual			~xr_level_details();

	bool			load_texture(const char* path);

	details_header&			header();
	const details_header&		header() const;
	uint32_t			num_slots() const;
	detail_slot_v3*&		slots();
	const detail_slot_v3*		slots() const;
	std::vector<xr_dm*>&		models();
	const std::vector<xr_dm*>&	models() const;
	const xr_image*			texture() const;
	const uint8_t*			raw_texture() const;
	const size_t			raw_texture_size() const;

protected:
	void			load(xr_reader& r);
	void			save(xr_writer& w) const;

private:
	details_header		m_header;
	detail_slot_v3*		m_slots;
	std::vector<xr_dm*>	m_models;
	xr_image*		m_texture;
	size_t			m_raw_texture_size;
	uint8_t*		m_raw_texture;
};

inline xr_level_details::xr_level_details():
	m_slots(0), m_texture(0), m_raw_texture(0) {}
inline xr_level_details::xr_level_details(xr_reader& r):
	m_slots(0), m_texture(0), m_raw_texture(0) { load(r); }
inline details_header& xr_level_details::header() { return m_header; }
inline const details_header& xr_level_details::header() const { return m_header; }
inline uint32_t xr_level_details::num_slots() const { return m_header.size_x*m_header.size_z; }
inline detail_slot_v3*& xr_level_details::slots() { return m_slots; }
inline const detail_slot_v3* xr_level_details::slots() const { return m_slots; }
inline std::vector<xr_dm*>& xr_level_details::models() { return m_models; }
inline const std::vector<xr_dm*>& xr_level_details::models() const { return m_models; }
inline const xr_image* xr_level_details::texture() const { return m_texture; }
inline const uint8_t* xr_level_details::raw_texture() const { return m_raw_texture; }
inline const size_t xr_level_details::raw_texture_size() const { return m_raw_texture_size; }

} // end of namespace xray_re

#endif
