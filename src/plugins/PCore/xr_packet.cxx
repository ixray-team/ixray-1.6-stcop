#include <cstring>
#include "xr_packet.h"

using namespace xray_re;

xr_ini_packet::xr_ini_packet() : m_counter(0)
{
	w = new xr_ini_writer();
}

void xr_ini_packet::w_raw(const void *data, size_t size)
{
	w->w_raw(data, size);
}

void xr_ini_packet::w_sz(const std::string& value)
{
	write(value);
}

inline void xr_ini_packet::w_u64(uint64_t value) { write_number(value); }
inline void xr_ini_packet::w_u32(uint32_t value) { write_number(value); }
inline void xr_ini_packet::w_s32(int32_t value) { write_number(value); }
inline void xr_ini_packet::w_u16(uint16_t value) { write_number(value); }
inline void xr_ini_packet::w_s16(int16_t value) { write_number(value); }
inline void xr_ini_packet::w_u8(uint8_t value) { write_number(value); }
inline void xr_ini_packet::w_s8(int8_t value) { write_number(value); }
inline void xr_ini_packet::w_bool(bool value) { write(value ? 1 : 0); }
inline void xr_ini_packet::w_float(float value) { write(value); }
inline void xr_ini_packet::w_float_q8(float value, float min, float max) { w_u8(uint8_t((value - min)*255.f/(max - min))); }
inline void xr_ini_packet::w_vec3(const fvector3& value) { write(value); }
/*
inline void xr_ini_packet::w_vec4(const fvector4& value) { write(value); }
inline void xr_ini_packet::w_quat(const fquaternion& value) { write(value); }
*/
inline void xr_ini_packet::w_size_u32(size_t value) { w_u32(static_cast<uint32_t>(value & UINT32_MAX)); }
inline void xr_ini_packet::w_size_u16(size_t value) { w_u16(static_cast<uint16_t>(value & UINT16_MAX)); }
inline void xr_ini_packet::w_size_u8(size_t value) { w_u8(static_cast<uint8_t>(value & UINT8_MAX)); }

xr_packet::xr_packet(): m_w_pos(0), m_r_pos(0)
{
	std::memset(m_buf, 0, sizeof(m_buf));
}

void xr_packet::r_raw(void* dest, size_t size)
{
	xr_assert(m_r_pos + size <= sizeof(m_buf));
	std::memmove(dest, m_buf + m_r_pos, size);
	m_r_pos += size;
}

void xr_packet::w_raw(const void* data, size_t size)
{
	assert(m_w_pos + size <= sizeof(m_buf));
	std::memmove(m_buf + m_w_pos, data, size);
	m_w_pos += size;
}

void xr_packet::r_begin(uint16_t& id)
{
	m_r_pos = 0;
	r_u16(id);
}

void xr_packet::w_begin(uint16_t id)
{
	m_w_pos = 0;
	w_u16(id);
}

const char* xr_packet::skip_sz()
{
	const char* p = reinterpret_cast<const char*>(m_buf + m_r_pos);
	while (m_r_pos < sizeof(m_buf)) {
		if (m_buf[m_r_pos++] == 0)
			return p;
	}
	// crash in debug mode if no 0 in the packet
	xr_assert(m_r_pos < sizeof(m_buf));
	return p;
}

void xr_packet::r_sz(std::string& value)
{
	uint8_t* p = &m_buf[m_r_pos];
	while (m_r_pos < sizeof(m_buf)) {
		if (m_buf[m_r_pos++] == 0) {
			value.assign(p, &m_buf[m_r_pos - 1]);
			return;
		}
	}
	// crash in debug mode if no 0 in the packet
	assert(m_r_pos < sizeof(m_buf));
	value.assign(p, m_buf + m_r_pos);
}

float xr_packet::r_angle8()
{
	r_u8();
	return 0;
}

void xr_packet::r_dir(fvector3& value)
{
	value.decompress(r_u16());
}

void xr_packet::r_sdir(fvector3& value)
{
	value.decompress(r_u16());
	value.mul(r_float());
}

void xr_packet::w_sdir(const fvector3& value)
{
	fvector3 temp;
	float m = value.square_magnitude();
	if (m < 1e-7f) {
		temp.set(0, 0, 0);
		m = 0;
	} else {
		temp.mul(value, 1.f/m);
	}
	w_u16(temp.compress());
	w_float(m);
}

void xr_packet::w_sz(const std::string& value)
{
	w_raw(value.data(), value.length() + 1);
}

void xr_packet::r_matrix(fmatrix& value)
{
	r_vec3(value.i); value._14 = 0;
	r_vec3(value.j); value._24 = 0;
	r_vec3(value.k); value._34 = 0;
	r_vec3(value.c); value._44 = 1.f;
}

void xr_packet::w_matrix(const fmatrix& value)
{
	w_vec3(value.i);
	w_vec3(value.j);
	w_vec3(value.k);
	w_vec3(value.c);
}

void xr_packet::init(const uint8_t* data, size_t size)
{
	xr_assert(size < sizeof(m_buf));
	m_r_pos = 0;
	m_w_pos = size;
	std::memmove(m_buf, data, size);
}
