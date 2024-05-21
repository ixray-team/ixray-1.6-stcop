#include <cstdlib>
#include <cstring>
#include "xr_reader.h"
#include "xr_lzhuf.h"
#include "xr_packet.h"

using namespace xray_re;

const uint32_t CHUNK_ID_MASK = ~xr_reader::CHUNK_COMPRESSED;

xr_reader::xr_reader(): m_data(0), m_p(0), m_end(0), m_next(0) {}

xr_reader::xr_reader(const void* data, size_t size)
{
	m_next = m_p = m_data = static_cast<const uint8_t*>(data);
	m_end = m_data + size;
}

xr_reader::~xr_reader() {}

size_t xr_reader::find_chunk(uint32_t find_id, bool* compressed, bool reset)
{
	if (reset)
		m_p = m_data;
	while (m_p < m_end) {
		assert(m_p + 8 <= m_end);
		uint32_t id = r_u32();
		size_t size = r_u32();
		assert(m_p + size <= m_end);
		if (find_id == (id & CHUNK_ID_MASK)) {
			xr_assert(compressed || (id & CHUNK_COMPRESSED) == 0);
			if (compressed != 0)
				*compressed = (id & CHUNK_COMPRESSED) != 0;
//			m_debug_find_chunk = m_p + size;
			return size;
		}
		m_p += size;
	}
	return 0;
}

size_t xr_reader::find_chunk(uint32_t find_id) { return find_chunk(find_id, 0); }

xr_reader* xr_reader::open_chunk(uint32_t id)
{
	bool compressed;
	size_t size = find_chunk(id, &compressed);
	if (size == 0)
		return 0;
	if (compressed) {
		size_t real_size;
		uint8_t* data;
		xr_lzhuf::decompress(data, real_size, m_p, size);
		return new xr_temp_reader(data, real_size);
	} else {
		return new xr_reader(m_p, size);
	}
}

void xr_reader::close_chunk(xr_reader*& r) const
{
	assert(r == 0 || (r != this && r->m_p <= r->m_end));
	delete r;
	r = 0;
}

xr_reader* xr_reader::open_chunk_next(uint32_t& _id, xr_reader* prev)
{
	if (prev)
		delete prev;
	else
		m_next = m_data;
	if (m_next < m_end) {
		assert(m_next + 8 <= m_end);
		m_p = m_next;
		uint32_t id = r_u32();
		uint32_t size = r_u32();
		assert(m_p + size <= m_end);
		m_next = m_p + size;
		_id = id;
		if (id & CHUNK_COMPRESSED) {
			size_t real_size;
			uint8_t* data;
			xr_lzhuf::decompress(data, real_size, m_p, size);
			return new xr_temp_reader(data, real_size);
		} else {
			return new xr_reader(m_p, size);
		}
	}
	return 0;
}

size_t xr_reader::r_raw_chunk(uint32_t id, void *dest, size_t dest_size)
{
	bool compressed;
	size_t size = find_chunk(id, &compressed);
	if (size == 0)
		return 0;
	assert(!compressed);
	assert(size <= dest_size);
	r_raw(dest, size);
	return size;
}

void xr_reader::r_raw(void* dest, size_t dest_size)
{
	assert(m_p + dest_size <= m_end);
	std::memmove(dest, m_p, dest_size);
	m_p += dest_size;
}

const char* xr_reader::skip_sz()
{
	const uint8_t* p = m_p;
	while (m_p < m_end) {
		if (*m_p++ == 0)
			return reinterpret_cast<const char*>(p);
	}
	// always crash if no '\0' in the chunk
	xr_assert(m_p < m_end);
	return reinterpret_cast<const char*>(p);
}

void xr_reader::r_s(std::string& value)
{
	const uint8_t* p = m_p;
	xr_assert(p < m_end);
	while (p != m_end && *p != '\n' && *p != '\r')
		++p;
	value.assign(m_p, p);
	while (p != m_end && (*p == '\n' || *p == '\r'))
		++p;
	m_p = p;
}

void xr_reader::r_sz(std::string& value)
{
	const uint8_t* p = m_p;
	xr_assert(p < m_end);
	while (*p++) {
		// FIXME: we should crash in debug mode if no '\0' in the chunk,
		// but that is not good for older models (from 2215).
		// update: OGF_S_LODS 
		// assert(p < m_end);
		if (p >= m_end) {
			value.assign(m_p, p);
			m_p = p;
			return;
		}
	}
	value.assign(m_p, p - 1);
	m_p = p;
}

void xr_reader::r_sz(char* dest, size_t dest_size)
{
	const uint8_t* p = m_p;
	xr_assert(p < m_end && dest_size > 0);
	const uint8_t* end = p + dest_size;
	if (end > m_end)
		end = m_end;
	while (*p++) {
		// crash in debug mode if we don't fit in buffer or chunk
		assert(p < end);
		if (p >= end) {
			std::memmove(dest, m_p, p - m_p);
			dest[dest_size - 1] = 0;
			m_p = p;
			return;
		}
	}
	std::memmove(dest, m_p, p - m_p);
	m_p = p;
}

void xr_reader::r_packet(xr_packet& packet, size_t size)
{
	packet.init(skip<uint8_t>(size), size);
}

float xr_reader::r_float_q16(float min, float max) { return r_u16()*((max - min)/65535.f) + min; }
float xr_reader::r_float_q8(float min, float max) { return r_u8()*((max - min)/255.f) + min; }

xr_temp_reader::~xr_temp_reader()
{
	if (m_data != 0) {
		free(const_cast<uint8_t*>(m_data));
		m_data = 0;
	}
}
