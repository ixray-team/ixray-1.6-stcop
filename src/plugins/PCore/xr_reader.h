#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_READER_H__
#define __XR_READER_H__

#include <string>
#include <vector>
#include <functional>
#include "xr_vector2.h"
#include "xr_vector3.h"
#include "xr_color.h"

namespace xray_re {

class xr_packet;
class xr_scrambler;

class xr_reader {
public:
			xr_reader();
			xr_reader(const void* data, size_t size);
	virtual		~xr_reader();

	enum {
		CHUNK_COMPRESSED	= 0x80000000,
	};

	size_t		find_chunk(uint32_t id, bool* compressed, bool reset = true);
	size_t		find_chunk(uint32_t id);
	void		debug_find_chunk();
	xr_reader*	open_chunk(uint32_t id);
	xr_reader*	open_chunk(uint32_t id, const xr_scrambler& scrambler);
	xr_reader*	open_chunk_next(uint32_t& id, xr_reader* iter);
	void		close_chunk(xr_reader*& r) const;

	size_t		size() const;
	const void*	data() const;
	void		advance(size_t offset);
	void		seek(size_t pos);
	bool		eof() const;
	size_t		tell() const;
	size_t		elapsed() const;

	size_t		r_raw_chunk(uint32_t id, void* dest, size_t dest_size);
	void		r_raw(void* dest, size_t dest_size);

	template<typename T> size_t		r_chunk(uint32_t id, T& value);
	template<typename T, typename F> void	r_chunks(T& container, F read);
	template<typename T> T			r();
	template<typename T> void		r(T& value);
	template<typename T> void		r_cseq(size_t n, T values[]);
	template<typename T, typename F> void	r_cseq(size_t n, T values[], F read);
	template<typename T> void		r_seq(size_t n, T& container);
	template<typename T, typename F> void	r_seq(size_t n, T& container, F read);
	template<typename T> const T*		pointer() const;
	template<typename T> const T*		skip(size_t n = 1);

	const char*	skip_sz();
	void		r_s(std::string& value);
	void		r_sz(std::string& value);
	void		r_sz(char* dest, size_t dest_size);
	uint32_t	r_u32();
	int32_t		r_s32();
	uint32_t	r_u24();
	uint16_t	r_u16();
	int16_t		r_s16();
	uint8_t		r_u8();
	int8_t		r_s8();
	bool		r_bool();
	float		r_float();
	float		r_float_q16(float min = 0, float max = 1.f);
	float		r_float_q8(float min = 0, float max = 1.f);
	void		r_fvector3(fvector3& v);
	void		r_fvector2(fvector2& v);
	void		r_i32vector2(i32vector2& v);
	void		r_fcolor(fcolor& c);
	void		r_dir(fvector3& v);
	void		r_sdir(fvector3& v);
	void		r_packet(xr_packet& packet, size_t size);

	struct f_r_sz {
		void operator()(std::string& s, xr_reader& r) { r.r_sz(s); }
	};
	template<typename T> struct f_r_new {
		explicit f_r_new(void (T::*_pmf)(xr_reader& r)): pmf(_pmf) {}
		void operator()(T*& p, xr_reader& r) { T* _p = new T; (_p->*pmf)(r); p = _p; }
	private:
		void (T::*pmf)(xr_reader& r);
	};

protected:
	const uint8_t*	m_data;
#if 1
	union {
		const uint8_t*	m_p;
		const int8_t*	m_p_s8;
		const uint16_t*	m_p_u16;
		const int16_t*	m_p_s16;
		const uint32_t*	m_p_u32;
		const int32_t*	m_p_s32;
		const float*	m_p_f;
	};
#else
	const uint8_t*	m_p;
#endif
	const uint8_t*	m_end;

	const uint8_t*	m_next;

private:
//	const uint8_t*	m_debug_find_chunk;
};

// for compressed chunks
class xr_temp_reader: public xr_reader {
public:
			xr_temp_reader(const uint8_t* data, size_t size);
	virtual		~xr_temp_reader();
};

inline size_t xr_reader::size() const { assert(m_data <= m_end); return m_end - m_data; }
inline const void* xr_reader::data() const { return m_data; }
inline void xr_reader::advance(size_t ofs) { m_p += ofs; assert(m_p <= m_end); }
inline void xr_reader::seek(size_t ofs) { m_p = m_data + ofs; assert(m_p <= m_end); }
inline bool xr_reader::eof() const { assert(m_p <= m_end); return m_p == m_end; }
inline size_t xr_reader::tell() const { assert(m_p <= m_end); return m_p - m_data; }
inline size_t xr_reader::elapsed() const { assert(m_p <= m_end); return m_end - m_p; }

template<typename T> inline T xr_reader::r() { T value; r_raw(&value, sizeof(T)); return value; }
template<typename T> inline void xr_reader::r(T& value) { value = *reinterpret_cast<const T*>(m_p); m_p += sizeof(T); }
inline uint32_t xr_reader::r_u32() { return *m_p_u32++; }
inline int32_t xr_reader::r_s32() { return *m_p_s32++; }
inline uint32_t xr_reader::r_u24() { uint32_t u24 = 0; r_raw(&u24, 3); return u24; }
inline uint16_t xr_reader::r_u16() { return *m_p_u16++; }
inline int16_t xr_reader::r_s16() { return *m_p_s16++; }
inline uint8_t xr_reader::r_u8() { return *m_p++; }
inline int8_t xr_reader::r_s8() { return *m_p_s8++; }
inline bool xr_reader::r_bool() { return *m_p++ != 0; }
inline float xr_reader::r_float() { return *m_p_f++; }
inline void xr_reader::r_fvector3(fvector3& v)
{
	v.x = *m_p_f++;
	v.y = *m_p_f++;
	v.z = *m_p_f++;
}
inline void xr_reader::r_fvector2(fvector2& v)
{
	v.x = *m_p_f++;
	v.y = *m_p_f++;
}
inline void xr_reader::r_i32vector2(i32vector2& v)
{
	v.x = *m_p_s32++;
	v.y = *m_p_s32++;
}
inline void xr_reader::r_fcolor(fcolor& c)
{
	c.r = *m_p_f++;
	c.g = *m_p_f++;
	c.b = *m_p_f++;
	c.a = *m_p_f++;
}

template<typename T> inline const T* xr_reader::pointer() const { return reinterpret_cast<const T*>(m_p); }
template<typename T> inline const T* xr_reader::skip(size_t n)
{
	const T* p = pointer<T>();
	advance(n*sizeof(T));
	return p;
}

template<typename T> inline void xr_reader::r_seq(size_t n, T& container)
{
	typename T::const_pointer p = pointer<typename T::value_type>();
	container.reserve(n);
	container.assign(p, p + n);
	advance(n*sizeof(typename T::value_type));
}

template<typename T, typename F> inline void xr_reader::r_seq(size_t n, T& container, F read)
{
	container.reserve(n);
	while (n--) {
		container.push_back(typename T::value_type());
		read(container.back(), *this);
	}
}

template<typename T> inline void xr_reader::r_cseq(size_t n, T values[])
{
	for (T *p = values, *end = p + n; p != end; ++p)
		r<T>(*p);
}

template<typename T, typename F> inline void xr_reader::r_cseq(size_t n, T values[], F read)
{
	for (T *p = values, *end = p + n; p != end; ++p)
		read(*p, *this);
}

template<typename T> inline size_t xr_reader::r_chunk(uint32_t id, T& value)
{
	return r_raw_chunk(id, &value, sizeof(T));
}

template<> inline size_t xr_reader::r_chunk(uint32_t id, std::string& value)
{
	size_t size = find_chunk(id);
	if (size) {
		r_sz(value);
		debug_find_chunk();
	}
	return size;
}

template<typename T, typename F> inline void xr_reader::r_chunks(T& container, F read)
{
	xr_reader* s;
	for (uint32_t id = 0; (s = open_chunk(id)); ++id) {
		container.push_back(typename T::value_type());
		read(container.back(), *s);
		close_chunk(s);
	}
}

inline void xr_reader::debug_find_chunk()
{
//	assert(m_p == m_debug_find_chunk);
}

inline xr_temp_reader::xr_temp_reader(const uint8_t* data, size_t size): xr_reader(data, size) {}

} // end of namespace xray_re

#endif
