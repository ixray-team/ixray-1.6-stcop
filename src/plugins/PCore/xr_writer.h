#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_WRITER_H__
#define __XR_WRITER_H__

#include <string>
#include <vector>
#include <stack>
#include <functional>
#include "xr_vector2.h"
#include "xr_vector3.h"
#include "xr_color.h"

namespace xray_re {

class xr_packet;
class xr_ini_packet;

class xr_writer {
public:
			xr_writer();
	virtual		~xr_writer();

	virtual void	w_raw(const void* data, size_t size) = 0;
	virtual void	seek(size_t pos) = 0;
	virtual size_t	tell() = 0;

	void		open_chunk(uint32_t id);
	void		close_chunk();
	void		w_raw_chunk(uint32_t id, const void* data, size_t size, bool compress = false);

	void		w_chunk(uint32_t id, const std::string& s);

	template<typename T> void		w_chunk(uint32_t id, const T& value);
	template<typename T, typename F> void	w_chunks(const T& container, F write);
	template<typename T, typename F> void	w_seq(const T& container, F write);
	template<typename T> void		w_seq(const T& container);
	template<typename T> void		w_cseq(size_t n, const T values[]);
	template<typename T, typename F> void	w_cseq(size_t n, const T values[], F write);
	template<typename T> void		w(const T& value);

	void		w_sz(const std::string& value);
	void		w_sz(const char* value);
	void		w_sf(const char* format, ...);
	void		w_s(const std::string& value);
	void		w_s(const char* value);
	void		w_u32(uint32_t value);
	void		w_s32(int32_t value);
	void		w_u24(uint32_t value);
	void		w_u16(uint16_t value);
	void		w_s16(int16_t value);
	void		w_u8(uint8_t value);
	void		w_s8(int8_t value);
	void		w_bool(bool value);
	void		w_float(float value);
	void		w_float_q16(float value, float min = 0, float max = 1.f);
	void		w_float_q8(float value, float min = 0, float max = 1.f);
	void		w_fvector3(const fvector3& v);
	void		w_fvector2(const fvector2& v);
	void		w_i32vector2(const i32vector2& v);
	void		w_fcolor(const fcolor& c);
	void		w_dir(const fvector3& v);
	void		w_sdir(const fvector3& v);
	void		w_size_u32(size_t value);
	void		w_size_u16(size_t value);
	void		w_size_u8(size_t value);

	void		w_packet(const xr_packet& packet);

	struct f_w_sz {
		void operator()(const std::string& s, xr_writer& w) { w.w_sz(s); }
	};
	template<typename T> struct f_w_const {
		using mem_func = void (T::*)(xr_writer& w) const;
		explicit f_w_const(mem_func f): m_f(f) {}
		void operator()(const T* obj, xr_writer& w) { (obj->*m_f)(w); }
		mem_func m_f;
	};

private:
	std::stack<size_t>	m_open_chunks;

	char buf[8192];
};

class xr_fake_writer: public xr_writer {
public:
			xr_fake_writer();
	virtual		~xr_fake_writer();
	virtual void	w_raw(const void* data, size_t size);
	virtual void	seek(size_t pos);
	virtual size_t	tell();

private:
	size_t		m_pos;
	size_t		m_size;
};

class xr_memory_writer: public xr_writer {
public:
			xr_memory_writer();
			~xr_memory_writer();

	virtual void	w_raw(const void* data, size_t size);
	virtual void	seek(size_t pos);
	virtual size_t	tell();

	const uint8_t*	data() const;

	bool		save_to(const char* path);
	bool		save_to(const std::string& path);
	bool		save_to(const char* path, const std::string& name);

private:
	std::vector<uint8_t>	m_buffer;
	size_t			m_pos;
};

struct xr_guid;

class xr_ini_writer: public xr_memory_writer {
public:
				xr_ini_writer();

	void			open_section(const char *name);
	void			open_section(std::string format, ...);
	void			close_section();
	void			write(const char *key, const char *value, bool enclose = true);
	void			write(const char *key, std::string value, bool enclose = true);
	//void			write(const char *key, std::string& value, bool enclose = true);
	void			write(const char *key, float value);
	void			write(const char *key, int8_t value);
	void			write(const char *key, int16_t value);
	void			write(const char *key, int32_t value);
	void			write(const char *key, int64_t value);
	void			write(const char *key, uint8_t value);
	void			write(const char *key, uint16_t value);
	void			write(const char *key, uint32_t value);
	void			write(const char *key, uint64_t value);
	void			write(const char *key, fvector2 value);
	void			write(const char *key, fvector3 value);
	void			write(const char *key, fcolor value);
	void			write(const char *key, xr_guid *value);
	template<typename T, typename F> inline void w_sections(const T& container, F write, const char* prefix = "object");
	template<typename T> inline void w_ini_seq(const T& container, const char* prefix);
	template<typename T, typename F> inline void w_ini_seq(const T& container, F write, const char* prefix);
	template<typename T, typename F> inline void w_ini_seq(const T& container, F write);

	
	void		write_packet(xr_ini_packet* packet);
private:
	std::stack<std::string> m_section_stack;
};

inline xr_writer::xr_writer() {}
inline xr_writer::~xr_writer() {}
template<typename T> inline void xr_writer::w(const T& value) { w_raw(&value, sizeof(T)); }
inline void xr_writer::w_u32(uint32_t value) { w<uint32_t>(value); }
inline void xr_writer::w_s32(int32_t value) { w<int32_t>(value); }
inline void xr_writer::w_u24(uint32_t value) { w_raw(&value, 3); }
inline void xr_writer::w_u16(uint16_t value) { w<uint16_t>(value); }
inline void xr_writer::w_s16(int16_t value) { w<int16_t>(value); }
inline void xr_writer::w_u8(uint8_t value) { w<uint8_t>(value); }
inline void xr_writer::w_s8(int8_t value) { w<int8_t>(value); }
inline void xr_writer::w_bool(bool value) { w_u8(value ? 1 : 0); }
inline void xr_writer::w_float(float value) { w<float>(value); }
inline void xr_writer::w_fvector3(const fvector3& v)
{
	w_float(v.x);
	w_float(v.y);
	w_float(v.z);
}
inline void xr_writer::w_fvector2(const fvector2& v)
{
	w_float(v.x);
	w_float(v.y);
}
inline void xr_writer::w_i32vector2(const i32vector2& v)
{
	w_s32(v.x);
	w_s32(v.y);
}
inline void xr_writer::w_fcolor(const fcolor&c)
{
	w_float(c.r);
	w_float(c.g);
	w_float(c.b);
	w_float(c.a);
}

inline void xr_writer::w_size_u32(size_t value) { w_u32(static_cast<uint32_t>(value & UINT32_MAX)); }
inline void xr_writer::w_size_u16(size_t value) { w_u16(static_cast<uint16_t>(value & UINT16_MAX)); }
inline void xr_writer::w_size_u8(size_t value) { w_u8(static_cast<uint8_t>(value & UINT8_MAX)); }

template<typename T> inline void xr_writer::w_cseq(size_t n, const T values[])
{
	if (n)
		w_raw(values, n*sizeof(T));
}

template<typename T, typename F> inline void xr_writer::w_cseq(size_t n, const T values[], F write)
{
	for (const T *p = values, *end = p + n; p != end; ++p)
		write(*p, *this);
}

template<typename T> inline void xr_writer::w_seq(const T& container)
{
	if (!container.empty())
		w_raw(&container[0], container.size()*sizeof(typename T::value_type));
}

template<typename T, typename F> inline void xr_writer::w_seq(const T& container, F write)
{
	for (typename T::const_iterator it = container.begin(),
			end = container.end(); it != end; ++it) {
		write(*it, *this);
	}
}

inline void xr_writer::w_chunk(uint32_t id, const std::string& s)
{
	open_chunk(id);
	w_sz(s);
	close_chunk();
}

template<typename T> inline void xr_writer::w_chunk(uint32_t id, const T& value)
{
	w_raw_chunk(id, &value, sizeof(T));
}

template<typename T, typename F> inline void xr_writer::w_chunks(const T& container, F write)
{
	typename T::const_iterator it = container.begin(), end = container.end();
	for (uint32_t id = 0; it != end; ++it) {
		open_chunk(id++);
		write(*it, *this);
		close_chunk();
	}
}

template<typename T> inline void xr_ini_writer::w_ini_seq(const T& container, const char* prefix)
{
	char buf[1024];

	typename T::const_iterator it = container.begin(), end = container.end();
	for (uint32_t id = 0; it != end; ++it) {
#if defined(_MSC_VER) && _MSC_VER >= 1400
		int n = sprintf_s(buf, sizeof(buf), "%s_%04d", prefix, id);
#else
		int n = snprintf(buf, sizeof(buf), "%s_%04d", prefix, id);
#endif
		if (n > 0)
			write(buf, *it);
		id++;
	}
}

template<typename T, typename F> inline void xr_ini_writer::w_ini_seq(const T& container, F write)
{
	uint32_t id = 0;
	for (typename T::const_iterator it = container.begin(),
			end = container.end(); it != end; ++it, ++id) {
		write(*it, this, id);
	}
}

template<typename T, typename F> inline void xr_ini_writer::w_ini_seq(const T& container, F write, const char* prefix)
{
	char buf[1024];
	uint32_t id = 0;

	for (typename T::const_iterator it = container.begin(),
			end = container.end(); it != end; ++it, ++id) {
#if defined(_MSC_VER) && _MSC_VER >= 1400
		int n = sprintf_s(buf, sizeof(buf), "%s_%04d", prefix, id);
#else
		int n = snprintf(buf, sizeof(buf), "%s_%04d", prefix, id);
#endif
		if (n > 0)
			write(*it, *this, buf);
	}
}

template<typename T, typename F> inline void xr_ini_writer::w_sections(const T& container, F write, const char* prefix)
{
	typename T::const_iterator it = container.begin(), end = container.end();
	for (uint32_t id = 0; it != end; ++it) {
		open_section("%s_%d", prefix, id++);
		write(*it, this);
		close_section();
	}
}

inline const uint8_t* xr_memory_writer::data() const { return &m_buffer[0]; }

} // end of namespace xray_re

#endif
