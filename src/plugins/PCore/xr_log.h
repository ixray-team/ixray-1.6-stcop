#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_LOG_H__
#define __XR_LOG_H__

#include <cstdarg>

namespace xray_re {

class xr_writer;

class xr_log {
public:
			xr_log();
			~xr_log();
	static xr_log&	instance();

	void		init(const char* name, const char* prefix = 0);

	void		diagnostic(const char* format, ...);
	void		diagnostic(const char* format, va_list ap);

	void		fatal(const char* msg, const char* file, unsigned line);

private:
	char		m_buf[1023 + 1];
	size_t		m_buf_size;
	char*		m_buf_p;
	xr_writer*	m_log;
};

inline xr_log::xr_log(): m_buf_size(sizeof(m_buf) - 1), m_buf_p(m_buf), m_log(0) {}

inline xr_log& xr_log::instance()
{
	static xr_log instance0;
	return instance0;
}

} // end of namespace xray_re

#endif
