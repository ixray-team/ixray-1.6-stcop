#include <cstdlib>
#include <cstring>
#include <iostream>
#include "xr_log.h"
#include "xr_file_system.h"
#if defined(DEBUG) && defined(WIN32)
#  include <Windows.h>// OutputDebugString
#endif

using namespace xray_re;

xr_log::~xr_log()
{
	if (m_log)
		xr_file_system::instance().w_close(m_log);
}

void xr_log::init(const char* name, const char* prefix)
{
	xr_file_system& fs = xr_file_system::instance();
	std::string path;
	if (fs.resolve_path(PA_LOGS, name, path))
		m_log = fs.w_open(path.append(".log").c_str(), true);
	if (prefix) {
		size_t n = std::strlen(prefix);
		if (n + 3 > m_buf_size)
			n = m_buf_size - 3;
		if (n)
			std::memcpy(m_buf, prefix, n);
		m_buf[n++] = ':';
		m_buf[n++] = ' ';
		m_buf_size -= n;
		m_buf_p += n;
	}
	if (m_log == 0)
		diagnostic("xray_re: log started (console only)");
	else
		diagnostic("xray_re: log started (console and %s.log)", name);
}

void xr_log::diagnostic(const char* format, va_list ap)
{
#if defined(_MSC_VER) && _MSC_VER >= 1400
	int n = vsprintf_s(m_buf_p, m_buf_size, format, ap);
#else
	int n = vsnprintf(m_buf_p, m_buf_size, format, ap);
#endif
	if (n >= 0) {
		if (m_log)
			m_log->w_s(m_buf);
		// m_buf_size initialization in constructor assures
		// there is space for extra '\0'.
		m_buf_p[n] = '\n';
		m_buf_p[n + 1] = '\0';
		// exclude prefix for the file output
#	if (MAYA_API_VERSION >= 201300) 
		fputs(m_buf_p, stderr);
#	else
		std::cerr << m_buf_p;
#	endif
#if defined(DEBUG) && defined(WIN32)
		OutputDebugStringA(m_buf_p);
#endif
	}
}

void xr_log::diagnostic(const char* format, ...)
{
	va_list ap;
	va_start(ap, format);
	diagnostic(format, ap);
	va_end(ap);
}

void xr_log::fatal(const char* message, const char* file, unsigned line)
{
	diagnostic("[bug] %s at %s:%u", message, file, line);
//	MessageBoxA(NULL, m_buf, "xray_re", MB_OK);
	std::abort();
}

void xray_re::dbg(const char* format, ...)
{
	va_list ap;
	va_start(ap, format);
	xr_log::instance().diagnostic(format, ap);
	va_end(ap);
}

void xray_re::msg(const char* format, ...)
{
	va_list ap;
	va_start(ap, format);
	xr_log::instance().diagnostic(format, ap);
	va_end(ap);
}

void xray_re::die(const char* message, const char* file, unsigned line)
{
	xr_log::instance().fatal(message, file, line);
}
