#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_FILE_SYSTEM_WIN32_H__
#define __XR_FILE_SYSTEM_WIN32_H__

#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#include "xr_file_system.h"

namespace xray_re {

class xr_mmap_reader_win32: public xr_reader {
public:
			xr_mmap_reader_win32();
			xr_mmap_reader_win32(HANDLE h, HANDLE h_mmap, const void* data, size_t size);
	virtual		~xr_mmap_reader_win32();

private:
	HANDLE		m_h;
	HANDLE		m_h_mmap;
};

class xr_file_writer_win32: public xr_writer {
public:
			xr_file_writer_win32();
			xr_file_writer_win32(HANDLE h);
	virtual		~xr_file_writer_win32();
	virtual void	w_raw(const void* src, size_t src_size);
	virtual void	seek(size_t pos);
	virtual size_t	tell();

private:
	HANDLE		m_h;
};

} // end of namespace xray_re

#endif
