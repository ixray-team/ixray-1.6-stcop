#include <string>
#include <cstdlib>
#include <sys/types.h>
#include <sys/stat.h>
#include "xr_file_system_win32.h"
#include "xr_string_utils.h"

using namespace xray_re;

bool xr_file_system::create_folder(const char* path) const
{
	if (read_only()) {
		dbg("fs_ro: creating folder %s", path);
		return true;
	}
	return CreateDirectoryA(path, NULL) != 0 ||
			GetLastError() == ERROR_ALREADY_EXISTS;
}

bool xr_file_system::create_path(const char* path) const
{
	if (read_only()) {
		dbg("fs_ro: creating path %s", path);
		return true;
	}
	char* temp = _strdup(path);
	bool done = false;
	for (char* p = temp; *p != 0; ++p) {
		if (*p != '\\')
			continue;
		*p = 0;
		DWORD attrs = GetFileAttributesA(temp);
		if (attrs == INVALID_FILE_ATTRIBUTES) {
			if (CreateDirectoryA(temp, NULL) == 0 &&
					GetLastError() != ERROR_ALREADY_EXISTS) {
				goto out;
			}
		} else if ((attrs & FILE_ATTRIBUTE_DIRECTORY) == 0) {
			goto out;
		}
		*p = '\\';
	}
	done = CreateDirectoryA(temp, NULL) != 0 || GetLastError() == ERROR_ALREADY_EXISTS;
out:
	free(temp);
	return done;
}

void xr_file_system::split_path(const char* path, std::string* folder,
		std::string* name, std::string* extension)
{
	char _drive[_MAX_DRIVE];
	char _dir[_MAX_DIR];
	char _name[_MAX_FNAME];
	char _extension[_MAX_EXT];
#if defined(_MSC_VER) && _MSC_VER >= 1400
	errno_t err = _splitpath_s(path,
			_drive, sizeof(_drive),
			_dir, sizeof(_dir),
			_name, sizeof(_name),
			_extension, sizeof(_extension));
	xr_assert(!err);
#else
	_splitpath(path, _drive, _dir, _name, _extension);
#endif
	if (folder) {
		folder->assign(_drive);
		folder->append(_dir);
		xr_strlwr(*folder);
	}
	if (name) {
		name->assign(_name);
		xr_strlwr(*name);
	}
	if (extension) {
		extension->assign(_extension);
		xr_strlwr(*extension);
	}
}

bool xr_file_system::folder_exist(const char* path)
{
	DWORD attrs = GetFileAttributesA(path);
	if (attrs == INVALID_FILE_ATTRIBUTES || (attrs & FILE_ATTRIBUTE_DIRECTORY) == 0)
		return false;
	return true;
}

bool xr_file_system::file_exist(const char* path)
{
	DWORD attrs = GetFileAttributesA(path);
	if (attrs == INVALID_FILE_ATTRIBUTES || (attrs & FILE_ATTRIBUTE_DIRECTORY) != 0)
		return false;
	return true;
}

size_t xr_file_system::file_length(const char* path)
{
	HANDLE h = CreateFileA(path, GENERIC_READ, FILE_SHARE_READ, NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (h == INVALID_HANDLE_VALUE)
		return 0;
	size_t length = 0;
	LARGE_INTEGER size64;
	if (GetFileSizeEx(h, &size64) && size64.HighPart == 0)
		length = size64.LowPart;
	CloseHandle(h);
	return length;
}

uint32_t xr_file_system::file_age(const char* path)
{
#if 1
#ifdef _MSC_VER
	struct _stat32 st;
	if (_stat32(path, &st) == 0)
#else
	struct _stat st;
	if (_stat(path, &st) == 0)
#endif
		return uint32_t(st.st_mtime);
	return 0;
#else
	union temp_time {
		FILETIME ft;
		uint64_t nsec;
	};
	HANDLE h = CreateFileA(path, GENERIC_READ, FILE_SHARE_READ, NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (h == INVALID_HANDLE_VALUE)
		return 0;
	uint32_t age = 0;
	temp_time time;
	if (GetFileTime(h, NULL, NULL, &time.ft) != FALSE) {
		SYSTEMTIME st1970;
		st1970.wYear = 1970;
		st1970.wMonth = 1;
		st1970.wDayOfWeek = 0;
		st1970.wDay = 1;
		st1970.wHour = 0;
		st1970.wMinute = 0;
		st1970.wSecond = 0;
		st1970.wMilliseconds = 0;
		temp_time time1970;
		SystemTimeToFileTime(&st1970, &time1970.ft);
		age = uint32_t((time.nsec - time1970.nsec) / 10000000ul);
	}
	CloseHandle(h);
	return age;
#endif
}

bool xr_file_system::copy_file(const char* src_path, const char* tgt_path) const
{
	if (read_only()) {
		dbg("fs_ro: copying %s to %s", src_path, tgt_path);
		return true;
	}
	return CopyFileA(src_path, tgt_path, FALSE) != FALSE;
}

xr_reader* xr_file_system::r_open(const char* path) const
{
	HANDLE h = CreateFileA(path, GENERIC_READ, FILE_SHARE_READ, NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (h == INVALID_HANDLE_VALUE)
		return 0;

	LARGE_INTEGER size64;
	if (!GetFileSizeEx(h, &size64) || size64.HighPart != 0) {
		CloseHandle(h);
		return 0;
	}
	DWORD len = size64.LowPart;

	SYSTEM_INFO si;
	GetSystemInfo(&si);

	xr_reader* r = 0;

	LPVOID data;
	if (len < si.dwAllocationGranularity) {
		DWORD read;
		PBYTE data = (PBYTE)malloc(len);
		if (data != NULL) {
			if (ReadFile(h, data, len, &read, NULL) && read == len) {
				r = new xr_temp_reader(data, len);
			} else {
				free(data);
			}
		}
		CloseHandle(h);
		return r;
	}

	HANDLE h_mmap = CreateFileMapping(h, NULL, PAGE_READONLY, 0, len, NULL);
	if (h_mmap == NULL) {
		CloseHandle(h);
		return 0;
	}

	data = MapViewOfFile(h_mmap, FILE_MAP_READ, 0, 0, len);
	if (data != NULL) {
		r = new xr_mmap_reader_win32(h, h_mmap, data, len);
		if (r)
			return r;
		UnmapViewOfFile(data);
	}

	CloseHandle(h_mmap);
	CloseHandle(h);

	return 0;
}

xr_writer* xr_file_system::w_open(const char* path, bool ignore_ro) const
{
	if (!ignore_ro && read_only()) {
		dbg("fs_ro: writing %s", path);
		return new xr_fake_writer();
	}

	HANDLE h = CreateFileA(path, GENERIC_WRITE, FILE_SHARE_READ, NULL,
			CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	//assert(h != INVALID_HANDLE_VALUE);
	if (h == INVALID_HANDLE_VALUE) {
		if(GetLastError() == ERROR_PATH_NOT_FOUND) {
			std::string folder;
			split_path(path, &folder);
			if (!create_path(folder))
				return 0;
			return w_open(path, ignore_ro);
		}
		else
			return 0;
	}
	xr_writer* w = new xr_file_writer_win32(h);
	if (w == 0)
		CloseHandle(h);
	return w;
}

xr_mmap_reader_win32::xr_mmap_reader_win32(): m_h(INVALID_HANDLE_VALUE), m_h_mmap(INVALID_HANDLE_VALUE) {}

xr_mmap_reader_win32::xr_mmap_reader_win32(HANDLE h, HANDLE h_mmap, const void* data, size_t size):
	m_h(h), m_h_mmap(h_mmap)
{
	m_next = m_p = m_data = static_cast<const uint8_t*>(data);
	m_end = m_data + size;
}

xr_mmap_reader_win32::~xr_mmap_reader_win32()
{
	assert(m_data != 0);
	assert(m_h_mmap != INVALID_HANDLE_VALUE);
	assert(m_h != INVALID_HANDLE_VALUE);
	UnmapViewOfFile(const_cast<uint8_t*>(m_data));
	CloseHandle(m_h_mmap);
	CloseHandle(m_h);
}

xr_file_writer_win32::xr_file_writer_win32(): m_h(INVALID_HANDLE_VALUE) {}

xr_file_writer_win32::xr_file_writer_win32(HANDLE h): m_h(h) {}

xr_file_writer_win32::~xr_file_writer_win32()
{
	CloseHandle(m_h);
}

void xr_file_writer_win32::w_raw(const void* data, size_t size)
{
	DWORD written;
	BOOL ok = WriteFile(m_h, data, DWORD(size & MAXDWORD), &written, NULL);
	xr_assert(ok && size == written);
}

void xr_file_writer_win32::seek(size_t pos)
{
	DWORD new_pos = SetFilePointer(m_h, LONG(pos & MAXLONG), NULL, FILE_BEGIN);
	xr_assert(pos == new_pos);
}

size_t xr_file_writer_win32::tell()
{
	return SetFilePointer(m_h, 0, NULL, FILE_CURRENT);
}
