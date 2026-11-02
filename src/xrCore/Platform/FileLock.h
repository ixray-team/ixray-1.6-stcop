#pragma once

#include "Platform.h"
#include <filesystem>
#ifndef IXR_WINDOWS
#include <fcntl.h>
#include <sys/file.h>
#include <unistd.h>
#endif

namespace Platform
{
class FileLock final
{
public:
	explicit FileLock(const std::filesystem::path& Path)
	{
		std::error_code Error;
		std::filesystem::create_directories(Path.parent_path(), Error);
		if (Error)
		{
			return;
		}
#ifdef IXR_WINDOWS
		Handle = ::CreateFileW(Path.c_str(), GENERIC_READ | GENERIC_WRITE, 0, nullptr, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
#else
		Descriptor = open(Path.c_str(), O_CREAT | O_RDWR | O_CLOEXEC, 0600);
		if (Descriptor >= 0 && flock(Descriptor, LOCK_EX | LOCK_NB) != 0)
		{
			close(Descriptor);
			Descriptor = -1;
		}
#endif
	}
	~FileLock()
	{
#ifdef IXR_WINDOWS
		if (IsLocked())
		{
			::CloseHandle(Handle);
		}
#else
		if (Descriptor >= 0)
		{
			close(Descriptor);
		}
#endif
	}
	FileLock(const FileLock&) = delete;
	FileLock& operator=(const FileLock&) = delete;
	bool IsLocked() const
	{
#ifdef IXR_WINDOWS
		return Handle != INVALID_HANDLE_VALUE;
#else
		return Descriptor >= 0;
#endif
	}

private:
#ifdef IXR_WINDOWS
	void* Handle = INVALID_HANDLE_VALUE;
#else
	int Descriptor = -1;
#endif
};
} // namespace Platform
