#pragma once

using FileHandle = HANDLE;
class FS_Path;

namespace Platform
{
    IC const char* ValidPath(const char* In)
    {
        return In;
    }

    IC const char* RestorePath(const char* In)
    {
        static std::string NewPath;
        NewPath = In;

        std::replace(NewPath.begin(), NewPath.end(), '/', '\\');
        return NewPath.c_str();
    }

    IC const xr_special_char* ValidPath(const xr_special_char* In)
    {
        return In;
    }

	bool OpenFileWnd(char* buffer, size_t sz_buf, FS_Path* P, int start_flt_ext, char flt[1024], LPCSTR offset, bool bMulti);
	//bool SaveFileWnd(char* buffer, size_t sz_buf, FS_Path* P, int start_flt_ext, char flt[1024], LPCSTR offset);

    IC FileHandle CreateFile(const char* FilePath, bool SharedWrite)
    {
        xr_special_char* Path = ANSI_TO_TCHAR_U8(FilePath);

        auto FlagsLeft = GENERIC_READ;
        auto FlagsRight = FILE_SHARE_READ;
        if (SharedWrite)
        {
            FlagsLeft |= GENERIC_WRITE;
        }
        else
        {
            FlagsRight |= FILE_SHARE_WRITE;
        }

        auto FileSource = ::CreateFile(Path, FlagsLeft, FlagsRight, 0, OPEN_EXISTING, 0, 0);
        //R_ASSERT3(FileSource != INVALID_HANDLE_VALUE, cFileName, Debug.error2string(GetLastError()));
        return FileSource;
    }

    IC FileHandle OpenFile(const char* FilePath)
    {
        const xr_special_char* Path = Platform::ANSI_TO_TCHAR_U8(FilePath);
        auto Handle = ::CreateFile(Path, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);

        return Handle;
    }

    IC void CloseFile(FileHandle Src)
    {
        CloseHandle(Src);
    }

    IC size_t GetFileSize(FileHandle Src)
    {
        return ::GetFileSize(Src, nullptr);
    }

    IC void* MapFile(FileHandle hSrcFile, [[maybe_unused]] size_t Size, bool bRead = false, size_t Offset = 0)
    {
        return MapViewOfFile(hSrcFile, bRead ? FILE_MAP_READ: FILE_MAP_ALL_ACCESS, 0, (DWORD)Offset, 0);
    }

    IC void UnmapFile(void* Ptr, [[maybe_unused]] size_t Size)
    {
        UnmapViewOfFile(Ptr);
    }

    IC size_t GetPageSize()
    {
        SYSTEM_INFO sys_inf;
        GetSystemInfo(&sys_inf);
        return sys_inf.dwAllocationGranularity;
    }

    inline int Unlink(const char *path)
    {
        return _unlink(path);
    }
}