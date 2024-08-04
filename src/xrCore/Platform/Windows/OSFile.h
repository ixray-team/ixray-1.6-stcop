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

    IC std::filesystem::path GetBinaryFolderPath()
    {
        char BinPath[MAX_PATH];
        int bytes = GetModuleFileNameA(NULL, BinPath, sizeof(BinPath));
        if (bytes == 0) 
        {
            return {};
        }
        return std::filesystem::path(BinPath).parent_path();
    }

    IC std::string GetModuleName()
    {
        char ModuleName[MAX_PATH];
        int bytes = GetModuleFileNameA(NULL, ModuleName, sizeof(ModuleName));
        if (bytes == 0) 
        {
            return {};
        }
        return std::string(ModuleName);
    }
	
    IC std::string GetModuleNameForAddress(uintptr_t address) 
    {
        char formatBuff[MAX_PATH] = { 0 };
        HINSTANCE hModule = (HINSTANCE)SymGetModuleBase(GetCurrentProcess(), address);
        if (hModule && GetModuleFileNameA(hModule, formatBuff, sizeof(formatBuff))) 
        {
            return std::string(formatBuff);
        }
        return {};
    }

    IC const xr_special_char* ValidPath(const xr_special_char* In)
    {
        return In;
    }

    IC std::string GetUsrName()
    {
        char UserName[UNLEN + 1];
        DWORD size = sizeof(UserName);
        if (GetUserNameA(UserName, &size)) 
        {
            return std::string(UserName);
        }
        return {};
    }

    IC std::string GetCompName()
    {
        char CompName[MAX_COMPUTERNAME_LENGTH + 1];
        DWORD size = sizeof(CompName);
        if (GetComputerNameA(CompName, &size)) 
        {
            return std::string(CompName);
        }
        return {};
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
        return ::GetFileSize(Src, NULL);
    }

    IC void* CreateMapData(FileHandle hSrcFile, bool ReadOnly = false)
    {
        auto MapFlags = ReadOnly ? PAGE_READONLY : PAGE_READWRITE;
        return CreateFileMapping(hSrcFile, 0, MapFlags, 0, 0, 0);
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

    inline int Unlink(const char* path)
    {
        return _wunlink(Platform::ANSI_TO_TCHAR_U8(path));
    }
}