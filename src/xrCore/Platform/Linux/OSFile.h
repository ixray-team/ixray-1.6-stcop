#pragma once

using errno_t = int;

#define _MAX_PATH PATH_MAX + 1
#define MAX_PATH PATH_MAX + 1

#define _lseek64 lseek64
#define _O_RDONLY O_RDONLY
#define _open open
#define _fdopen fdopen
#define _wfdopen fdopen
#define _wopen open
#define _close close
#define _write write
#define _read read
#define _lseeki64 lseek64
#define _lseek lseek
#define _wsopen_s _sopen_s

#define _O_WRONLY O_WRONLY
#define _O_RDONLY O_RDONLY
#define _O_TRUNC O_TRUNC
#define _O_CREAT O_CREAT
#define _S_IWRITE S_IWRITE
#define _S_IREAD S_IREAD
#define _O_BINARY 0
#define O_BINARY 0
#define O_SEQUENTIAL 0
#define SH_DENYWR 0
#define _SH_DENYNO 0

using __int64 = int64_t;
using __time64_t = __int64;
using __time32_t = long;
using _fsize_t =  unsigned long;

inline bool _sopen_s(int* handle, const char* FileName, ...)
{
    auto NewHandle = open(FileName, O_RDONLY);
    *handle = NewHandle;
    return NewHandle == -1;
}

inline errno_t fopen_s(FILE **f, const char *name, const char *mode) 
{
    errno_t ret = 0;
    //assert(f);
    *f = fopen(name, mode);
    /* Can't be sure about 1-to-1 mapping of errno and MS' errno_t */
    if (!*f)
        ret = errno;
    return ret;
}

inline int _mkdir(const char *dir)
{
    return mkdir(dir, S_IRWXU);
}

#define _wfopen_s fopen_s

#define _A_HIDDEN 0x02
#define _A_SUBDIR 0x00000010

inline int _filelength(int fd)
{
    struct stat file_info;
    ::fstat(fd, &file_info);
    return file_info.st_size;
}

inline void _splitpath(const char* path, // Path Input
                       char* drive, // Drive     : Output
                       char* dir, // Directory : Output
                       char* fname, // Filename  : Output
                       char* ext // Extension : Output
)
{
    if(!path)
        return;

    const char *p, *end;

    if(drive)
        strcpy(drive, "");

    end = NULL;
    for(p = path; *p; p++)
        if(*p == '/' || *p == '\\')
            end = p + 1;

    if(end)
    {
        if(dir)
        {
            memcpy(dir, path, end - path);
            dir[end - path] = 0;
        }
        path = end;
    }
    else if(dir)
        dir[0] = 0;

    end = strchr(path, '.');

    if(!end)
        end = p;

    if(fname)
    {
        memcpy(fname, path, end - path);
        fname[end - path] = 0;
    }

    if(ext)
        strcpy(ext, end);
}

using FileHandle = int;

class FS_Path;
namespace Platform
{
    IC const char* ValidPath(const char* In)
    {
        static std::string NewPath;
        NewPath = In;

        std::replace(NewPath.begin(), NewPath.end(), '\\', '/');
        return NewPath.c_str();
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
        char result[PATH_MAX];
        ssize_t count = readlink("/proc/self/exe", result, PATH_MAX);
        if (count == -1)
        {
            return {};
        }
        return std::filesystem::path(std::string(result, count)).parent_path();
    }

    IC std::string GetModuleName() 
    {
        char result[PATH_MAX];
        ssize_t count = readlink("/proc/self/exe", result, PATH_MAX);
        if (count == -1) 
        {
            return {};
        }
        return std::string(result, count);
    }
	
    IC std::string GetModuleNameForAddress(uintptr_t address) 
    {
        Dl_info dl_info;
        if (dladdr((void*)address, &dl_info) && dl_info.dli_fname) 
        {
            return std::string(dl_info.dli_fname);
        }
        return {};
    }
	
    IC std::string GetUsrName() 
    {
        char UserName[LOGIN_NAME_MAX];
        if (getlogin_r(UserName, sizeof(UserName)) == 0) 
        {
            return std::string(UserName);
        }
        return {};
    }

    IC std::string GetCompName() 
    {
        char CompName[HOST_NAME_MAX];
        if (gethostname(CompName, sizeof(CompName)) == 0) 
        {
            return std::string(CompName);
        }
        return {};
    }

    IC bool OpenFileWnd(char* buffer, size_t sz_buf, FS_Path* P, int start_flt_ext, char flt[1024], LPCSTR offset, bool bMulti)
    {
        return true;
    }

    IC FileHandle CreateFile(const xr_special_char* FilePath, [[maybe_unused]] bool SharedWrite)
    {
        const char* Path = ValidPath(FilePath);

        auto FlagsLeft = O_RDONLY | O_CREAT;
        auto FlagsRight = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;

        auto FileSource = open(Path, FlagsLeft, FlagsRight);
        //R_ASSERT3(FileSource != -1, Path, "Error file create");
        return FileSource;
    }

    IC FileHandle OpenFile(const xr_special_char* FilePath)
    {
        const char* Path = ValidPath(FilePath);
        auto Handle = open(Path, O_RDONLY);

        return Handle;
    }

    IC void CloseFile(FileHandle Src)
    {
        close(Src);
    }

    IC size_t GetFileSize(FileHandle Src)
    {
        struct stat file_info;
        ::fstat(Src, &file_info);
        return file_info.st_size;
    }

    IC FileHandle CreateMapData(FileHandle hSrcFile, [[maybe_unused]] bool ReadOnly = false)
    {
        return hSrcFile;
    }

    IC void* MapFile(FileHandle hSrcFile, size_t Size, bool bRead = false, size_t Offset = 0)
    {
        auto flag = PROT_READ;

        if (!bRead)
            flag |= PROT_WRITE;

        auto Ptr = mmap(0, Size, flag, MAP_SHARED, hSrcFile, Offset);
        if (Ptr == MAP_FAILED)
        {
            std::string error = strerror(errno);
            return nullptr;
        }
        return Ptr;
    }

    IC void UnmapFile(void* Ptr, size_t Size)
    {
        munmap(Ptr, Size);
    }

    IC size_t GetPageSize()
    {
        return sysconf(_SC_PAGE_SIZE);
    }

    inline int Unlink(const char *path)
    {
        const char* conv_fn = ValidPath(path);
        int result = unlink(conv_fn);
        return result;
    }
}

inline int _rmdir(const char *path)
{
    const char* conv_fn = Platform::ValidPath(path);
    int result = rmdir(conv_fn);
    return result;
}

inline void _splitpath_s(
    const char * path,
    char * drive,
    [[maybe_unused]] size_t driveNumberOfElements,
    char * dir,
    [[maybe_unused]] size_t dirNumberOfElements,
    char * fname,
    [[maybe_unused]] size_t nameNumberOfElements,
    char * ext,
    [[maybe_unused]] size_t extNumberOfElements)
{
    _splitpath(path, drive, dir, fname, ext);
}