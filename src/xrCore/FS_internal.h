#pragma once

#include "lzhuf.h"
#include <fcntl.h>

void*			FileDownload	(LPCSTR fn, u32* pdwSize=nullptr);
void			FileCompress	(const char *fn, const char* sign, void* data, u32 size);
void * 			FileDecompress	(const char *fn, const char* sign, u32* size=nullptr);

class CFileWriter : public IWriter
{
private:
	FILE* hf;
public:
	CFileWriter (const char *name, bool exclusive)
	{
		R_ASSERT	(name && name[0]);
		fName		= name;
		const xr_special_char* wName = Platform::ANSI_TO_TCHAR_U8(name);
		VerifyPath	(*fName);
        if (exclusive){
    		int handle	= _wopen(wName,_O_WRONLY|_O_TRUNC|_O_CREAT|_O_BINARY,SH_DENYWR);

#ifdef IXR_WINDOWS
    		if (handle==-1)
    			Msg	("!Can't create file: '%s'. Error: '%s'.",*fName,_sys_errlist[errno]);
#endif
    		hf		= _wfdopen(handle,TEXT("wb"));
        }else{
			_wfopen_s(&hf, wName, TEXT("wb"));
			if (hf==0)
            {
                string1024 error;
                xr_strerror(errno, error, sizeof(error));
                Msg("!Can't write file: '%s'. Error: '%s'.", *fName, error);
            }
		}
	}

	virtual ~CFileWriter()
	{
		if (0 != hf)
		{
			fclose(hf);
#ifdef IXR_WINDOWS
			// release RO attrib
            xr_special_char* wName = Platform::ANSI_TO_TCHAR_U8(*fName);
			DWORD dwAttr = GetFileAttributes(wName);

			if ((dwAttr != u32(-1)) && (dwAttr & FILE_ATTRIBUTE_READONLY)) {
				dwAttr &= ~FILE_ATTRIBUTE_READONLY;
				SetFileAttributes(wName, dwAttr);
			}
#endif
		}
	}
	// kernel
	virtual void	w			(const void* _ptr, u32 count) 
    { 
		if ((0!=hf) && (0!=count)){
			const u32 mb_sz = 0x1000000;
			u8* ptr 		= (u8*)_ptr;
			int req_size;
            string1024 error;

			for (req_size = count; req_size>mb_sz; req_size-=mb_sz, ptr+=mb_sz)
            {
				size_t W = fwrite(ptr,mb_sz,1,hf);
                xr_strerror(errno, error, sizeof(error));
				R_ASSERT3(W==1,"Can't write mem block to file. Disk maybe full.", error);
			}

			if (req_size)
            {
				size_t W = fwrite(ptr,req_size,1,hf);
                xr_strerror(errno, error, sizeof(error));
				R_ASSERT3(W==1,"Can't write mem block to file. Disk maybe full.", error);
			}
		}
    };
	virtual void	seek		(u32 pos)	{	if (0!=hf) fseek(hf,pos,SEEK_SET);		};
	virtual u32		tell		()			{	return (0!=hf)?ftell(hf):0;				};
	virtual bool	valid		()			{	return (0!=hf);}
	virtual	void	flush		()			{	if (hf)	fflush(hf);						};
};

// It automatically frees memory after destruction
class CTempReader : public IReader
{
public:
				CTempReader(void *_data, int _size, int _iterpos) : IReader(_data,_size,_iterpos)	{}
	virtual		~CTempReader();
};
class CPackReader : public IReader
{
	void*		base_address;
public:
				CPackReader(void* _base, void* _data, int _size) : IReader(_data,_size){base_address=_base;}
	virtual		~CPackReader();
};
class XRCORE_API CFileReader : public IReader
{
public:
				CFileReader(const char *name);
	virtual		~CFileReader();
};
class CCompressedReader : public IReader
{
public:
				CCompressedReader(const char *name, const char *sign);
	virtual		~CCompressedReader();
};
class CVirtualFileReader : public IReader
{
private:
    FileHandle hSrcFile;
    FileHandle hSrcMap;
public:
				CVirtualFileReader(const char *cFileName);
	virtual		~CVirtualFileReader();
};