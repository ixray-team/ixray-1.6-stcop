#include "stdafx.h"
#include "FS_internal.h"

//////////////////////////////////////////////////////////////////////
// Tools
//////////////////////////////////////////////////////////////////////
//---------------------------------------------------
void VerifyPath(const char* path)
{
	string1024 tmp;
	for (int i = 0; path[i]; i++) 
	{
		if (path[i] != Platform::kPreferredSeparator[0] || i == 0)
			continue;
		CopyMemory(tmp, path, i);
		tmp[i] = 0;
		_mkdir(tmp);
	}
}

static errno_t open_internal(const char* fn, int& handle)
{
	const char* FileName = Platform::ValidPath(fn);
	return _wsopen_s
			(
				&handle,
				Platform::ANSI_TO_TCHAR_U8(FileName),
				_O_RDONLY | _O_BINARY, _SH_DENYNO, _S_IREAD
			);
}

bool file_handle_internal(const char* file_name, intptr_t& size, int& file_handle)
{
	if (open_internal(file_name, file_handle))
	{
		Sleep(1);
		if (open_internal(file_name, file_handle))
			return (false);
	}

	size = _filelengthi64(file_handle);
	
	return (true);
}

void *FileDownload(const char* file_name, const int &file_handle, intptr_t&file_size)
{
	void *buffer = Memory.mem_alloc(file_size);

	int r_bytes	= _read(file_handle,buffer,file_size);
    R_ASSERT3(static_cast<size_t>(r_bytes) == file_size, "Can't read from file : ", file_name);
    R_ASSERT3(!_close(file_handle), "can't close file : ", file_name);

	return (buffer);
}

void* FileDownload(const char* file_name, intptr_t& buffer_size)
{
	int file_handle;
	bool HandleComplete = file_handle_internal(file_name, buffer_size, file_handle);
	R_ASSERT3(
		HandleComplete,
		"can't open file : ",
		file_name
	);

	return (FileDownload(file_name, file_handle, buffer_size));
}

typedef char MARK[9];
IC void mk_mark(MARK& M, const char* S)
{
	strncpy_s(M,sizeof(M),S,8);
}

void  FileCompress	(const char *fn, const char* sign, void* data, intptr_t size)
{
	MARK M; mk_mark(M,sign);

	int H	= _open(fn,O_BINARY|O_CREAT|O_WRONLY|O_TRUNC,S_IREAD|S_IWRITE);
	R_ASSERT2(H>0,fn);
	_write	(H,&M,8);
	_writeLZ(H,data,(u32)size);
	_close	(H);
}

void* FileDecompress(const char* fn, const char* sign, intptr_t& size)
{
	MARK M, F;
	mk_mark(M, sign);

	int	H = _open(fn, O_BINARY | O_RDONLY);
	R_ASSERT2(H > 0, fn);
	_read(H, &F, 8);

	if (strncmp(M, F, 8) != 0)
	{
		F[8] = 0;		Msg("FATAL: signatures doesn't match, file(%s) / requested(%s)", F, sign);
	}
	R_ASSERT(strncmp(M, F, 8) == 0);

	void* ptr = nullptr;
	intptr_t SZ;
	SZ = _readLZ(H, ptr, _filelength(H) - 8);
	_close(H);
	if (size) size = SZ;
	return ptr;
}

//---------------------------------------------------
// memory
CMemoryWriter::~CMemoryWriter() 
{
	xr_free(data);
}

void CMemoryWriter::w(const void* ptr, u32 count)
{
	if (position + count > mem_size)
	{
		// reallocate
		if (mem_size == 0)
			mem_size = 128;

		while (mem_size <= (position + count))
		{
			mem_size *= 2;
		}

		if (nullptr == data)
		{
			data = (u8*)Memory.mem_alloc(mem_size);
		}
		else
		{
			data = (u8*)Memory.mem_realloc(data, mem_size);
		}
	}

	CopyMemory(data + position, ptr, count);
	position += count;

	if (position > file_size)
	{
		file_size = position;
	}
}

//static const u32 mb_sz = 0x1000000;
bool CMemoryWriter::save_to(const char* fn)
{
	IWriter* F = FS.w_open(fn);
	if (F)
	{
		F->w(pointer(), size());
		FS.w_close(F);
		return 		true;
	}
	return false;
}

void CBufferMemoryWriter::w(const void* ptr, u32 count)
{
	R_ASSERT(position+count <= GetBuffer().size());
	CopyMemory(GetBuffer().data()+position, ptr, count);
	position += count;
}

void IWriter::open_chunk	(u32 type)
{
	w_u32(type);
	chunk_pos.push(tell());
	w_u32(0);	// the place for 'size'
}

void IWriter::close_chunk()
{
	VERIFY(!chunk_pos.empty());
	// se7kills FIXED Memory Leaking (int) convert to (u32) 
	u32 pos = tell();
	u32 SavePosition = pos - chunk_pos.top() - 4;
	seek(chunk_pos.top());
	w_u32(SavePosition);
	seek(pos);
	chunk_pos.pop();
}

u32	IWriter::chunk_size	()					// returns size of currently opened chunk, 0 otherwise
{
	if (chunk_pos.empty())	
		return 0;

	return tell() - chunk_pos.top() - 4;
}

void IWriter::w_compressed(void* ptr, u32 count)
{
	u8*		dest	= nullptr;
	unsigned	dest_sz	= 0;
	_compressLZ	(&dest,&dest_sz,ptr,count);
	
	if (dest && dest_sz)
		w(dest,dest_sz);
	xr_free		(dest);
}

void IWriter::w_chunk(u32 type, void* data, u32 size)
{
	open_chunk(type);
	if (type & CFS_CompressMark)
		w_compressed(data, size);
	else
		w(data, size);

	close_chunk();
}

void IWriter::w_sdir(const Fvector& D)
{
	Fvector C;
	float mag = D.magnitude();
	if (mag > EPS_S)
	{
		C.div(D, mag);
	}
	else
	{
		C.set(0, 0, 1);
		mag = 0;
	}
	w_dir(C);
	w_float(mag);
}

void IWriter::w_printf(const char* format, ...)
{
	va_list mark;
	char buf[1024];

	va_start(mark, format);
	vsprintf(buf, format, mark);
	va_end(mark);

	w(buf, xr_strlen(buf));
}

//---------------------------------------------------
// base stream
IReader* IReader::open_chunk(u32 ID)
{
	bool bCompressed;

	size_t	dwSize = find_chunk(ID, &bCompressed);
	if (dwSize != 0)
	{
		if (bCompressed)
		{
			u8* dest = nullptr;
			unsigned	dest_sz;
			_decompressLZ(&dest, &dest_sz, pointer(), dwSize);
			return new CTempReader(dest, dest_sz, tell() + dwSize);
		}
		else
		{
			return new IReader(pointer(), dwSize, tell() + dwSize);
		}
	}
	
	return nullptr;
}

void IReader::close()
{
	auto pointer = (IReader*)this;
	xr_delete(pointer);
}

#include "FS_impl.h"

intptr_t IReader::find_chunk(u32 ID, bool* bCompressed)
{
	return inherited::find_chunk(ID, bCompressed);
}

IReader* IReader::open_chunk_iterator(u32& ID, IReader* _prev)
{
	if (nullptr == _prev)
	{
		// first
		rewind();
	}
	else
	{
		// next
		seek(_prev->iterpos);
		_prev->close();
	}

	//	open
	if (elapsed() < 8)
		return nullptr;

	ID = r_u32();
	u32 _size = r_u32();
	if (ID & CFS_CompressMark)
	{
		// compressed
		u8* dest = nullptr;
		u32 dest_sz = 0;

		_decompressLZ(&dest, &dest_sz, pointer(), _size);
		return new CTempReader(dest, dest_sz, tell() + _size);
	}
	else
	{
		// normal
		return new IReader(pointer(), _size, tell() + _size);
	}
}

void IReader::r	(void *p, intptr_t cnt)
{
	VERIFY			(Pos+cnt<=Size);
	CopyMemory		(p,pointer(),cnt);
	advance			(cnt);
#ifdef DEBUG
	bool	bShow		= false		;
	if (cast_file_reader())			bShow = true;
	if (cast_virtual_file_reader())	bShow = true;
	if (bShow)			{
  		FS.dwOpenCounter	++		;
	}
#endif
};

IC bool is_term(char a)
{ 
	return (a==13)||(a==10);
}

IC u32 IReader::advance_term_string()
{
	u32 sz = 0;
	char* src = (char*)data;
	while (!eof())
	{
		Pos++;
		sz++;
		if (!eof() && is_term(src[Pos]))
		{
			while (!eof() && is_term(src[Pos]))
				Pos++;
			break;
		}
	}
	return sz;
}

void IReader::r_string(char* dest, u32 tgt_sz)
{
	char* src = (char*)data + Pos;
	u32 sz = advance_term_string();
	R_ASSERT2(sz < (tgt_sz - 1), "Dest string less than needed.");

	strncpy_s(dest, tgt_sz, src, sz);
	dest[sz] = 0;
}

void IReader::r_string(xr_string& dest)
{
	char* src = (char*)data + Pos;
	u32 sz = advance_term_string();
	dest.assign(src, sz);
}

void IReader::r_stringZ(char* dest, u32 tgt_sz)
{
	char* src = (char*)data;
	u32 sz = xr_strlen(src);
	R_ASSERT2(sz < tgt_sz, "Dest string less than needed.");
	while ((src[Pos] != 0) && (!eof())) *dest++ = src[Pos++];
	*dest = 0;
	Pos++;
}

void IReader::r_stringZ(shared_str& dest)
{
	dest = (char*)(data + Pos);
	Pos += dest.size() + 1;
}

void IReader::r_stringZ(xr_string& dest)
{
	dest = (char*)(data + Pos);
	Pos += dest.size() + 1;
}

void IReader::skip_stringZ()
{
	char* src = (char*)data;
	while ((src[Pos] != 0) && (!eof())) Pos++;
	Pos++;
}

//--------------------------------------------------
// temp stream
CTempReader::~CTempReader()
{
	xr_free(data);	
}

//---------------------------------------------------
// pack stream
CPackReader::~CPackReader()
{
	Platform::UnmapFile(base_address, Size);
}

//---------------------------------------------------
// file stream
CFileReader::CFileReader(const char *name)
{
    data	= (char *)FileDownload(name, Size);
    Pos		= 0;
}

CFileReader::~CFileReader()
{
	xr_free(data);
}

//---------------------------------------------------
// compressed stream
CCompressedReader::CCompressedReader(const char *name, const char *sign)
{
    data	= (char *)FileDecompress(name,sign, Size);
    Pos		= 0;
}

CCompressedReader::~CCompressedReader()
{	
	xr_free(data);
}

CVirtualFileRW::CVirtualFileRW(const char* cFileName)
{
	// Open the file
	hSrcFile = Platform::CreateFile(cFileName, true);
	Size = Platform::GetFileSize(hSrcFile);
	hSrcMap = Platform::CreateMapData(hSrcFile, false);

#ifdef IXR_WINDOWS
	R_ASSERT3(hSrcMap != INVALID_HANDLE_VALUE, cFileName, Debug.error2string(GetLastError()));
#endif

	data = (char*)Platform::MapFile(hSrcMap, Size);
	R_ASSERT3(data, cFileName, Debug.error2string(GetLastError()));
}

CVirtualFileRW::~CVirtualFileRW() 
{
    Platform::UnmapFile((void*)data, Size);

#ifdef IXR_WINDOWS
	CloseHandle(hSrcMap);
#endif
	Platform::CloseFile(hSrcFile);
}

void CMultiReader::AppendReader(IReader& reader)
{
	readers.emplace_back(FullSize, &reader);
	FullSize += reader.length();
}

void CMultiReader::advance(intptr_t cnt)
{
	R_ASSERT(Pos + cnt >= 0); // we can move backward, right?
	R_ASSERT(Pos + cnt <= FullSize);
	Pos += cnt;
	VERIFY(reader_index < readers.size());
	if (Pos < readers[reader_index].first)
	{
		do
		{
			VERIFY(reader_index > 0);
			reader_index--;
		} while (Pos < readers[reader_index].first);
	} else
	{
		while (reader_index + 1 < readers.size() && Pos > readers[reader_index + 1].first)
		{
			reader_index++;
		}
	}
	readers[reader_index].second->seek(Pos - readers[reader_index].first);
}

void CMultiReader::r(void* p, intptr_t cnt)
{
	R_ASSERT(cnt > 0); // we can move backward, right?
	R_ASSERT(Pos + cnt <= FullSize);
	auto ElapsedToRead = cnt;
	auto CurPtr = (u8*)p;
	while (ElapsedToRead > 0)
	{
		VERIFY(reader_index < readers.size());
		auto& reader = readers[reader_index];
		auto ToRead = std::min(ElapsedToRead, reader.second->elapsed());
		reader.second->r(CurPtr, ToRead);
		CurPtr += ToRead;
		ElapsedToRead -= ToRead;
		if (reader.second->eof())
		{
			reader_index++;
		}
		if (!IVERIFY(ElapsedToRead >= 0))
		{
			break;
		}
	}
	Pos += cnt;
}

void CMultiReader::seek(intptr_t ptr)
{
	R_ASSERT(Pos >= 0); // we can move backward, right?
	R_ASSERT(Pos <= FullSize);
	Pos = ptr;
	for (reader_index = 0; reader_index < readers.size(); reader_index++)
	{
		if (readers[reader_index].first > Pos)
		{
			reader_index--;
			break;
		}
	}
	readers[reader_index].second->seek(Pos - readers[reader_index].first);
}

CVirtualFileReader::CVirtualFileReader(const char* cFileName)
{
	// Open the file
	hSrcFile = Platform::CreateFile(cFileName, false);
	Size = Platform::GetFileSize(hSrcFile);

	if (Size == 0)
		return;

	hSrcMap = Platform::CreateMapData(hSrcFile, true);

#ifdef IXR_WINDOWS
	R_ASSERT3(hSrcMap != INVALID_HANDLE_VALUE, cFileName, Debug.error2string(GetLastError()));
#endif

	data = (char*)Platform::MapFile(hSrcMap, Size, true);
	R_ASSERT3(data, cFileName, Debug.error2string(GetLastError()));
}

CVirtualFileReader::~CVirtualFileReader() 
{
	if (Size == 0)
	{
		return;
	}

    Platform::UnmapFile((void*)data, Size);

#ifdef IXR_WINDOWS
	CloseHandle(hSrcMap);
#endif

    Platform::CloseFile(hSrcFile);
}
