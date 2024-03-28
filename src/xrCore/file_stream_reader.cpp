#include "stdafx.h"
#include "file_stream_reader.h"

void CFileStreamReader::construct	(LPCSTR file_name, const u32 &window_size)
{
	m_file_handle = Platform::OpenFile(file_name);
	u32 file_size = (u32)Platform::GetFileSize(m_file_handle);

#ifdef IXR_WINDOWS
	HANDLE file_mapping_handle =
		CreateFileMapping(
			m_file_handle,
			0,
			PAGE_READONLY,
			0,
			0,
			0
		);
	VERIFY					(file_mapping_handle != INVALID_HANDLE_VALUE);

	inherited::construct	(file_mapping_handle,0,file_size,file_size,window_size);
#else
    inherited::construct	(m_file_handle,0,file_size,file_size,window_size);
#endif

}

void CFileStreamReader::construct(const FileHandle& file_mapping_handle, const u32& start_offset, const u32& file_size, const u32& archive_size, const u32& window_size)
{
	// XXX: Clang be like
}

void CFileStreamReader::destroy		()
{
#ifdef IXR_WINDOWS
	auto file_mapping_handle = this->file_mapping_handle();
	inherited::destroy();
	CloseHandle	(file_mapping_handle);
#else
    inherited::destroy();
#endif

    Platform::CloseFile(m_file_handle);
}
