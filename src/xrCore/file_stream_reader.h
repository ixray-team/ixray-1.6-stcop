#pragma once
#include "stream_reader.h"

class CFileStreamReader : public CStreamReader
{
private:
	using inherited = CStreamReader;

private:
	FileHandle m_file_handle;

public:
	virtual void			construct		(LPCSTR file_name, const u32 &window_size);

	virtual	void			construct(
		const FileHandle& file_mapping_handle,
		const u32& start_offset,
		const u32& file_size,
		const u32& archive_size,
		const u32& window_size
	);

	virtual	void			destroy			();
};