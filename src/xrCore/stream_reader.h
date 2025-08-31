#pragma once

class XRCORE_API CStreamReader :
	public IReaderBase
{
private:
	FileHandle	m_file_mapping_handle;
	intptr_t m_start_offset;
	intptr_t m_file_size;
	intptr_t m_archive_size;
	intptr_t m_window_size;

private:
	intptr_t m_current_offset_from_start;
	intptr_t m_current_window_size;
	u8* m_current_map_view_of_file;
	u8* m_start_pointer;
	u8* m_current_pointer;

private:
	void			map(const intptr_t& new_offset);
	IC void			unmap();
	IC void			remap(const intptr_t& new_offset);

private:
	// should not be called
	IC CStreamReader(const CStreamReader& object);
	IC CStreamReader& operator=			(const CStreamReader&);

public:
	IC CStreamReader();

public:
	virtual	void construct(
		const FileHandle& file_mapping_handle,
		const intptr_t start_offset,
		const intptr_t file_size,
		const intptr_t archive_size,
		const intptr_t window_size
	);
	virtual	void			destroy();

public:
	IC		const FileHandle& file_mapping_handle() const;

	IC		intptr_t	elapsed() const override;
	IC		intptr_t	length() const override;
	IC		void		seek(intptr_t offset) override;
	IC		intptr_t	tell() const override;

	IC		void	close();

	void			advance(intptr_t offset) override;
	void			r(void* buffer, intptr_t buffer_size) override;
	CStreamReader* open_chunk(const u32& chunk_id);
	intptr_t		find_chunk(u32 ID, BOOL* bCompressed = 0) override;

public:
	void			r_stringZ(shared_str& dest);
private:
	typedef IReaderBase			inherited;
};

#include "stream_reader_inline.h"