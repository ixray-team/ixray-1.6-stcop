#include "stdafx.h"
#include "stream_reader.h"

void CStreamReader::construct
(
		const FileHandle &file_mapping_handle,
		const intptr_t start_offset,
		const intptr_t file_size,
		const intptr_t archive_size,
		const intptr_t window_size
)
{
	m_file_mapping_handle		= file_mapping_handle;
	m_start_offset				= start_offset;
	m_file_size					= file_size;
	m_archive_size				= archive_size;
	m_window_size				= std::max(window_size,(intptr_t)FS.dwAllocGranularity);

	map							(0);
}

void CStreamReader::destroy()
{
	unmap();
}

void CStreamReader::map(const intptr_t&new_offset)
{
	VERIFY						(new_offset <= m_file_size);
	m_current_offset_from_start	= new_offset;

	u32							granularity = FS.dwAllocGranularity;
	u32							start_offset = m_start_offset + new_offset;
	u32							pure_start_offset = start_offset;
	start_offset				= (start_offset/granularity)*granularity;

	VERIFY						(pure_start_offset >= start_offset);
	u32							pure_end_offset = m_window_size + pure_start_offset;
	u32							end_offset = pure_end_offset/granularity;
	if (pure_end_offset%granularity)
		++end_offset;

	end_offset					*= granularity;
	if (end_offset > m_archive_size)
		end_offset				= m_archive_size;
	
	m_current_window_size		= end_offset - start_offset;
	m_current_map_view_of_file	= (u8*)Platform::MapFile(m_file_mapping_handle, m_current_window_size, true, start_offset);
	m_current_pointer			= m_current_map_view_of_file;

	u32							difference = pure_start_offset - start_offset;
	m_current_window_size		-= difference;
	m_current_pointer			+= difference;
	m_start_pointer				= m_current_pointer;
}

void CStreamReader::advance(intptr_t offset)
{
	VERIFY(m_current_pointer >= m_start_pointer);
	VERIFY(u32(m_current_pointer - m_start_pointer) <= m_current_window_size);
	int							offset_inside_window = int(m_current_pointer - m_start_pointer);
	if (offset_inside_window + offset >= (int)m_current_window_size) {
		remap(m_current_offset_from_start + offset_inside_window + offset);
		return;
	}

	if (offset_inside_window + offset < 0) {
		remap(m_current_offset_from_start + offset_inside_window + offset);
		return;
	}

	m_current_pointer += offset;
}

void CStreamReader::r(void* _buffer, intptr_t buffer_size)
{
	VERIFY(m_current_pointer >= m_start_pointer);
	VERIFY(m_current_pointer - m_start_pointer <= m_current_window_size);

	int offset_inside_window = intptr_t(m_current_pointer - m_start_pointer);
	if (offset_inside_window + buffer_size < m_current_window_size)
	{
		Memory.mem_copy(_buffer, m_current_pointer, buffer_size);
		m_current_pointer += buffer_size;
		return;
	}

	u8* buffer = (u8*)_buffer;
	intptr_t elapsed_in_window = m_current_window_size - u32(m_current_pointer - m_start_pointer);

	do
	{
		Memory.mem_copy(buffer, m_current_pointer, elapsed_in_window);
		buffer += elapsed_in_window;
		buffer_size -= elapsed_in_window;
		advance(elapsed_in_window);

		elapsed_in_window = m_current_window_size;
	} while (m_current_window_size < buffer_size);

	Memory.mem_copy(buffer, m_current_pointer, buffer_size);
	advance(buffer_size);
}

CStreamReader* CStreamReader::open_chunk(u32 chunk_id)
{
	bool compressed;
	intptr_t size = find_chunk(chunk_id, &compressed);
	if (!size)
		return(nullptr);

	R_ASSERT2(!compressed, "cannot use CStreamReader on compressed chunks");
	CStreamReader* result = new CStreamReader();
	result->construct(file_mapping_handle(), m_start_offset + tell(), size, m_archive_size, m_window_size);
	return (result);
}

#include "FS_impl.h"
intptr_t CStreamReader::find_chunk(u32 ID, bool* bCompressed)
{
	return inherited::find_chunk(ID, bCompressed);
}

void CStreamReader::r_stringZ(shared_str& dest)
{
    xr_string result;
    for (;;)
    {
        const intptr_t remaining = elapsed();
        R_ASSERT2(remaining > 0, "Unterminated string in stream");
        if (remaining <= 0)
            return;

        const intptr_t available = std::min(remaining,
            m_current_window_size - (m_current_pointer - m_start_pointer));
        const auto* terminator = static_cast<const u8*>(
            memchr(m_current_pointer, 0, static_cast<size_t>(available)));
        if (terminator)
        {
            if (result.empty())
                dest = reinterpret_cast<const char*>(m_current_pointer);
            else
            {
                result.append(reinterpret_cast<const char*>(m_current_pointer),
                    static_cast<size_t>(terminator - m_current_pointer));
                dest = result.c_str();
            }
            m_current_pointer += terminator - m_current_pointer + 1;
            return;
        }

        result.append(reinterpret_cast<const char*>(m_current_pointer),
            static_cast<size_t>(available));
        if (available == remaining)
        {
            m_current_pointer += available;
            R_ASSERT2(false, "Unterminated string in stream");
            return;
        }
        advance(available);
    }
}
