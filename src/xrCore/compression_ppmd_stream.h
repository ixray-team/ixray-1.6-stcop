#pragma once

namespace compression::ppmd
{
	class stream
	{
	private:
		u32 m_buffer_size;
		u8* m_buffer;
		u8* m_pointer;

	public:
		inline			stream(const void* buffer, const u32& buffer_size);
		inline	void	put_char(const u8& object);
		inline	int		get_char();
		inline	void	rewind();
		inline	u8*		buffer() const;
		inline	u32		tell() const;
	};
}

#include "compression_ppmd_stream_inline.h"