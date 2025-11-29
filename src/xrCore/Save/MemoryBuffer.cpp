#include "stdafx.h"
#include "MemoryBuffer.h"


bool CMemoryChunk::CanWrite(size_t size)
{
	return count + size < ChunkSize;
}

bool CMemoryChunk::Write(const void* data, size_t size)
{
	VERIFY(size < CMemoryChunk::ChunkSize);
	if (!CanWrite(size)) {
		return false;
	}
	memcpy(this->data + count, data, size);
	count += size;
	return true;
}

CMemoryBuffer::CMemoryBuffer()
{
	Chunks.push_back(new CMemoryChunk());
}

CMemoryBuffer::~CMemoryBuffer()
{
	for (auto elem : Chunks) {
		xr_delete(elem);
	}
}

bool CMemoryBuffer::Write(const void* data, size_t size)
{
	u8* Ptr = (u8*)(data);
	while (size)
	{
		size_t ToWrite = std::min(size, CMemoryChunk::ChunkSize - 1);
		size -= ToWrite;
		if (!Chunks.back()->Write(Ptr, ToWrite)) {
			Chunks.push_back(new CMemoryChunk());
			Chunks.back()->Write(Ptr, ToWrite);
		}
		Ptr += ToWrite;
	}
	return true;
}
