#include "stdafx.h"
#include "MemoryBuffer.h"


bool CMemoryChunk::CanWrite(size_t size)
{
	return size <= ChunkSize - count;
}

bool CMemoryChunk::Write(const void* data, size_t size)
{
	VERIFY(size <= CMemoryChunk::ChunkSize);
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

size_t CMemoryBuffer::GetOffset(const void* position) const
{
	const auto address = reinterpret_cast<uintptr_t>(position);
	size_t offset = 0;
	for (const auto* chunk : Chunks)
	{
		const auto start = reinterpret_cast<uintptr_t>(chunk->data);
		if (address >= start && address - start < chunk->count)
			return offset + (address - start);
		offset += chunk->count;
	}
	R_ASSERT2(false, "Reserved memory does not belong to this buffer");
	return size_t(-1);
}
