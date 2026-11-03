#pragma once

class CMemAllocMimalloc :
	public IMemoryAllocator
{
public:
	void* alloc(size_t size) override;
	void* realloc(void* p, size_t size) override;
	void free(void* p) override;

	static CMemAllocMimalloc* Create();
	static void Compact();
	static u32 Usage(u32* BlocksUsed, u32* BlocksFree);
};
