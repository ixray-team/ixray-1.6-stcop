#pragma once

class CMemAllocPure : public IMemoryAllocator {
public:
	void* alloc(size_t size) override;
	void* realloc(void* p, size_t size) override;
	void free(void* p) override;

	static CMemAllocPure* Create();
};
