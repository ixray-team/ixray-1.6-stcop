#pragma once

// Offset allocator over a fixed [0, capacity) byte range, in the spirit of
// Sebastian Aaltonen's OffsetAllocator: allocations return opaque offsets into
// the managed range (never pointers), frees coalesce with neighbors.
//
// Address-ordered first-fit with splitting and free-neighbor coalescing;
// O(log n) operations. The allocator owns no memory itself - the caller maps
// offsets onto its own storage (e.g. a dedicated GPU buffer).
class XRCORE_API COffsetAllocator
{
public:
	static constexpr u32 InvalidOffset = 0xFFFFFFFFu;

	struct SStats
	{
		u32 Capacity;
		u32 Used;
		u32 Free;
		u32 LargestFreeBlock;
		u32 FreeBlockCount;
		u32 AllocationCount;
	};

	void	Init				(u32 capacityBytes);
	void	Reset				();
	bool	IsInitialized		() const { return CapacityBytes != 0; }

	// Returns an aligned offset or InvalidOffset on exhaustion / bad size.
	u32		Alloc				(u32 sizeBytes, u32 alignment = 16);
	// Unknown/invalid offsets are logged and ignored (never corrupt state).
	void	Free				(u32 offset);
	bool	Owns				(u32 offset) const;

	SStats	GetStats			() const;

private:
	// Invariants: blocks are never adjacent (coalesced on free), never empty.
	xr_map<u32, u32>	FreeBlocks;			// offset -> size
	xr_map<u32, u32>	LiveBlocks;			// offset -> size (for validation on free)
	u32					CapacityBytes = 0;
	u32					UsedBytes = 0;
};
