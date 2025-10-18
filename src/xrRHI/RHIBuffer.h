#pragma once
#include "RHIEnums.h"

struct RHIBufferSubresource
{
	const void* pSysMem;
	u32 SysMemPitch;
	u32 SysMemSlicePitch;
	u32 SysMemSize;
};

struct RHIMappedSubresource
{
	void* pData;
	u32 RowPitch;
	u32 DepthPitch;
};

struct RHIBufferDesc
{
	u32 Size;
	ERHI_USAGE Usage;
	ERHI_BUFFER_TYPE Type;
	/*ERHI_CPU_ACCESS_FLAG*/ int CPUAccessFlags = 0;
	u32 StructureByteStride = 0;
};

class IRHIBuffer
{
public:
	virtual ~IRHIBuffer() = default;

	virtual void AddRef() = 0;
	virtual u32 Release() = 0;

	virtual bool Map(ERHI_BUFFER_MAP MapType, u32 MapFlags, RHIMappedSubresource* pData) = 0;
	virtual void Unmap() = 0;

	virtual void UpdateSubresource(void* pData, u32 Size) = 0;

	virtual void SetVertexBuffer(u32 StartSlot, const u32 Stride, const u32 Offset) = 0;
	virtual void SetIndexBuffer(bool Is32BitBuffer, u32 Offset) = 0;
};
