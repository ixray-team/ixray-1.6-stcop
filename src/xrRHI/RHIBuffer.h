/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
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
	// A buffer made through NRI that the old backend wraps and holds instead of making one (ID3D11Buffer, ID3D12Resource).
	// D3D12 takes it as read by shaders, the state NRI leaves it in after every write
	void* Adopt = nullptr;
};

class IRHIBuffer
{
public:
	virtual ~IRHIBuffer() = default;

	virtual void AddRef() = 0;
	virtual u32 Release() = 0;
	virtual u32 GetSize() const = 0;

	virtual bool Map(ERHI_BUFFER_MAP MapType, u32 MapFlags, RHIMappedSubresource* pData) = 0;
	virtual void Unmap() = 0;

	virtual void UpdateSubresource(const void* Data, u32 Size, u32 Offset = 0) = 0;

	virtual void SetVertexBuffer(u32 StartSlot, const u32 Stride, const u32 Offset) = 0;
	virtual void SetIndexBuffer(bool Is32BitBuffer, u32 Offset) = 0;
};
