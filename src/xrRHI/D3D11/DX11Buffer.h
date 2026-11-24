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
#include <d3d11.h>
#include "RHI.h"

u32 GetD3D11CPUAccess(ERHI_CPU_ACCESS_FLAG flag);

class CD3D11Buffer : 
	public IRHIBuffer
{
public:
	CD3D11Buffer();
	~CD3D11Buffer();

	void Create(const RHIBufferDesc& desc = {}, const RHIBufferSubresource* pSubresource = nullptr);

	bool Map(ERHI_BUFFER_MAP MapType, u32 MapFlags, RHIMappedSubresource* pData) override;
	void Unmap() override;

	void UpdateSubresource(const void* Data, u32 Size, u32 Offset = 0) override;

	void SetVertexBuffer(u32 StartSlot, const u32 Stride, const u32 Offset) override;
	void SetIndexBuffer(bool Is32BitBuffer, u32 Offset) override;

	void AddRef() override;
	u32 Release() override;
	u32 GetSize() const override { return m_bufferDesc.Size; }

	ID3D11Buffer* GetD3DObject();

private:
	ID3D11Buffer* m_pBuffer;
	RHIBufferDesc m_bufferDesc;
	std::atomic<u32> References = 1;
};