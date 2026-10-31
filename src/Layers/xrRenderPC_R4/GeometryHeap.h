#pragma once
#include "RenderAPI.h"
#include <atomic>

struct PackedVertex;
class GfxCmdList;

struct GeometryAlloc
{
	u32 Page;
	u32 VertexBase;
	u32 VertexCount;
	u32 IndexBase;
	u32 IndexCount;
};

class RENDER_API GeometryHeap
{
public:
	static constexpr u32 MaxPages = 5;

	void Destroy();
	void Trim();
	void Reserve(u32 Vertices, u32 Indices);
	GeometryAlloc Alloc(u32 Vertices, u32 Indices);
	GeometryAlloc AllocPositions(const Fvector* Positions, u32 Count, const u16* Indices, u32 IndexCount);
	void Free(const GeometryAlloc& Block);
	void Upload(const GeometryAlloc& Block, u32 FirstVertex, const PackedVertex* Vertices, u32 Count);
	void UploadIndices(const GeometryAlloc& Block, u32 FirstIndex, const u16* Indices, u32 Count);
	void Flush();
	void Bind(GfxCmdList& Cmd);

	bool HasPending() const { return Pending; }
	u32 PageCount() const { return Opened; }
	IRHIBuffer* IndexBuffer(u32 Page) const { return Pages[Page].Indices; }
	IRHIShaderResourceView* VertexView(u32 Page) const { return Pages[Page].View; }

private:
	struct Range
	{
		u32 Offset;
		u32 Size;
	};

	struct HeapPage
	{
		IRHIBuffer* Vertices = nullptr;
		IRHIShaderResourceView* View = nullptr;
		IRHIBuffer* Indices = nullptr;
		u32 VertexCapacity = 0;
		u32 IndexCapacity = 0;
		xr_vector<Range> FreeVertices;
		xr_vector<Range> FreeIndices;
	};

	struct PendingCopy
	{
		IRHIBuffer* Buffer;
		u32 Offset;
		u32 Size;
		u32 Data;
	};

	static bool Take(xr_vector<Range>& Free, u32 Size, u32& Offset);
	static void Give(xr_vector<Range>& Free, u32 Offset, u32 Size);
	u32 OpenPage(u32 Vertices, u32 Indices);
	void Queue(IRHIBuffer* Buffer, u32 Offset, const void* Data, u32 Size);

	xrCriticalSection Lock;
	HeapPage Pages[MaxPages];
	std::atomic<u32> Opened{ 0 };
	std::atomic<bool> Pending{ false };
	u32 ReservedVertices = 0;
	u32 ReservedIndices = 0;
	xr_vector<PendingCopy> Copies;
	xr_vector<u8> Arena;
	xr_vector<u8> Staging;
};
