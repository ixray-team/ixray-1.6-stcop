#include "stdafx.h"
#include "GeometryHeap.h"
#include "PackedVertex.h"

namespace
{
	constexpr u32 Stride = sizeof(PackedVertex);
	constexpr u32 IndexStride = sizeof(u16);
	constexpr u32 MaxBytes = 128u << 20;
	constexpr u32 MaxVertices = MaxBytes / Stride;
	constexpr u32 MaxIndices = MaxBytes / IndexStride;
	constexpr u32 DefaultVertices = (32u << 20) / Stride;
	constexpr u32 DefaultIndices = 16 * DefaultVertices;
	constexpr u32 ArenaKeep = 4u << 20;
	constexpr u32 VertexSlot = ShaderBindings::Texture("g_Vertices0");
	static_assert(ShaderBindings::Texture("g_Vertices4") + GeometryHeap::MaxPages - 1 == VertexSlot);
}

bool GeometryHeap::Take(xr_vector<Range>& Free, u32 Size, u32& Offset)
{
	const auto It = std::find_if(Free.begin(), Free.end(), [Size](const Range& Item) { return Item.Size >= Size; });
	if (It == Free.end())
	{
		Offset = 0;
		return !Size;
	}
	Offset = It->Offset;
	It->Offset += Size;
	It->Size -= Size;
	if (!It->Size)
	{
		Free.erase(It);
	}
	return true;
}

void GeometryHeap::Give(xr_vector<Range>& Free, u32 Offset, u32 Size)
{
	if (!Size)
	{
		return;
	}
	auto It = std::lower_bound(Free.begin(), Free.end(), Offset, [](const Range& Item, u32 Value) { return Item.Offset < Value; });
	if (It != Free.begin() && std::prev(It)->Offset + std::prev(It)->Size == Offset)
	{
		--It;
		It->Size += Size;
	}
	else
	{
		It = Free.insert(It, { Offset, Size });
	}
	const auto Next = std::next(It);
	if (Next != Free.end() && It->Offset + It->Size == Next->Offset)
	{
		It->Size += Next->Size;
		Free.erase(Next);
	}
}

void GeometryHeap::Destroy()
{
	xrCriticalSectionGuard Guard(Lock);
	for (u32 Page = 0; Page < Opened; ++Page)
	{
		HeapPage& Item = Pages[Page];
		_RELEASE(Item.View);
		_RELEASE(Item.Vertices);
		_RELEASE(Item.Indices);
		Item = {};
	}
	Opened = 0;
	Pending = false;
	ReservedVertices = ReservedIndices = 0;
	Copies = {};
	Arena = {};
	Staging = {};
}

void GeometryHeap::Trim()
{
	xrCriticalSectionGuard Guard(Lock);
	for (u32 Page = 0; Page < Opened; ++Page)
	{
		HeapPage& Item = Pages[Page];
		const bool Empty = Item.FreeVertices.size() == 1 && Item.FreeVertices[0].Size == Item.VertexCapacity && Item.FreeIndices.size() == 1 && Item.FreeIndices[0].Size == Item.IndexCapacity;
		if (!Item.Vertices || !Empty)
		{
			continue;
		}
		std::erase_if(Copies, [&Item](const PendingCopy& Copy) { return Copy.Buffer == Item.Vertices || Copy.Buffer == Item.Indices; });
		_RELEASE(Item.View);
		_RELEASE(Item.Vertices);
		_RELEASE(Item.Indices);
		Item = {};
	}
	u32 Count = Opened;
	while (Count && !Pages[Count - 1].Vertices)
	{
		--Count;
	}
	Opened = Count;
	Pending = !Copies.empty();
}

void GeometryHeap::Reserve(u32 Vertices, u32 Indices)
{
	xrCriticalSectionGuard Guard(Lock);
	ReservedVertices = Vertices;
	ReservedIndices = Indices;
}

u32 GeometryHeap::OpenPage(u32 Vertices, u32 Indices)
{
	u32 Page = 0;
	while (Page < Opened && Pages[Page].Vertices)
	{
		++Page;
	}
	if (Page == MaxPages)
	{
		FATAL(make_string<const char*>("GeometryHeap: all %u pages are full, %u vertices and %u indices do not fit", MaxPages, Vertices, Indices));
	}
	HeapPage& Item = Pages[Page];
	const u32 Scale = 1u << std::min(Page, 2u);
	Item.VertexCapacity = std::min(std::max(Vertices, ReservedVertices ? ReservedVertices : DefaultVertices * Scale), MaxVertices);
	Item.IndexCapacity = std::min(std::max(Indices, ReservedIndices ? ReservedIndices : DefaultIndices * Scale), MaxIndices);
	ReservedVertices = ReservedIndices = 0;

	RHIBufferDesc Desc = {};
	Desc.Size = Item.VertexCapacity * Stride;
	Desc.Usage = ERHI_USAGE::USAGE_DEFAULT;
	Desc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
	Desc.StructureByteStride = Stride;
	Item.Vertices = GRHI->CreateBuffer(Desc);

	RHIShaderResourceViewDesc View = {};
	View.ElementWidth = Item.VertexCapacity;
	Item.View = GRHI->CreateShaderResourceView(Item.Vertices, &View);

	RHIUtils::CreateIndexBuffer(&Item.Indices, nullptr, Item.IndexCapacity * IndexStride);
	Item.FreeVertices = { { 0, Item.VertexCapacity } };
	Item.FreeIndices = { { 0, Item.IndexCapacity } };
	Msg("* Geometry page %u: %u vertices, %u indices", Page, Item.VertexCapacity, Item.IndexCapacity);
	if (Page == Opened)
	{
		++Opened;
	}
	return Page;
}

GeometryAlloc GeometryHeap::Alloc(u32 Vertices, u32 Indices)
{
	if (Vertices > MaxVertices || Indices > MaxIndices)
	{
		FATAL(make_string<const char*>("GeometryHeap: %u vertices and %u indices exceed a page (%u vertices, %u indices)", Vertices, Indices, MaxVertices, MaxIndices));
	}
	const u32 IndexSpan = (Indices + 1) & ~1u;
	GeometryAlloc Result = { 0, 0, Vertices, 0, Indices };
	xrCriticalSectionGuard Guard(Lock);
	for (; Result.Page < Opened; ++Result.Page)
	{
		HeapPage& Item = Pages[Result.Page];
		if (Item.Vertices && Take(Item.FreeVertices, Vertices, Result.VertexBase))
		{
			if (Take(Item.FreeIndices, IndexSpan, Result.IndexBase))
			{
				break;
			}
			Give(Item.FreeVertices, Result.VertexBase, Vertices);
		}
	}
	if (Result.Page == Opened)
	{
		Result.Page = OpenPage(Vertices, IndexSpan);
		Take(Pages[Result.Page].FreeVertices, Vertices, Result.VertexBase);
		Take(Pages[Result.Page].FreeIndices, IndexSpan, Result.IndexBase);
	}
	ReservedVertices -= std::min(ReservedVertices, Vertices);
	ReservedIndices -= std::min(ReservedIndices, IndexSpan);
	if (FreeRefs.empty())
	{
		Refs.resize(std::max<size_t>(Refs.size(), 1));
		FreeRefs.push_back(u32(Refs.size()));
		Refs.push_back(0);
	}
	Result.Ref = FreeRefs.back();
	FreeRefs.pop_back();
	Refs[Result.Ref] = 1;
	return Result;
}

GeometryAlloc GeometryHeap::AllocPositions(const Fvector* Positions, u32 Count, const u16* Indices, u32 IndexCount)
{
	xr_vector<PackedVertex> Vertices(Count, PackedVertex{});
	for (u32 Index = 0; Index < Count; ++Index)
	{
		Vertices[Index].P = Positions[Index];
	}
	const GeometryAlloc Block = Alloc(Count, IndexCount);
	Upload(Block, 0, Vertices.data(), Count);
	UploadIndices(Block, 0, Indices, IndexCount);
	return Block;
}

void GeometryHeap::Retain(const GeometryAlloc& Block)
{
	if (Block.Ref)
	{
		xrCriticalSectionGuard Guard(Lock);
		VERIFY(Block.Ref < Refs.size() && Refs[Block.Ref]);
		++Refs[Block.Ref];
	}
}

void GeometryHeap::Free(GeometryAlloc& Block)
{
	if (!Block.Ref)
	{
		return;
	}
	xrCriticalSectionGuard Guard(Lock);
	VERIFY(Block.Ref < Refs.size() && Refs[Block.Ref]);
	if (--Refs[Block.Ref] == 0)
	{
		FreeRefs.push_back(Block.Ref);
		HeapPage& Item = Pages[Block.Page];
		const u32 IndexSpan = (Block.IndexCount + 1) & ~1u;
		const auto Overlaps = [](const PendingCopy& Copy, IRHIBuffer* Buffer, u32 Offset, u32 Size)
		{
			return Size && Copy.Buffer == Buffer && Copy.Offset < Offset + Size && Offset < Copy.Offset + Copy.Size;
		};
		std::erase_if(Copies, [&](const PendingCopy& Copy)
		{
			return Overlaps(Copy, Item.Vertices, Block.VertexBase * Stride, Block.VertexCount * Stride) || Overlaps(Copy, Item.Indices, Block.IndexBase * IndexStride, IndexSpan * IndexStride);
		});
		Give(Item.FreeVertices, Block.VertexBase, Block.VertexCount);
		Give(Item.FreeIndices, Block.IndexBase, IndexSpan);
		Pending = !Copies.empty();
	}
	Block = {};
}

void GeometryHeap::Queue(IRHIBuffer* Buffer, u32 Offset, const void* Data, u32 Size)
{
	if (!Size)
	{
		return;
	}
	const u8* Bytes = static_cast<const u8*>(Data);
	xrCriticalSectionGuard Guard(Lock);
	Copies.push_back({ Buffer, Offset, Size, u32(Arena.size()) });
	Arena.insert(Arena.end(), Bytes, Bytes + Size);
	Pending = true;
}

void GeometryHeap::Upload(const GeometryAlloc& Block, u32 FirstVertex, const PackedVertex* Vertices, u32 Count)
{
	VERIFY(FirstVertex + Count <= Block.VertexCount);
	Queue(Pages[Block.Page].Vertices, (Block.VertexBase + FirstVertex) * Stride, Vertices, Count * Stride);
}

void GeometryHeap::UploadIndices(const GeometryAlloc& Block, u32 FirstIndex, const u16* Indices, u32 Count)
{
	VERIFY(FirstIndex + Count <= Block.IndexCount);
	Queue(Pages[Block.Page].Indices, (Block.IndexBase + FirstIndex) * IndexStride, Indices, Count * IndexStride);
}

void GeometryHeap::Flush()
{
	xrCriticalSectionGuard Guard(Lock);
	std::stable_sort(Copies.begin(), Copies.end(), [](const PendingCopy& A, const PendingCopy& B) { return A.Buffer != B.Buffer ? A.Buffer < B.Buffer : A.Offset < B.Offset; });
	for (size_t First = 0, Last = 0; First < Copies.size(); First = Last)
	{
		const PendingCopy& Head = Copies[First];
		u32 End = Head.Offset + Head.Size;
		for (Last = First + 1; Last < Copies.size() && Copies[Last].Buffer == Head.Buffer && Copies[Last].Offset == End; ++Last)
		{
			End += Copies[Last].Size;
		}
		if (Last - First == 1)
		{
			Head.Buffer->UpdateSubresource(Arena.data() + Head.Data, Head.Size, Head.Offset);
			continue;
		}
		Staging.resize(End - Head.Offset);
		for (size_t Index = First; Index < Last; ++Index)
		{
			memcpy(Staging.data() + (Copies[Index].Offset - Head.Offset), Arena.data() + Copies[Index].Data, Copies[Index].Size);
		}
		Head.Buffer->UpdateSubresource(Staging.data(), u32(Staging.size()), Head.Offset);
	}
	Copies.clear();
	Arena.clear();
	Staging.clear();
	if (Arena.capacity() > ArenaKeep || Staging.capacity() > ArenaKeep)
	{
		Arena.shrink_to_fit();
		Staging.shrink_to_fit();
	}
	Pending = false;
}

void GeometryHeap::Bind(GfxCmdList& Cmd, ERHI_SHADER_TYPE Stage)
{
	for (u32 Page = 0, Count = Opened; Page < Count; ++Page)
	{
		if (Pages[Page].View)
		{
			Cmd.SetResource(Stage, VertexSlot - Page, Pages[Page].View);
		}
	}
}
