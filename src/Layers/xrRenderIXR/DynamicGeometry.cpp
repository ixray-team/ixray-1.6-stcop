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
#include "stdafx.h"
#include "DynamicGeometry.h"

// The engine defines Device as a macro; NRI and NriDevice use it as a name
#undef Device
#include "../../xrRHI/Nri/NriDevice.h"

#include <NRI.h>

void DynamicGeometry::Create()
{
	// Its chunks are written while the GPU still reads what the frames before wrote into them
	CHECK_OR_EXIT(GRHI->APILevel == ERHI_API_LAYER::D3D12 || GRHI->SupportsNoOverwriteSRV(), "The video driver cannot write a shader-readable dynamic buffer without discarding it (D3D11 MapNoOverwriteOnDynamicBufferSRV). Update the driver or set renderer_api d3d12");
}

void DynamicGeometry::Destroy()
{
	const nri::CoreInterface& Api = GRHI->Nri->CoreApi();

	for (xr_vector<Chunk>& Frame : Chunks)
	{
		for (const Chunk& Item : Frame)
		{
			Api.DestroyDescriptor(Item.View);
			Api.DestroyBuffer(Item.Buffer);
		}

		Frame.clear();
	}

	Position = 0;
	Uploaded = 0;
}

void* DynamicGeometry::Lock(u32 Count, EVertexFormat LockFormat, u32& Offset)
{
	VERIFY(Count != 0 && Locked == 0);

	// The game's thread is recording its draws for the render thread, and this one went past the recorder
	if (DrawRecorder::IsRecording())
	{
		static bool IsTold = false;

		if (!IsTold)
		{
			IsTold = true;
			LogStackTrace("! A draw of the game's thread is not recorded (DrawRecorder): it is drawn out of its place");
		}
	}

	// A lock larger than a chunk spans several: its draws are not drawn
	if (Count <= ChunkRecords && Position % ChunkRecords + Count > ChunkRecords)
	{
		Position = (Position / ChunkRecords + 1) * ChunkRecords;
	}

	xr_vector<Chunk>& Frame = Chunks[Slot];

	// An empty lock still takes the chunk its offset falls in
	while ((Position + std::max(Count, 1u) - 1) / ChunkRecords >= Frame.size())
	{
		const nri::CoreInterface& Api = GRHI->Nri->CoreApi();
		Chunk Item = {};
		nri::BufferDesc Desc = {};
		Desc.size = u64(ChunkRecords) * sizeof(PackedVertex);
		Desc.structureStride = sizeof(PackedVertex);
		Desc.usage = nri::BufferUsageBits::SHADER_RESOURCE;
		Api.CreateCommittedBuffer(*GRHI->Nri->Device, nri::MemoryLocation::HOST_UPLOAD, 0.0f, Desc, Item.Buffer);
		Api.SetDebugName(Item.Buffer, "Dynamic geometry");

		nri::BufferViewDesc View = {};
		View.buffer = Item.Buffer;
		View.type = nri::BufferView::STRUCTURED_BUFFER;
		View.structureStride = sizeof(PackedVertex);
		Api.CreateBufferView(View, Item.View);
		Frame.push_back(Item);
	}

	Start = Position;
	Locked = Count;
	Format = LockFormat;
	Offset = Start;

	if (Records.size() < Start + Count)
	{
		Records.resize(Start + Count);
	}

	if (Format == EVertexFormat::Packed)
	{
		return Records.data() + Start;
	}

	Scratch.resize(std::max<size_t>(Scratch.size(), size_t(Count) * VertexStride(Format)));
	return Scratch.data();
}

void DynamicGeometry::Unlock(u32 Count)
{
	VERIFY(Locked != 0 && Count <= Locked);

	if (Format != EVertexFormat::Packed && Count)
	{
		PackVertices(Format, &Records[Start], Scratch.data(), Count);
	}

	Position = Start + Count;
	Locked = 0;
}

void DynamicGeometry::Draw(GfxCmdList& Cmd, ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 StartVertex, u32 ReadCount)
{
	Push(Cmd, Topology, VertexCount, 1, StartVertex, ReadCount ? ReadCount : VertexCount, DRAW_SOURCE_RING);
}

void DynamicGeometry::DrawInstanced(GfxCmdList& Cmd, ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 InstanceCount, u32 StartVertex)
{
	Push(Cmd, Topology, VertexCount, InstanceCount, StartVertex, VertexCount, DRAW_SOURCE_RING);
}

void DynamicGeometry::DrawQuads(GfxCmdList& Cmd, u32 Quads, u32 BaseVertex)
{
	Push(Cmd, ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 6 * Quads, 1, BaseVertex, 4 * Quads, DRAW_SOURCE_RING | DRAW_QUADS);
}

void DynamicGeometry::DrawFan(GfxCmdList& Cmd, u32 Triangles, u32 BaseVertex)
{
	u32 First = 0;
	PackedVertex* Fan = static_cast<PackedVertex*>(Lock(Triangles * 3, EVertexFormat::Packed, First));

	for (u32 Triangle = 0; Triangle < Triangles; ++Triangle)
	{
		Fan[Triangle * 3] = Records[BaseVertex];
		Fan[Triangle * 3 + 1] = Records[BaseVertex + Triangle + 1];
		Fan[Triangle * 3 + 2] = Records[BaseVertex + Triangle + 2];
	}

	Unlock(Triangles * 3);
	Draw(Cmd, ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, Triangles * 3, First);
}

void DynamicGeometry::BeginRecord()
{
	VERIFY(!IsRecording);
	IsRecording = true;
	Recorded = {};
}

DynamicBatches DynamicGeometry::EndRecord()
{
	VERIFY(IsRecording);
	IsRecording = false;
	return Recorded;
}

void DynamicGeometry::Submit(GfxCmdList& Cmd, DynamicBatches Batches)
{
	NriPass& Passes = RImplementation.NriPasses;

	Passes.SubmitDynamic(Cmd, Batches.First, Batches.Count);

	if (Batches.Count && GRHI->APILevel == ERHI_API_LAYER::D3D12)
	{
		Upload();
	}
}

void DynamicGeometry::Push(GfxCmdList& Cmd, ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 InstanceCount, u32 First, u32 Count, u32 Flags)
{
	if (!VertexCount || !InstanceCount)
	{
		return;
	}

	VERIFY(First + Count <= Position);
	NriPass& Passes = RImplementation.NriPasses;
	const SVS* Vertex = Cmd.GetVS();

	// Errors of the producer: the old backend drew none of these either, but one larger than a chunk
	const ENriDecline Reason = Vertex && Vertex->Input == EVertexInput::Legacy ? ENriDecline::Input
		: Topology == ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN ? ENriDecline::Topology
		: First / ChunkRecords != (First + Count - 1) / ChunkRecords ? ENriDecline::Size : ENriDecline::None;

	if (Reason != ENriDecline::None)
	{
		Passes.Drop(Cmd, Reason);
		return;
	}

	CBDraw& Base = Cmd.Constants.Write<CBDraw>();
	Base.draw_vertex_base = First % ChunkRecords;
	Base.draw_source_base = 0;
	Base.draw_flags = Flags;
	const NriDynamicDraw Item = { Chunks[Slot][First / ChunkRecords].View, Topology, VertexCount, InstanceCount };

	if (IsRecording)
	{
		const u32 Batch = Passes.RecordDynamic(Cmd, Item);
		Recorded.First = Recorded.Count ? Recorded.First : Batch;
		Recorded.Count += Batch != ~0u ? 1u : 0u;
		VERIFY(Batch == ~0u || Batch + 1 == Recorded.First + Recorded.Count);
		return;
	}

	// D3D11 uploads when the run is replayed
	if (Passes.DrawDynamic(Cmd, Item) && GRHI->APILevel == ERHI_API_LAYER::D3D12)
	{
		Upload();
	}
}

void DynamicGeometry::Upload()
{
	const nri::CoreInterface& Api = GRHI->Nri->CoreApi();

	while (Uploaded < Position)
	{
		const u32 Index = Uploaded / ChunkRecords;
		const u32 End = std::min(Position, (Index + 1) * ChunkRecords);
		const u64 Bytes = u64(End - Uploaded) * sizeof(PackedVertex);
		nri::Buffer& Buffer = *Chunks[Slot][Index].Buffer;
		std::memcpy(Api.MapBuffer(Buffer, u64(Uploaded % ChunkRecords) * sizeof(PackedVertex), Bytes), &Records[Uploaded], Bytes);
		Api.UnmapBuffer(Buffer);
		Uploaded = End;
	}
}

void DynamicGeometry::EndFrame()
{
	VERIFY(!IsRecording);
	Position = 0;
	Uploaded = 0;
	Slot = (Slot + 1) % FrameCount;
}
