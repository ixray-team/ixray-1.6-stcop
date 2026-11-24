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
#include "RenderAPI.h"
#include "R_DStreams.h"
#include "PackedVertex.h"

namespace nri
{
struct Buffer;
struct Descriptor;
} // namespace nri

class GfxCmdList;

// Batches recorded one after another in a frame, by their numbers
struct DynamicBatches
{
	u32 First = 0;
	u32 Count = 0;
};

// The frame's dynamic geometry as the 32-byte records the vertex shaders pull at g_RingVertices: written on the CPU,
// put into the frame slot's chunks of upload memory once a draw takes them, and drawn through NRI. A draw it cannot
// take is told once and not drawn. An offset is a record's place in the frame, which tells its chunk and its place in
// the chunk
class RENDER_API DynamicGeometry
{
public:
	static constexpr u32 ChunkRecords = 131072;

	void Create();
	void Destroy();
	void* Lock(u32 Count, EVertexFormat Format, u32& Offset);
	void Unlock(u32 Count);
	// ReadCount is how many records it reads from StartVertex on where that is not its vertex count
	void Draw(GfxCmdList& Cmd, ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 StartVertex, u32 ReadCount = 0);
	// The instance's number reaches the vertex shader as SV_InstanceID and DRAWID
	void DrawInstanced(GfxCmdList& Cmd, ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 InstanceCount, u32 StartVertex);
	void DrawQuads(GfxCmdList& Cmd, u32 Quads, u32 BaseVertex);
	// A fan of records already written, as a list of its triangles
	void DrawFan(GfxCmdList& Cmd, u32 Triangles, u32 BaseVertex);
	u32 GetCapacity() const { return ChunkRecords; }
	// The draws made from here to EndRecord are not drawn: each is resolved against what is set on its command list and
	// bound as targets at the time, and kept as a batch for the frame
	void BeginRecord();
	DynamicBatches EndRecord();
	// Batches recorded in this frame, drawn now with what they were recorded with: in any order with other draws
	void Submit(GfxCmdList& Cmd, DynamicBatches Batches);
	// What was written since the last upload goes into the chunks: before the GPU runs a draw of it
	void Upload();
	// After the frame's last draw: the next frame writes into the next slot's chunks
	void EndFrame();

private:
	static constexpr u32 FrameCount = 4;

	struct Chunk
	{
		nri::Buffer* Buffer;
		nri::Descriptor* View;
	};

	void Push(GfxCmdList& Cmd, ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 InstanceCount, u32 First, u32 Count, u32 Flags);

	xr_vector<Chunk> Chunks[FrameCount];
	xr_vector<PackedVertex> Records;
	xr_vector<u8> Scratch;
	u32 Slot = 0;
	u32 Position = 0;
	u32 Uploaded = 0;
	u32 Start = 0;
	u32 Locked = 0;
	EVertexFormat Format = EVertexFormat::Packed;
	bool IsRecording = false;
	DynamicBatches Recorded;
};
