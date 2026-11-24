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

namespace nri
{
struct Texture;
struct CommandBuffer;
struct Streamer;
} // namespace nri

class GpuResource;

// A texture bound for the first time in a frame, loaded without stopping it: a worker reads the file, the frame's
// streaming step makes the texture and fills it through NRI, and the next bind takes it. For plain block-compressed 2D
// files, which is what a level's textures are; any other is loaded where it is bound, as before. Read inside the draw,
// a large texture stopped the frame for tens of milliseconds
namespace TextureDecode
{
struct Decoded
{
	xr_vector<xr_vector<u8>> Levels; // the blocks of every mip
	u32 Format = 0; // DXGI
	u32 Width = 0;
	u32 Height = 0;
	u32 Mips = 0;
	u32 BlockBytes = 0;
	u32 Bytes = 0;
	nri::Texture* Texture = nullptr; // made and filled by Upload
};

// Any thread. False where the file is not such a texture
bool Read(const char* Name, Decoded& Out);
// Any thread: hands a read texture to the frame, which sets Done when it is made
void Queue(Decoded& Item, xr_atomic_bool& Done);
// Whether a frame takes what is queued: outside a level's frames nothing does
bool IsUploading();
// The rendering thread, once a frame, in a pass that records copies: makes one queued texture
void Upload(nri::CommandBuffer& Commands, nri::Streamer& Streamer);
// The rendering thread: the made texture, held by Out from now on
void Create(Decoded& In, GpuResource& Out, u32& OutSize);
void Drop(Decoded& In);
} // namespace TextureDecode
