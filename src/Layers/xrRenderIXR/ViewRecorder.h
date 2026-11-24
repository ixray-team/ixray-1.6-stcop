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
struct CommandBuffer;
} // namespace nri

// Views recorded on the workers into command buffers of their own (D3D12 only), taken from the NRI device's ring. They
// run after everything the primary thread recorded before them and before what it records next, carried by the frame's
// one submit: no submit of their own
namespace ViewRecorder
{
constexpr u32 MaxViews = 4;

bool IsParallel();
nri::CommandBuffer& Begin(const char* Name);
void End(nri::CommandBuffer& Commands);
void Chain(nri::CommandBuffer* const* Buffers, u32 Count);

// On the primary thread: Body(Commands, Item) records view Item of Count on a worker, through NRI alone, and touches
// nothing the primary thread or another view owns
template <class Body>
void Record(u32 Count, const char* Name, Body&& Recording)
{
	if (!Count)
	{
		return;
	}

	R_ASSERT(Count <= MaxViews);
	nri::CommandBuffer* Buffers[MaxViews] = {};

	xr_parallel_for(u32(0), Count, [&](u32 Item)
	{
		Buffers[Item] = &Begin(Name);
		Recording(*Buffers[Item], Item);
		End(*Buffers[Item]);
	});

	Chain(Buffers, Count);
}
} // namespace ViewRecorder
