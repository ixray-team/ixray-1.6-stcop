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
#include "LampSelection.h"
#include "GpuSceneInternal.h"

void LampSelection::Create()
{
	Seen = CreateBuffer(Bytes, sizeof(u32), nri::BufferUsageBits::SHADER_RESOURCE_STORAGE | nri::BufferUsageBits::SHADER_RESOURCE, "Lamps seen");
	SeenView = MakeView(Seen, nri::BufferView::STORAGE_STRUCTURED_BUFFER, nri::Format::UNKNOWN, sizeof(u32));
	nri::BufferDesc CopyDesc = {};
	CopyDesc.size = Bytes;

	for (nri::Buffer*& Copy : Copies)
	{
		Api.CreateCommittedBuffer(*GRHI->Nri->Device, nri::MemoryLocation::HOST_READBACK, 0.0f, CopyDesc, Copy);
		Api.SetDebugName(Copy, "Lamps seen copy");
		std::memset(Api.MapBuffer(*Copy, 0, Bytes), 0, Bytes);
		Api.UnmapBuffer(*Copy);
	}

	Shader = DEV->_CreateCS("common\\scene_lamps_seen");
	IsNew = true;
}

void LampSelection::Destroy()
{
	if (!Seen)
	{
		return;
	}

	// The view is the scene's, as every view MakeView makes: its unload has destroyed it
	Api.DestroyBuffer(Seen);

	for (nri::Buffer*& Copy : Copies)
	{
		Api.DestroyBuffer(Copy);
		Copy = nullptr;
	}

	for (xr_vector<const light*>& List : Lists)
	{
		List.clear();
	}

	Seen = nullptr;
	SeenView = nullptr;
	Shader = nullptr;
	Hidden.clear();
}

void LampSelection::Mark(GfxCmdList& Cmd, const xr_vector<light*>& Lamps)
{
	const u32 Slot = RFrame.dwFrame % Frames;
	Lists[Slot].clear();
	Stamps[Slot] = RFrame.dwFrame;
	Fmatrix View;
	Fvector4 Records[4 + Most];

	if (Lamps.empty() || !HizOfFrameBefore(View, Records[0], Records[1], Records[2]))
	{
		return;
	}

	if (!Seen)
	{
		Create();
	}

	const u32 Count = std::min(u32(Lamps.size()), Most);
	Records[3].set(float(Count), 0.0f, 0.0f, 0.0f);
	std::memcpy(&Records[3].y, &Stamps[Slot], sizeof(u32));

	for (u32 Index = 0; Index < Count; ++Index)
	{
		Fvector Place;
		View.transform_tiny(Place, Lamps[Index]->position);
		Records[4 + Index].set(Place.x, Place.y, Place.z, Lamps[Index]->range);
		Lists[Slot].push_back(Lamps[Index]);
	}

	GPU_EVENT(LampSelection);
	NriPass& Passes = RImplementation.NriPasses;
	const NriBinding Reads[2] = { { 0, nullptr, 0, Passes.StreamRecords(Records, (4 + Count) * sizeof(Fvector4), sizeof(Fvector4)) }, { 1, nullptr, 0, HizRead } };
	const NriBinding Written = { 0, nullptr, 0, SeenView, Seen, IsNew };
	Passes.Dispatch(Cmd, Shader._get(), Reads, 2, &Written, 1, 1, 1, 1);
	IsNew = false;

	// The pass hands the buffer over read by shaders, and takes it back so
	nri::CommandBuffer& Commands = *BeginNriCommands(nullptr, 0, nullptr, 0, nullptr);
	Tracked Item = { Seen, { nri::AccessBits::SHADER_RESOURCE, nri::StageBits::ALL } };
	Move(Commands, Item, nri::AccessBits::COPY_SOURCE, nri::StageBits::COPY);
	Api.CmdCopyBuffer(Commands, *Copies[Slot], 0, *Seen, 0, Bytes);
	Move(Commands, Item, nri::AccessBits::SHADER_RESOURCE, nri::StageBits::ALL);
	EndNriCommands(&Commands);
}

void LampSelection::Read(const xr_vector<light*>& Lamps)
{
	Hidden.assign(Lamps.size(), false);
	// The copy of the oldest frame in the ring: a map of one the device has not reached makes the frame wait for it
	const u32 Slot = (RFrame.dwFrame + 1) % Frames;

	if (!Seen || Lists[Slot].empty() || Stamps[Slot] + Frames - 1 != RFrame.dwFrame || RFrame.IsCameraCut())
	{
		return;
	}

	u32 Words[Most + 1];
	std::memcpy(Words, Api.MapBuffer(*Copies[Slot], 0, Bytes), Bytes);
	Api.UnmapBuffer(*Copies[Slot]);

	// The copy is that frame's only when it carries its stamp: until then the slot holds an older answer
	if (Words[Most] != Stamps[Slot])
	{
		return;
	}

	for (u32 Index = 0; Index < Lamps.size(); ++Index)
	{
		const auto Found = std::find(Lists[Slot].begin(), Lists[Slot].end(), Lamps[Index]);
		Hidden[Index] = Found != Lists[Slot].end() && !Words[Found - Lists[Slot].begin()];
	}
}
