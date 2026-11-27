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
#include "RenderThread.h"
#include "SkinningPass.h"
#include "SkeletonX.h"
#include "SkeletonCustom.h"
#include "PackedVertex.h"
#include "dxRenderDeviceRender.h"

// The engine defines Device as a macro; NRI and NriDevice use it as a name
#undef Device
#include "../../xrRHI/Nri/NriDevice.h"

#include <NRI.h>

namespace
{
	constexpr u32 GroupSize = 64;
	constexpr u32 MaxGroups = 65535;
	constexpr u64 MaxElements = 1ull << 26;
	constexpr u32 OutputSlot = ShaderBindings::Texture("g_SkinnedVertices");
	constexpr u32 DataSlot = ShaderBindings::Texture("g_SkinData");
	constexpr u32 PageSlot = ShaderBindings::Texture("g_Vertices0");

	u32 Fit(u32 Capacity, u64 Need)
	{
		if (Need > MaxElements)
		{
			FATAL(make_string<const char*>("Skinning: %llu elements exceed the buffer limit", Need));
		}
		while (Capacity < Need)
		{
			Capacity *= 2;
		}
		return u32(std::min<u64>(Capacity, MaxElements));
	}

	void WriteRows(Fvector4* Rows, const Fmatrix& M)
	{
		Rows[0].set(M.i.x, M.j.x, M.k.x, M.c.x);
		Rows[1].set(M.i.y, M.j.y, M.k.y, M.c.y);
		Rows[2].set(M.i.z, M.j.z, M.k.z, M.c.z);
	}
}

// An update callback is the game's (foot IK, attachments, scripted actions): it runs inside the skeleton's lock and
// ray-queries the skeletons around it, which takes their locks, so two workers running two of them wait for each other
// for good. The bones are calculated on the workers without it, and the callbacks run here afterwards, in the list's
// order. The tracks of such a skeleton advance here first: their animation callbacks are the game's too
void PoseSkeletons(xr_vector<CKinematics*>& Skeletons)
{
	std::erase_if(Skeletons, [](const CKinematics* Skeleton) { return Skeleton->IsPosed() || Skeleton->IsHeld(); });

	if (Skeletons.empty())
	{
		return;
	}

	for (CKinematics* Skeleton : Skeletons)
	{
		if (Skeleton->GetUpdateCallback())
		{
			Skeleton->PrepareBones();
		}
	}

	xr_parallel_for(size_t(0), Skeletons.size(), [&Skeletons](size_t Index)
	{
		Skeletons[Index]->PoseBones();
	});

	for (CKinematics* Skeleton : Skeletons)
	{
		Skeleton->FinishBones();
	}
}

void SkinningPass::Destroy()
{
	Release();
	Shader = nullptr;
	Pending.clear();
	Cursor = 0;
}

void SkinningPass::Release()
{
	_RELEASE(AdoptedView);
	_RELEASE(Adopted);
	GRHI->Nri->Retire(OutputWrite);
	GRHI->Nri->Retire(OutputRead);
	GRHI->Nri->Retire(Output);
	OutputWrite = nullptr;
	OutputRead = nullptr;
	Output = nullptr;
}

void SkinningPass::Grow()
{
	const nri::CoreInterface& Api = GRHI->Nri->CoreApi();
	nri::BufferDesc Desc = {};
	Desc.size = u64(OutputCapacity) * sizeof(PackedVertex);
	Desc.structureStride = sizeof(PackedVertex);
	Desc.usage = nri::BufferUsageBits::SHADER_RESOURCE | nri::BufferUsageBits::SHADER_RESOURCE_STORAGE;

	if (Api.CreateCommittedBuffer(*GRHI->Nri->Device, nri::MemoryLocation::DEVICE, 0.0f, Desc, Output) != nri::Result::SUCCESS)
	{
		FATAL(make_string<const char*>("Skinning: cannot create a buffer of %u vertices", OutputCapacity));
	}

	Api.SetDebugName(Output, "Skinned vertices");
	nri::BufferViewDesc View = {};
	View.buffer = Output;
	View.type = nri::BufferView::STRUCTURED_BUFFER;
	View.structureStride = sizeof(PackedVertex);
	Api.CreateBufferView(View, OutputRead);
	View.type = nri::BufferView::STORAGE_STRUCTURED_BUFFER;
	Api.CreateBufferView(View, OutputWrite);

	RHIBufferDesc Wrapped = {};
	Wrapped.Size = u32(Desc.size);
	Wrapped.Usage = ERHI_USAGE::USAGE_DEFAULT;
	Wrapped.Type = ERHI_BUFFER_TYPE::STRUCTURED;
	Wrapped.StructureByteStride = sizeof(PackedVertex);
	Wrapped.Adopt = reinterpret_cast<void*>(Api.GetBufferNativeObject(Output));
	Adopted = GRHI->CreateBuffer(Wrapped);
	RHIShaderResourceViewDesc ViewDesc = {};
	ViewDesc.ElementWidth = OutputCapacity;
	AdoptedView = Adopted ? GRHI->CreateShaderResourceView(Adopted, &ViewDesc) : nullptr;

	if (!OutputRead || !OutputWrite || !AdoptedView)
	{
		FATAL("Skinning: cannot create the views of the output");
	}

	IsNew = true;
}

void SkinningPass::BeginFrame()
{
	++Frame;
	Cursor = 0;
	Pending.clear();
}

void SkinningPass::Bind(GfxCmdList& Cmd)
{
	if (AdoptedView)
	{
		Cmd.SetResource(ERHI_SHADER_TYPE::VS, OutputSlot, AdoptedView, OutputRead);
	}
}

void SkinningPass::Request(CSkeletonX& Mesh, const Fmatrix& World)
{
	VERIFY(!g_AppInfo.MainThread || g_AppInfo.IsRenderThread());
	if (Mesh.SkinFrame != Frame)
	{
		Mesh.SkinFrame = Frame;
		Pending.push_back({ &Mesh, World });
	}
}

// A held skeleton's last pose is stored again: its bones stay as they were, so the skinned vertices made of them now are
// the last pose's where the object stands now
void SkinningPass::Snapshot(CKinematics& Kinematics, const Fmatrix& World)
{
	if (!Kinematics.IsHeld())
	{
		Kinematics.CalculateBones(true);
	}

	Kinematics.StoreVisualMatrix(World);
}

void SkinningPass::Mark(CKinematics* Kinematics, const Fmatrix& World)
{
	if (Kinematics && RenderThread::IsPipelined())
	{
		Marked.emplace_back(Kinematics, World);
	}
}

// The old pose is the last capture's: the bones' own history steps by the device's frame counter, which the game's
// next frame advances while this one is drawn
void SkinningPass::Capture()
{
	std::swap(Poses, LastPoses);
	std::swap(PoseBones, LastBones);
	Poses.clear();
	PoseBones.clear();
	Parents.clear();

	for (const auto& [Kinematics, World] : Marked)
	{
		Parents.push_back(Kinematics);
	}

	std::sort(Parents.begin(), Parents.end());
	Parents.erase(std::unique(Parents.begin(), Parents.end()), Parents.end());
	PoseSkeletons(Parents);

	for (const auto& [Kinematics, World] : Marked)
	{
		if (Poses.contains(Kinematics))
		{
			continue;
		}

		Snapshot(*Kinematics, World);
		const u16 Count = Kinematics->LL_BoneCount();
		const auto Last = LastPoses.find(Kinematics);
		const bool IsLast = Last != LastPoses.end() && Last->second.Count == Count;
		Poses.emplace(Kinematics, Taken{ u32(PoseBones.size()), IsLast ? Last->second.World : World, World, Count });

		for (u16 Bone = 0; Bone < Count; ++Bone)
		{
			const Fmatrix& Now = Kinematics->LL_GetBoneInstance(Bone).mRenderTransform;
			PoseBones.push_back(Now);
			PoseBones.push_back(IsLast ? LastBones[Last->second.First + Bone * 2] : Now);
		}
	}

	Marked.clear();
}

bool SkinningPass::Find(const CKinematics& Kinematics, Pose& Out) const
{
	const auto Found = Poses.find(&Kinematics);

	if (Found == Poses.end())
	{
		return false;
	}

	Out = { PoseBones.data() + Found->second.First, Found->second.OldWorld };
	return true;
}

const Fmatrix& SkinningPass::OldWorld(const CKinematics& Kinematics) const
{
	const auto Found = Poses.find(&Kinematics);
	return Found != Poses.end() ? Found->second.OldWorld : Kinematics.mOldWorldMartrix;
}

void SkinningPass::Rigid(CKinematics& Kinematics, const Fmatrix& World, u16 Bone, Fmatrix& Now, Fmatrix& Old)
{
	Pose Taken;

	if (Find(Kinematics, Taken))
	{
		Now.mul_43(World, Taken.Bones[Bone * 2]);
		Old.mul_43(Taken.OldWorld, Taken.Bones[Bone * 2 + 1]);
		return;
	}

	Snapshot(Kinematics, World);
	const CBoneInstance& Instance = Kinematics.LL_GetBoneInstance(Bone);
	Now.mul_43(World, Instance.mRenderTransform_tmp);
	Old.mul_43(Kinematics.mOldWorldMartrix, Instance.mRenderTransform_old);
}

void SkinningPass::Flush(GfxCmdList& Cmd)
{
	if (Pending.empty())
	{
		return;
	}
	GPU_EVENT(skinning);
	GeometryHeap& Heap = *Cmd.Heap;
	if (Heap.HasPending())
	{
		Heap.Flush();
		Heap.Bind(Cmd);
	}

	// The skeletons of what waits are posed all at once: the loop below then finds them posed
	Parents.clear();

	for (const Item& Entry : Pending)
	{
		if ((Parents.empty() || Parents.back() != Entry.Mesh->Parent) && !Poses.contains(Entry.Mesh->Parent))
		{
			Parents.push_back(Entry.Mesh->Parent);
		}
	}

	PoseSkeletons(Parents);

	u64 Vertices = 0;
	u64 Elements = 1;
	u32 Jobs = 0;
	CKinematics* Last = nullptr;
	for (const Item& Entry : Pending)
	{
		CSkeletonX& Mesh = *Entry.Mesh;
		if (Mesh.Parent != Last)
		{
			Last = Mesh.Parent;

			if (!Poses.contains(Last))
			{
				Snapshot(*Last, Entry.World);
			}
		}
		Vertices += Mesh.vCount;
		Jobs += (Mesh.vCount + GroupSize - 1) / GroupSize;
		Elements += 6ull * Mesh.BonesUsed.size();
	}
	Elements += Jobs;

	if (!Output || Cursor + Vertices > OutputCapacity)
	{
		if (Output)
		{
			Msg("! Skinning output too small (%u used, %llu more vertices), growing to %u", Cursor, Vertices, OutputCapacity * 2);
			OutputCapacity *= 2;
			Cursor = 0;
			++Frame;
			for (const Item& Entry : Pending)
			{
				Entry.Mesh->SkinFrame = Frame;
			}
		}
		OutputCapacity = Fit(OutputCapacity, Vertices);
		Release();
		Grow();
	}

	if (Elements > MaxElements)
	{
		FATAL(make_string<const char*>("Skinning: %llu elements exceed the buffer limit", Elements));
	}

	if (!Shader)
	{
		Shader = DEV->_CreateCS("skin");
	}

	Records.resize(size_t(Elements) * 4);
	u32* Words = Records.data();
	Fvector4* Rows = reinterpret_cast<Fvector4*>(Words);
	Words[0] = Jobs;
	Words[1] = Words[2] = Words[3] = 0;
	u32* Job = Words + 4;
	u32 BoneBase = 1 + Jobs;
	for (const Item& Entry : Pending)
	{
		CSkeletonX& Mesh = *Entry.Mesh;
		Mesh.SkinOutBase = Cursor;
		for (u32 First = 0; First < Mesh.vCount; First += GroupSize, Job += 4)
		{
			Job[0] = Mesh.vBase + First;
			Job[1] = Cursor + First;
			Job[2] = BoneBase;
			Job[3] = std::min(Mesh.vCount - First, GroupSize) | (Mesh.Alloc.Page << 8);
		}
		Pose Taken;
		const bool IsTaken = Find(*Mesh.Parent, Taken);

		for (u32 Slot = 0; Slot < Mesh.BonesUsed.size(); ++Slot, BoneBase += 6)
		{
			const u16 Id = Mesh.BonesUsed[Slot];
			const CBoneInstance& Bone = Mesh.Parent->LL_GetBoneInstance(Id);
			WriteRows(Rows + BoneBase, IsTaken ? Taken.Bones[Id * 2] : Bone.mRenderTransform_tmp);
			WriteRows(Rows + BoneBase + 3, IsTaken ? Taken.Bones[Id * 2 + 1] : Bone.mRenderTransform_old);
		}
		Cursor += Mesh.vCount;
	}

	NriPass& Passes = RImplementation.NriPasses;
	NriBinding Reads[1 + GeometryHeap::MaxPages] = { { DataSlot, nullptr, 0, Passes.StreamRecords(Words, u32(Records.size() * sizeof(u32)), sizeof(Fvector4)) } };
	u32 ReadCount = 1;

	for (u32 Page = 0; Page < Heap.PageCount(); ++Page)
	{
		if (IRHIBuffer* Source = Heap.VertexBuffer(Page))
		{
			Reads[ReadCount++] = { PageSlot - Page, nullptr, 0, Passes.WrapRead(Source, sizeof(PackedVertex)) };
		}
	}

	// D3D11 drops a view of the output from the vertex stage behind the old backend's cache once the pass writes it
	const NriBinding Written = { 0, nullptr, 0, OutputWrite, Output, std::exchange(IsNew, false) };
	Cmd.SetResource(ERHI_SHADER_TYPE::VS, OutputSlot, nullptr);
	GRHI->ShaderResourceCache->Apply();
	Passes.Dispatch(Cmd, Shader._get(), Reads, ReadCount, &Written, 1, std::min(Jobs, MaxGroups), (Jobs + MaxGroups - 1) / MaxGroups, 1);
	Bind(Cmd);
	Pending.clear();
}
